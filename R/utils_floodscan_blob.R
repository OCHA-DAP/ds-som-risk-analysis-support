# FloodScan from Azure blob ---------------------------------------------------
#
# The original pipeline read FloodScan from a static NetCDF snapshot on the
# shared drive (AA_DATA_DIR), which ends 2022-12-31 -- that is why the seasonal
# flood analysis stopped at OND 2022. The daily FloodScan COGs maintained by
# `ds-floodscan-ingest` are kept current on blob, so we read those instead.
#
#   prod / container `raster`
#     floodscan/daily/v5/processed/aer_area_300s_v<YYYY-MM-DD>_v05r01.tif
#
# Each COG is the full FloodScan Africa grid (1080x1080 @ 0.08333 deg) with two
# bands: 1 = SFED, 2 = MFED. Same grid as the NetCDF, so the seasonal maxima
# produced here are drop-in replacements for the NetCDF-derived ones.

FS_BLOB_ACCOUNT <- "imb0chd0prod"
FS_BLOB_CONTAINER <- "raster"
FS_BLOB_PREFIX <- "floodscan/daily/v5/processed"
FS_BLOB_FILE <- "aer_area_300s_v%s_v05r01.tif"

# first date in the FloodScan record
FS_BLOB_START <- as.Date("1998-01-12")

# band order within the COG
FS_BLOB_BANDS <- c(SFED_AREA = 1L, MFED_AREA = 2L)


#' Read access token for the prod blob account
#'
#' Uses the same environment variable as `ocha_stratus`, so it is already set
#' wherever other CHD tooling runs.
#'
#' @return `character` SAS token
#' @export
fs_blob_sas <- function() {
  sas <- Sys.getenv("DSCI_AZ_BLOB_PROD_SAS")
  if (!nzchar(sas)) {
    stop(
      "DSCI_AZ_BLOB_PROD_SAS is not set. It is needed to read the FloodScan ",
      "COGs from the prod blob account.",
      call. = FALSE
    )
  }
  sas
}


#' Build the blob URL for one or more FloodScan dates
#'
#' @param dates `Date` vector
#' @param sas `character` SAS token
#'
#' @return `character` vector of https URLs
#' @export
fs_blob_url <- function(dates, sas = fs_blob_sas()) {
  sprintf(
    "https://%s.blob.core.windows.net/%s/%s/%s?%s",
    FS_BLOB_ACCOUNT,
    FS_BLOB_CONTAINER,
    FS_BLOB_PREFIX,
    sprintf(FS_BLOB_FILE, format(as.Date(dates), "%Y-%m-%d")),
    sas
  )
}


#' Latest FloodScan date available on blob
#'
#' Probes backwards from `from` rather than listing the container -- the
#' container holds >10k blobs and this only needs a couple of HEAD requests.
#' Intended to be used as an "always" target so the pipeline invalidates
#' whenever new FloodScan data lands.
#'
#' @param from `Date` to start probing back from (default today)
#' @param max_lookback `integer` how many days back to try before giving up
#' @param sas `character` SAS token
#'
#' @return `Date` of the most recent available COG
#' @export
fs_blob_latest_date <- function(from = Sys.Date(),
                                max_lookback = 30,
                                sas = fs_blob_sas()) {
  for (i in seq_len(max_lookback) - 1L) {
    d <- as.Date(from) - i
    h <- try(curl::curl_fetch_memory(
      fs_blob_url(d, sas = sas),
      handle = curl::new_handle(nobody = TRUE, failonerror = FALSE)
    ), silent = TRUE)
    if (!inherits(h, "try-error") && h$status_code == 200) {
      return(d)
    }
  }
  stop(
    "No FloodScan COG found on blob within ", max_lookback,
    " days of ", format(as.Date(from)), ".",
    call. = FALSE
  )
}


#' Dates belonging to the MAM and OND seasons
#'
#' Mirrors the season definition used by `floodscan_lookup()`: MAM is
#' March-May, OND is October-December.
#'
#' @param start `Date` first date of the record to consider
#' @param end `Date` last date available
#' @param seasons `character` seasons to include
#' @param complete_only `logical` drop seasons that are not fully covered by
#'     the available record. Keeps a half-finished season from being compared
#'     against complete ones.
#'
#' @return `tibble` with `date` and `fs_seas` (e.g. "OND_2023")
#' @export
fs_blob_season_dates <- function(start = FS_BLOB_START,
                                 end,
                                 seasons = c("MAM", "OND"),
                                 complete_only = TRUE) {
  all_dates <- seq(as.Date(start), as.Date(end), by = "day")

  df <- tibble(date = all_dates) %>%
    mutate(
      seas = case_when(
        month(date) %in% c(3, 4, 5) ~ "MAM",
        month(date) %in% c(10, 11, 12) ~ "OND",
        .default = "other"
      ),
      fs_seas = paste0(seas, "_", year(date))
    ) %>%
    filter(seas %in% seasons)

  if (complete_only) {
    # a season is complete when all of its calendar days are in the record
    expected <- df %>%
      distinct(seas, yr = year(date)) %>%
      mutate(
        n_expected = if_else(
          seas == "MAM",
          as.integer(as.Date(paste0(yr, "-06-01")) - as.Date(paste0(yr, "-03-01"))),
          as.integer(as.Date(paste0(yr + 1, "-01-01")) - as.Date(paste0(yr, "-10-01")))
        ),
        fs_seas = paste0(seas, "_", yr)
      )

    df <- df %>%
      group_by(fs_seas) %>%
      mutate(n_have = n()) %>%
      ungroup() %>%
      left_join(select(expected, fs_seas, n_expected), by = "fs_seas") %>%
      filter(n_have == n_expected) %>%
      select(-n_have, -n_expected)
  }

  df %>%
    select(date, fs_seas) %>%
    arrange(date)
}


#' Download FloodScan COGs from blob
#'
#' Concurrent download via `curl::multi_download()`. A share of requests get
#' dropped by the storage account under concurrency, so failures are retried
#' rather than silently skipped.
#'
#' Connections also stall outright rather than failing, so a wall-clock cap and
#' a stalled-transfer cap are both set. Without them a single dead connection
#' hangs the pipeline indefinitely.
#'
#' @param dates `Date` vector
#' @param dir `character` destination directory
#' @param sas `character` SAS token
#' @param max_tries `integer` attempts per date
#' @param batch_timeout `numeric` seconds before a batch is abandoned and its
#'     outstanding files retried
#' @param quiet `logical` suppress progress messages
#'
#' @return `tibble` with `date`, `path` and `success`
#' @export
fs_blob_download <- function(dates,
                             dir,
                             sas = fs_blob_sas(),
                             max_tries = 5,
                             batch_timeout = 180,
                             quiet = FALSE) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  dates <- as.Date(dates)
  paths <- file.path(dir, sprintf(FS_BLOB_FILE, format(dates, "%Y-%m-%d")))

  # a non-empty file on disk from an earlier run is good enough
  done <- file.exists(paths) & file.size(paths) > 0

  for (attempt in seq_len(max_tries)) {
    todo <- which(!done)
    if (!length(todo)) break

    if (!quiet) {
      cat(sprintf(
        "  floodscan download attempt %d: %d file(s)\n", attempt, length(todo)
      ))
    }

    res <- try(
      curl::multi_download(
        urls = fs_blob_url(dates[todo], sas = sas),
        destfiles = paths[todo],
        resume = FALSE,
        progress = FALSE,
        # abandon the whole batch rather than hang on one dead connection
        multi_timeout = batch_timeout,
        # per-transfer guards
        connecttimeout = 30,
        timeout = 120,
        low_speed_limit = 1000,
        low_speed_time = 20
      ),
      silent = TRUE
    )

    if (inherits(res, "try-error")) {
      ok <- rep(FALSE, length(todo))
    } else {
      ok <- res$success & !is.na(res$status_code) & res$status_code == 200
      ok[is.na(ok)] <- FALSE
    }
    done[todo] <- ok

    # drop the partial files so the retry starts clean
    unlink(paths[todo][!ok])
  }

  tibble(date = dates, path = paths, success = done)
}


#' Extent covering the cells whose centres fall inside a geometry's bbox
#'
#' Reproduces the cell selection `fs_filter_bounds()` did on the NetCDF
#' (`between(lat, ymin, ymax)` on coordinate values, i.e. cell centres) so the
#' blob-derived rasters line up with the NetCDF-derived ones rather than
#' picking up an extra row or column from `terra::crop()`'s intersect rule.
#'
#' @param r `SpatRaster` on the FloodScan grid
#' @param geometry `sf` object
#'
#' @return `SpatExtent`
#' @export
fs_bbox_extent <- function(r, geometry) {
  bb <- sf::st_bbox(geometry)
  half <- terra::res(r) / 2

  xs <- terra::xFromCol(r, seq_len(terra::ncol(r)))
  ys <- terra::yFromRow(r, seq_len(terra::nrow(r)))

  xs <- xs[xs >= bb[["xmin"]] & xs <= bb[["xmax"]]]
  ys <- ys[ys >= bb[["ymin"]] & ys <= bb[["ymax"]]]

  if (!length(xs) || !length(ys)) {
    stop("geometry does not overlap the FloodScan grid", call. = FALSE)
  }

  terra::ext(
    min(xs) - half[1], max(xs) + half[1],
    min(ys) - half[2], max(ys) + half[2]
  )
}


#' Seasonal maximum flood fraction from the FloodScan COGs on blob
#'
#' For each MAM/OND season, downloads that season's daily COGs, crops them to
#' the geometry and reduces to the per-pixel maximum. Only the seasonal maxima
#' are kept, so memory stays flat regardless of how many years are processed.
#'
#' Each seasonal maximum is cached as a small local GeoTIFF, so re-runs and
#' subsequent years only pay for what is new. `data/` is gitignored.
#'
#' @param geometry `sf` object used to crop (its bbox is used)
#' @param end_date `Date` last date to consider -- pass
#'     `fs_blob_latest_date()` so the pipeline tracks new data
#' @param start_date `Date` first date to consider
#' @param band `character` FloodScan band, "SFED_AREA" or "MFED_AREA"
#' @param seasons `character` seasons to include
#' @param complete_only `logical` skip part-finished seasons
#' @param cache_dir `character` where to keep the seasonal maxima
#' @param sas `character` SAS token
#' @param quiet `logical` suppress progress messages
#'
#' @return `SpatRaster`, one layer per season, named with the first date of the
#'     season -- matching what the NetCDF route produced
#' @export
fs_blob_seasonal_max <- function(geometry,
                                 end_date = fs_blob_latest_date(),
                                 start_date = FS_BLOB_START,
                                 band = "SFED_AREA",
                                 seasons = c("MAM", "OND"),
                                 complete_only = TRUE,
                                 cache_dir = file.path(
                                   "data", "floodscan_seasonal_max"
                                 ),
                                 sas = fs_blob_sas(),
                                 quiet = FALSE) {
  band_idx <- FS_BLOB_BANDS[[band]]

  df_seas <- fs_blob_season_dates(
    start = start_date,
    end = end_date,
    seasons = seasons,
    complete_only = complete_only
  )

  # cache key has to change when the crop does, otherwise a cached tif from a
  # different AOI gets reused silently
  bb <- sf::st_bbox(geometry)
  aoi_key <- substr(
    digest::digest(round(as.numeric(bb), 4), algo = "crc32"), 1, 8
  )
  cdir <- file.path(cache_dir, paste0(band, "_", aoi_key))
  dir.create(cdir, showWarnings = FALSE, recursive = TRUE)

  seas_list <- split(df_seas$date, df_seas$fs_seas)
  # chronological, not alphabetical
  seas_list <- seas_list[order(vapply(seas_list, min, as.Date(NA)))]

  tmp_root <- file.path(tempdir(), "floodscan_cogs")

  lr <- imap(seas_list, function(dates_tmp, seas_tmp) {
    f_cache <- file.path(cdir, paste0(seas_tmp, ".tif"))

    if (file.exists(f_cache)) {
      r <- terra::rast(f_cache)
      set.names(r, as.character(min(dates_tmp)))
      return(r)
    }

    if (!quiet) cat("floodscan season", seas_tmp, "\n")

    td <- file.path(tmp_root, seas_tmp)
    df_dl <- fs_blob_download(dates_tmp, dir = td, sas = sas, quiet = quiet)

    # A seasonal maximum built from a subset of the season is biased low, and
    # caching it would make that permanent. Fail loudly instead.
    if (any(!df_dl$success)) {
      missing <- df_dl$date[!df_dl$success]
      unlink(td, recursive = TRUE)
      stop(
        seas_tmp, ": ", length(missing), " of ", nrow(df_dl),
        " days could not be downloaded (", paste(format(missing), collapse = ", "),
        "). Re-run to retry -- completed seasons are cached.",
        call. = FALSE
      )
    }

    e <- fs_bbox_extent(terra::rast(df_dl$path[1]), geometry)

    r_max <- terra::rast(
      map(df_dl$path, \(f) terra::crop(terra::rast(f)[[band_idx]], e))
    ) %>%
      max()

    terra::writeRaster(
      r_max, f_cache,
      overwrite = TRUE,
      gdal = c("COMPRESS=DEFLATE")
    )
    unlink(td, recursive = TRUE)

    r <- terra::rast(f_cache)
    set.names(r, as.character(min(dates_tmp)))
    r
  })

  lr <- compact(lr)
  if (!length(lr)) stop("no FloodScan seasons could be built", call. = FALSE)

  terra::rast(unname(lr))
}
