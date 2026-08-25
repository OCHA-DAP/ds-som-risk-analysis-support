# WorldPop from Azure blob -----------------------------------------------------
#
# The pipeline previously used a Somalia WorldPop 2020 raster from the shared
# drive (`som_ppp_2020_1km_Aggregated_UNadj.tif`). With FloodScan now running to
# the present, flood years through 2026 were being multiplied against a 2020
# population.
#
# The current global WorldPop count rasters live on blob:
#
#   dev / container `raster`
#     worldpop/pop_count/global_pop_<year>_CN_1km_R2025A_UA_v1.tif
#
# These are proper COGs (512x512 blocks, overviews) at the same 1km / 0.008333
# deg resolution as the old country raster, so the Somalia window can be read
# without pulling the ~320 MB global file.

WP_BLOB_ACCOUNT <- "imb0chd0dev"
WP_BLOB_CONTAINER <- "raster"
WP_BLOB_PREFIX <- "worldpop/pop_count"
WP_BLOB_FILE <- "global_pop_%s_CN_1km_R2025A_UA_v1.tif"

# years published under this release
WP_BLOB_YEARS <- c(2018, 2026)


#' Read access token for the dev blob account
#'
#' @return `character` SAS token
#' @export
wp_blob_sas <- function() {
  sas <- Sys.getenv("DSCI_AZ_BLOB_DEV_SAS")
  if (!nzchar(sas)) {
    stop(
      "DSCI_AZ_BLOB_DEV_SAS is not set. It is needed to read the WorldPop ",
      "rasters from the dev blob account.",
      call. = FALSE
    )
  }
  sas
}


#' Blob URL for a global WorldPop count raster
#'
#' @param year `numeric` one of `WP_BLOB_YEARS`
#' @param sas `character` SAS token
#'
#' @return `character` GDAL `/vsicurl/` path
#' @export
wp_blob_url <- function(year = 2026, sas = wp_blob_sas()) {
  if (!year %in% WP_BLOB_YEARS) {
    stop(
      "WorldPop year ", year, " is not on blob. Available: ",
      paste(WP_BLOB_YEARS, collapse = ", "),
      call. = FALSE
    )
  }
  sprintf(
    "/vsicurl/https://%s.blob.core.windows.net/%s/%s/%s?%s",
    WP_BLOB_ACCOUNT,
    WP_BLOB_CONTAINER,
    WP_BLOB_PREFIX,
    sprintf(WP_BLOB_FILE, year),
    sas
  )
}


#' Crop the global WorldPop raster to an area of interest
#'
#' Reads only the window covering `geometry` out of the global COG and writes it
#' to a local GeoTIFF, returning the path so it can be tracked by a
#' `format = "file"` target the same way the shared-drive raster was.
#'
#' The raster is also **masked** to `geometry`. The global product is not
#' clipped to any border, and Somalia's bounding box takes in large populated
#' areas of eastern Ethiopia and north-eastern Kenya -- a bbox crop alone more
#' than doubles the apparent national population. Masking reproduces the
#' semantics of the WorldPop country product the pipeline used before.
#'
#' The result is cached under `cache_dir` (gitignored), so this only hits blob
#' the first time.
#'
#' @param geometry `sf` object whose bbox defines the window and whose
#'     boundary is used as the mask
#' @param year `numeric` WorldPop year
#' @param cache_dir `character` where to keep the cropped raster
#' @param iso3 `character` used in the output filename
#' @param mask `logical` mask to `geometry` as well as cropping to its bbox
#' @param overwrite `logical` re-download even if the cache exists
#' @param sas `character` SAS token
#'
#' @return `character` path to the local GeoTIFF
#' @export
wp_blob_crop <- function(geometry,
                         year = 2026,
                         cache_dir = file.path("data", "worldpop"),
                         iso3 = "som",
                         mask = TRUE,
                         overwrite = FALSE,
                         sas = wp_blob_sas()) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  f_out <- file.path(
    cache_dir,
    sprintf("%s_pop_%s_1km_R2025A_UA.tif", iso3, year)
  )

  if (file.exists(f_out) && !overwrite) {
    return(f_out)
  }

  # keeps GDAL from probing for sidecar files on every open
  terra::setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")

  cat("cropping global WorldPop", year, "to", toupper(iso3), "from blob\n")

  r <- terra::rast(wp_blob_url(year = year, sas = sas))
  r_crop <- terra::crop(r, terra::ext(sf::st_bbox(geometry)))

  if (mask) {
    r_crop <- terra::mask(r_crop, terra::vect(sf::st_union(geometry)))
  }

  terra::writeRaster(
    r_crop, f_out,
    overwrite = TRUE,
    gdal = c("COMPRESS=DEFLATE", "TILED=YES")
  )

  f_out
}
