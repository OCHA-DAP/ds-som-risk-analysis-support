library(rvest)
library(janitor)
library(lubridate)
library(glue)

#' latest_gefs_metadata
#'
#' @param url \code{character} base url of CHIRP-GEFS 16 day forecast server (default = "https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/daily_16day/",)
#' @param leadtime leadtimes to download (default=1:10)
#'
#' @return tibble containing metadata of latest CHIRPS-GEFS files available and what there equivalent name should be on the gdrive

latest_gefs_metadata <- function(
    url = "https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/daily_16day/",
    leadtime = 1:10) {
  year_latest_url <- latest_gefs_url(url)
  month_latest_url <- latest_gefs_url(year_latest_url)
  day_latest_url <- latest_gefs_url(month_latest_url)

  dl_tbl <- gefs_url_table(day_latest_url) %>%
    slice(leadtime)


  # convert to file naming convention in gdrive
  date_chr_trimmed <- str_remove_all(dl_tbl$name, "data\\.|\\.tif")
  formatted_date <- str_replace_all(date_chr_trimmed, "(\\d{4})\\.(\\d{2})(\\d{2})", "\\1-\\2-\\3")
  fnames <- paste0(paste0("yem_aoi_chirps_gefs_", formatted_date)[1], ".", 1:length(formatted_date), ".tif")

  return(
    dl_tbl %>%
      mutate(
        fname_bare = paste0(formatted_date[1], ".", 1:length(formatted_date)),
        drive_fname = fnames,
        forecast_made = formatted_date[1]
      )
  )
}



#' gefs_url_table
#'
#' @param url
#'
#' @return
#' @export
#'
#' @examples
gefs_url_table <- function(url) {
  html_table_ob <- html_table(read_html(url))[[1]]
  html_table_ob %>%
    clean_names() %>%
    filter(last_modified != "") %>%
    mutate(
      last_modified = ymd_hm(last_modified),
      url = glue("{url}{name}")
    )
}
#' latest_gefs_url
#'
#' @param url \code{character}
#'
#' @return \code{character} latest url available on current page
#'
#' @examples
latest_gefs_url <- function(url) {
  url_table <- gefs_url_table(url)
  url_table %>%
    filter(last_modified == max(last_modified)) %>%
    pull(url)
}


download_chirps_gefs <- function(year = c(2004),
                                 leadtime = c(1:10),
                                 mask = gdf_bas3,
                                 out_path) {
  meta_base_table <- gefs_url_table(url = "https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/daily_16day/")
  yr_rgx <- glue_collapse(paste0("/", year, "/"), sep = "|")
  yr_url <- str_subset(meta_base_table$url, pattern = yr_rgx)

  files_downloaded <- list.files(out_path)

  yr_url %>%
    map(
      \(yr_url_tmp){
        meta_mo_tbl <- gefs_url_table(url = yr_url_tmp)
        meta_mo_tbl$url %>%
          map(
            \(mo_url_tmp){
              meta_day_tbl <- gefs_url_table(url = mo_url_tmp)
              meta_day_tbl$url %>%
                map(
                  \(day_url_tmp){
                    yyy_mm_dd <- str_extract(day_url_tmp, "\\d{4}/\\d{2}/\\d{2}") %>%
                      str_replace_all("/", "-")
                    tif_name <- paste0("chirps_gefs_16d_", yyy_mm_dd, ".tif")

                    if (tif_name %in% files_downloaded) {
                      cat("skipping ", tif_name, " already downloaded\n")
                    }

                    if (!tif_name %in% files_downloaded) {
                      meta_day_tbl <- gefs_url_table(url = day_url_tmp)

                      meta_day_tbl_filt <- meta_day_tbl %>%
                        slice(leadtime)

                      cat("downloading leadtimes \n")

                      r_tmp <- rast(meta_day_tbl_filt$url)

                      lts <- str_sub(names(r_tmp), start = -2, end = -1)

                      cat("cropping gefs to mask\n")
                      r_crop <- crop(r_tmp, mask)

                      names_clean <- paste0(yyy_mm_dd, "_lt", lts)

                      r_crop %>%
                        set.names(names_clean)

                      cat("writing ", yyy_mm_dd, " raster\n")

                      out_name <- file.path(out_path, tif_name)
                      r_crop %>%
                        writeRaster(
                          out_name,
                          gdal = c("cog")
                        )
                    }
                  }
                )
            }
          )
      }
    )
}
download_chirps_gefs_parallel2 <- function(year = c(2004),
                                 leadtime = c(1:10),
                                 mask = gdf_bas3,
                                 out_path) {
  meta_base_table <- gefs_url_table(url = "https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/daily_16day/")
  yr_rgx <- glue_collapse(paste0("/", year, "/"), sep = "|")
  yr_url <- str_subset(meta_base_table$url, pattern = yr_rgx)

  files_downloaded <- list.files(out_path)

  yr_url %>%
    future_map(
      \(yr_url_tmp){
        meta_mo_tbl <- gefs_url_table(url = yr_url_tmp)
        meta_mo_tbl$url %>%
          future_map(
            \(mo_url_tmp){
              meta_day_tbl <- gefs_url_table(url = mo_url_tmp)
              meta_day_tbl$url %>%
                future_map(
                  \(day_url_tmp){
                    yyy_mm_dd <- str_extract(day_url_tmp, "\\d{4}/\\d{2}/\\d{2}") %>%
                      str_replace_all("/", "-")
                    tif_name <- paste0("chirps_gefs_16d_", yyy_mm_dd, ".tif")

                    if (tif_name %in% files_downloaded) {
                      cat("skipping ", tif_name, " already downloaded\n")
                    }

                    if (!tif_name %in% files_downloaded) {
                      meta_day_tbl <- gefs_url_table(url = day_url_tmp)

                      meta_day_tbl_filt <- meta_day_tbl %>%
                        slice(leadtime)

                      cat("downloading leadtimes \n")

                      r_tmp <- rast(meta_day_tbl_filt$url)

                      lts <- str_sub(names(r_tmp), start = -2, end = -1)

                      cat("cropping gefs to mask\n")
                      r_crop <- crop(r_tmp, mask)

                      names_clean <- paste0(yyy_mm_dd, "_lt", lts)

                      r_crop %>%
                        set.names(names_clean)

                      cat("writing ", yyy_mm_dd, " raster\n")

                      out_name <- file.path(out_path, tif_name)
                      r_crop %>%
                        writeRaster(
                          out_name,
                          gdal = c("cog")
                        )
                    }
                  }
                )
            }
          )
      }
    )
}

download_chirps_gefs_parallel <- function(year = c(2004),
                                 leadtime = c(1:10),
                                 mask = gdf_bas3,
                                 out_path) {
  meta_base_table <- gefs_url_table(url = "https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/daily_16day/")
  yr_rgx <- glue_collapse(paste0("/", year, "/"), sep = "|")
  yr_url <- str_subset(meta_base_table$url, pattern = yr_rgx)

  files_downloaded <- list.files(out_path)

  yr_url %>%
    future_map(
      \(yr_url_tmp){
        meta_mo_tbl <- gefs_url_table(url = yr_url_tmp)
        meta_mo_tbl$url %>%
          future_map(
            \(mo_url_tmp){
              meta_day_tbl <- gefs_url_table(url = mo_url_tmp)
              meta_day_tbl$url %>%
                future_map(
                  \(day_url_tmp){
                    yyy_mm_dd <- as_date(str_extract(day_url_tmp, "\\d{4}/\\d{2}/\\d{2}") %>%
                      str_replace_all("/", "-"))
                    tif_name <- paste0("chirps_gefs_16d_", yyy_mm_dd, ".tif")

                    if (tif_name %in% files_downloaded) {
                      cat("skipping ", tif_name, " already downloaded\n")
                    }

                    if (!tif_name %in% files_downloaded) {
                      meta_day_tbl <- gefs_url_table(url = day_url_tmp)

                      meta_day_tbl_filt <- meta_day_tbl %>%
                        slice(leadtime)

                      meta_day_tbl_filt <- meta_day_tbl_filt %>%
                        mutate(
                          lt_date =  as.Date(str_extract(name, "\\d{4}.\\d{4}"), format = "%Y.%m%d"),
                          lt = as.numeric(lt_date - yyy_mm_dd)+1

                        )


                      cat("downloading leadtimes \n")
                      fp_tmp <- tempfile(fileext = ".tif")

                      lrtmp_lts<- meta_day_tbl_filt$url %>%
                        future_map2(
                          meta_day_tbl_filt$lt,

                          \(tmpurl,lt){

                            rname_new <-  paste0(yyy_mm_dd,"_lt_",lt)

                            cat("downloading ", rname_new,"\n")
                            download.file(destfile = fp_tmp,
                                           tmpurl,
                                           mode = "wb",
                                           method = "libcurl"
                                          )

                            rtmp <- rast(fp_tmp)
                            rtmp %>%
                              set.names(
                                rname_new
                              )
                            rtmp_crop <- crop(rtmp, mask)
                            return(rtmp_crop)
                          }
                          )
                      rtmp_lts<- rast(lrtmp_lts)
                      cat("writing ", yyy_mm_dd, " raster\n")

                      out_name <- file.path(out_path, tif_name)
                      rtmp_lts %>%
                        writeRaster(
                          out_name,
                          gdal = c("cog")
                        )
                    }
                  }
                )
            }
          )
      }
    )
}
