cat("loading libs\n")
library(terra)
library(sf)
library(tidyverse)
library(rvest)
library(furrr)


set_parallel <- c("none","furrr_simple","furrr_complex")[2]
cat("sourcing DL function\n")
source("R/download_chirps_gefs.R")

cat("loading gpkg data\n")
gpkg_dir <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public",
  "raw",
  "som",
  "hydrobasin.gpkg"
)

out_gefs_dir <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public",
  "raw",
  "som",
  "chirps_gefs",
  "16d_daily"
)

gdf_bas3 <- st_read(
  gpkg_dir,
  "hybas3_af_lev03_v1c_shabelle_juba"
)



if(set_parallel=="none"){
  cat("beginning DL process\n")
  gefs_downloaded <- download_chirps_gefs(
    year = c(2002),
    leadtime = c(1:10),
    mask = gdf_bas3,
    out_path = out_gefs_dir
  )
}


if(set_parallel=="furrr_complex"){
  plan(multisession, workers = 7)
  cat("beginning DL process with FURRR\n")
  gefs_downloaded_parallel <- download_chirps_gefs_parallel(
    year = c(2003),
    leadtime = c(1:10),
    mask = gdf_bas3,
    out_path = out_gefs_dir
  )
}
if(set_parallel=="furrr_simple"){
  plan(multisession, workers = 7)
  cat("beginning DL process with FURRR\n")
  gefs_downloaded_parallel <- download_chirps_gefs_parallel2(
    year = c(2003),
    leadtime = c(1:10),
    mask = gdf_bas3,
    out_path = out_gefs_dir
  )
}
