cat("loading libs\n")
library(terra)
library(sf)
library(tidyverse)
library(rvest)

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


cat("beginning DL process\n")
gefs_downloaded <- download_chirps_gefs(
  year = c(2000),
  leadtime = c(1:10),
  mask = gdf_bas3,
  out_path = out_gefs_dir
)
