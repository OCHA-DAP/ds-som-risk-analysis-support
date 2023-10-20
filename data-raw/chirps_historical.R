library(rgee)
library(tidyverse)
library(tidyrgee)
library(targets)
library(rhdx)
library(janitor)
library(sf)
ee_Initialize()

analysis_level <- c("adm0", "adm1", "adm2", "bas4")[4]

adm_analysis <- str_detect(analysis_level, "^adm")
if (adm_analysis) {
  som_adm0_levels <- c(
    "som_admbnda_adm0_ocha_20230308",
    "som_admbnda_adm1_ocha_20230308",
    "som_admbnda_adm2_ocha_20230308"
  )

  som_adm <- som_adm0_levels %>%
    map(
      ~ search_datasets("Somalia - Subnational Administrative Boundaries") %>%
        pluck(1) %>%
        get_resource(2) %>%
        read_resource(layer = .x) %>%
        clean_names() %>%
        select(matches("^adm\\d_"))
    ) %>%
    set_names(c("adm0", "adm1", "adm2"))

  gdf_zone <- som_adm[[analysis_level]]
}
if (!adm_analysis) {
  cat("reading bas4\n")
  gdb_basin <- file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public",
    "raw",
    "som",
    "hydrobasin.gpkg"
  )
  gdf_zone <- read_sf(gdb_basin, layer = "hybas3_af_lev03_v1c_shabelle_juba") %>%
    rename(
      geometry = "geom"
    )
}

out_dir <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public",
  "processed",
  "som",
  "chirps"
)


cat("reading and manipulate image collection\n")
ic <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")
tic <- as_tidyee(ic)

tic_monthly_rainfall <- tic %>%
  group_by(year, month) %>%
  summarise(
    stat = "sum"
  )



fc_zone <- sf_as_ee(x = gdf_zone)

cat("begin extraction of zonal stats for ", analysis_level, "\n")
df_rainfall_adm <- ee_extract_tidy(
  x = tic_monthly_rainfall,
  y = fc_zone,
  scale = 5566,
  stat = "mean",
  via = "drive"
)

prefix_date <- format(Sys.Date(), "%Y%m%d")
cat("write zonal sats to csv\n")
write_csv(
  x = df_rainfall_adm,
  file = file.path(
    out_dir,
    paste0(
      prefix_date, "_chirps_monthly_historical_", analysis_level, ".csv"
    )
  )
)
cat("finished")
