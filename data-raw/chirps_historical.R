library(rgee)
library(tidyverse)
library(tidyrgee)
library(targets)
library(rhdx)
library(janitor)
ee_Initialize()


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


cat("get adm0 boundaries from FAO\n")
# fc_adm0 = ee$FeatureCollection('FAO/GAUL/2015/level0')

fc_adm0 <- sf_as_ee(x = som_adm$adm0)

cat("begin extraction of zonal stats\n")
df_rainfall_adm0 <- ee_extract_tidy(
  x = tic_monthly_rainfall,
  y = fc_adm0,
  scale = 5566,
  stat = "mean",
  via = "drive"
)


cat("write zonal sats to csv\n")
write_csv(
  x = df_rainfall_adm0,
  file = file.path(
    out_dir,
    "20230926_chirps_monthly_historical_adm0.csv"
  )
)
cat("finished")
