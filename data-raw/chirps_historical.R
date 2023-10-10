library(rgee)
library(tidyverse)
library(tidyrgee)
library(targets)
library(rhdx)
library(janitor)
ee_Initialize()

admin_level <-  c("adm0","adm1","adm2")[3]

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

som_adm[[admin_level]]

fc_adm <- sf_as_ee(x = som_adm[[admin_level]])

cat("begin extraction of zonal stats for ", admin_level,"\n")
df_rainfall_adm <- ee_extract_tidy(
  x = tic_monthly_rainfall,
  y = fc_adm,
  scale = 5566,
  stat = "mean",
  via = "drive"
)

prefix_date <- format(Sys.Date() ,"%Y%m%d")
cat("write zonal sats to csv\n")
write_csv(
  x = df_rainfall_adm,
  file = file.path(
    out_dir,
    paste0(
      prefix_date,"_chirps_monthly_historical_",admin_level,".csv"
    )
  )
)
cat("finished")
