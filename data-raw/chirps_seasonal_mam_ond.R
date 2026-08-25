library(rgee)
library(tidyverse)
library(tidyrgee)
library(targets)
library(rhdx)
library(janitor)
ee_Initialize()

out_dir <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public",
  "processed",
  "som",
  "chirps"
)

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





cat("reading and manipulate image collection\n")
ic <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")
tic <- as_tidyee(ic)


tic_seasons <- tic %>%
  mutate(
    season = case_when(
      month %in% c(3, 4, 5) ~ "MAM",
      month %in% c(10, 11, 12) ~ "OND",
      .default = NA
    )
  ) %>%
  filter(
    !is.na(season)
  ) %>%
  group_by(year, season) %>%
  summarise(
    stat = "sum"
  )

fc_adm1 <- sf_as_ee(x = som_adm$adm1)

cat("Extract yearly MAM & OND for all historical chirps\n")
df_adm1_mam_ond <- ee_extract_tidy(
  x = tic_seasons,
  y = fc_adm1,
  scale = 5566,
  stat = "mean",
  via = "drive"
)


cat("write zonal sats to csv\n")
write_csv(
  x = df_adm1_mam_ond,
  file = file.path(
    out_dir,
    "20231005_chirps_adm1_MAM_OND.csv"
  )
)


cat("Extract all monthly rainfall as well to admin 1\n")
tic_monthly_rainfall <- tic %>%
  group_by(year, month) %>%
  summarise(
    stat = "sum"
  )

df_adm1_monthly <- ee_extract_tidy(
  x = tic_monthly_rainfall,
  y = fc_adm1,
  scale = 5566,
  stat = "mean",
  via = "drive"
)
cat("Monthly extraction finished\n")

write_csv(
  x = df_adm1_monthly,
  file = file.path(
    out_dir,
    "20231005_chirps_adm1_monthly.csv"
  )
)
