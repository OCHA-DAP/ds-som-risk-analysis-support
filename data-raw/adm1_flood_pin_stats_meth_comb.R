library(targets)
library(sf)
library(tidyverse)
tar_load(gdf_adm1_summary_ranges_map)


gdf_adm1_summary_ranges_map %>%
  st_write(
    "som_flood.gpkg",
    layer = "adm1_flood_pin_stats_meth_comb"

  )
