library(sf)
library(tidyverse)

# from visual inspection in QGIS I know that this level 3
# basin covers both Juba an Shabelle river basins into ETH highlands
hybasid_juba_shabelle <- 1030008100


# reading this in from GFF/GFH project - which contains all hydrobasin levels (1-12) for the African content.
# it should go somewhere in our `AA_DATA_DIR` - but currently defined by country level data schema

input_dir <- file.path(
  # from GFH proj
  Sys.getenv("GFH_DATA_DIR"),
  "inputs"
)


# geodataframe basin 3
gdf_bas3 <- st_read(
  dsn = file.path(
    input_dir,
    "hybas_af_lev01-12_v1c"
  ),
  layer = "hybas_af_lev03_v1c"
) %>%
  clean_names() %>%
  filter(hybas_id == hybasid_juba_shabelle)

# will drop this in `AA_DATA`

out_gpkg <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public",
  "raw",
  "som",
  "hydrobasin.gpkg"
)

st_write(gdf_bas3, out_gpkg, "hybas3_af_lev03_v1c_shabelle_juba", append = T)
