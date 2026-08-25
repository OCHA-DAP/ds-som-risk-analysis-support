#' This script download the latest and most forward looking ECMWF forecast (SEAS 51) available
#' at the time of analysis (October 2023 forecast w/ max lead times 4-6)
#'
#' The data is downloaded over the AOI defined in the SOM admin boundaries
#' I had originally thought we might further integrate historical ECMWF data and wrote code
#' to download all of the historical SEAS51 ECMWF forecast data over somalia. Later we decided
#' it is out of scope. I don't want to yet delete the code to do this as we might return to it later for
#' this project or others, therefore, I created a `download_historical` parameter and set it to `FALSE`
#' and made this download conditional down at the bottom of the script
#'
#' The purpose of this specific data is further explained in `exploration/ECMWF.Rmd`



download_historical <- FALSE

library(ecmwfr)
library(tidyverse)
library(sf)
# library(rnaturalearth)
library(glue)
library(janitor)
library(rhdx)



fp_outpath_ecmwf <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "raw",
  "som",
  "ecmwf_seasonal",
  "seas51"
)

# think only need to do this 1x
ecmwfr::wf_set_key(
  user = Sys.getenv("ECMWF_USER_UID"),
  key = Sys.getenv("ECMWF_USER_KEY"),
  service = "cds"
)

df_cds_datasets <- wf_datasets(
  user = Sys.getenv("ECMWF_USER_UID"),
  service = "cds"
)

# view
df_cds_datasets %>%
  tibble() %>%
  filter(
    str_detect(name, "seas")
  )

# checkout some meta-data
prod_info <- wf_product_info(
  dataset = "seasonal-monthly-single-levels",
  user = Sys.getenv("ECMWF_USER_UID"),
  service = "cds"
)

cat("printing layers\n")
str_extract_all(prod_info$rich_abstract, pattern = "<td class='variables-name'>(.*?)</td>")


cat("defining bbox for extraction\n")
som_adm_levels <- c("som_admbnda_adm0_ocha_20230308")

gdf_adm0 <- search_datasets("Somalia - Subnational Administrative Boundaries") %>%
  pluck(1) %>%
  get_resource(2) %>%
  read_resource(layer = som_adm_levels) %>%
  clean_names() %>%
  select(matches("^adm\\d_"))


# convert AOI into necessary format for ECMWF API
aoi_bbox <- st_bbox(gdf_adm0)

cat("writing data requests to list\n")
request_coords <- glue("{aoi_bbox['ymin']}/{aoi_bbox['xmin']}/{aoi_bbox['ymax']}/{aoi_bbox['xmax']}")


req_oct2023 <- c(4:6) %>%
  map(
    ~ list(
      product_type = "monthly_mean",
      format = "netcdf",
      originating_centre = "ecmwf",
      system = "51",
      variable = c("total_precipitation"),
      year = as.character(c(2023)),
      month = "10",
      area = request_coords,
      leadtime_month = .x,
      dataset_short_name = "seasonal-monthly-single-levels",
      target = glue("ecmwf_forecast_2023_lt{.x}.nc")
    )
  )

cat("Downloading 2023 data lts 4:6\n")
req_oct2023 %>%
  map(\(rq){
    wf_request(
      user = Sys.getenv("ECMWF_USER_UID"), # user ID (for authentication)
      request = rq, # the request
      transfer = TRUE, # download the file
      path = fp_outpath_ecmwf
    )
  })


if(download_historical){

  # We have to split 2022 and 2023 (latest year) due to limitation in API
  # for 1981-2022 we can request months 1:12, for 2023 if we do that it will fail because not all months have been published
  # it would definitely be nice if API let you access last month published.

  request_lte_2022 <- c(1:4) %>%
    map(
      ~ list(
        product_type = "monthly_mean",
        format = "netcdf",
        originating_centre = "ecmwf",
        system = "51",
        variable = c("total_precipitation"),
        year = as.character(c(1981:2022)),
        month = sprintf("%02d", c(1:12)),
        area = request_coords,
        leadtime_month = .x,
        dataset_short_name = "seasonal-monthly-single-levels",
        target = glue("ecmwf_forecast_lte2022_lt{.x}.nc")
      )
    )

  request_2023 <- c(1:4) %>%
    map(
      ~ list(
        product_type = "monthly_mean",
        format = "netcdf",
        originating_centre = "ecmwf",
        system = "51",
        variable = c("total_precipitation"),
        year = as.character(c(2023)),
        month = sprintf("%02d", c(1:10)),
        area = request_coords,
        leadtime_month = .x,
        dataset_short_name = "seasonal-monthly-single-levels",
        target = glue("ecmwf_forecast_2023_lt{.x}.nc")
      )
    )


  request_2023 %>%
    map(\(rq){
      wf_request(
        user = Sys.getenv("ECMWF_USER_UID"), # user ID (for authentication)
        request = rq, # the request
        transfer = TRUE, # download the file
        path = fp_outpath_ecmwf
      )
    })

  request_lte_2022 %>%
    map(\(rq){
      wf_request(
        user = Sys.getenv("ECMWF_USER_UID"), # user ID (for authentication)
        request = rq, # the request
        transfer = TRUE, # download the file
        path = fp_outpath_ecmwf
      )
    })
}
