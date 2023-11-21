library(tidyverse)
library(openxlsx)
tar_load(df_adm1_max_range_table)
tar_load(df_adm2_max_range_table)


df_adm2_max_range_table %>% glimpse()
df_adm1_max_range_table %>% glimpse()


df_adm1_pop_nums_export <- df_adm1_max_range_table %>%
  mutate(

    across(
        matches("mam_pop_exposed|ond_pop_exposed"),~round(.x,-2)
      ),
    across(
      c("pop_24","final_pin"),~round(.x,0)
    ),
    across(
      contains("pct"),~round(.x,2)
    )
    ) %>%
  rename(
    `Region` = adm1_en,
    `Region pcode` = adm1_pcode,
    `MAM Pop exposed (min)` = mam_pop_exposed_24_min,
    `MAM Pop exposed (max)` = mam_pop_exposed_24_max,
    `MAM Pop % exposed (min)` = mam_pct_pop_exposed_24_min,
    `MAM Pop % exposed (max)` = mam_pct_pop_exposed_24_max,
    `OND Pop exposed (min)` = ond_pop_exposed_24_min,
    `OND Pop exposed (max)` = ond_pop_exposed_24_max,
    `OND Pop % exposed (min)` = ond_pct_pop_exposed_24_min,
    `OND Pop % exposed (man)` = ond_pct_pop_exposed_24_max,
    `Final PiN` = final_pin
  )

df_adm1_pop_nums_export %>%
  glimpse()
df_adm2_pop_nums_export <- df_adm2_max_range_table %>%
  mutate(
    across(
      matches("mam_pop_exposed|ond_pop_exposed"),~round(.x,-2)
    ),
    across(
      c("pop_24","final_pin"),~round(.x,0)
    ),
    across(
      contains("pct"),~round(.x,2)
    )
  ) %>%
  rename(
    `Region` = adm1_en,
    `Region pcode` = adm1_pcode,
    `District` = adm2_en,
    `District pcode` = adm2_pcode,

    `MAM Pop exposed (min)` = mam_pop_exposed_24_min,
    `MAM Pop exposed (max)` = mam_pop_exposed_24_max,
    `MAM Pop % exposed (min)` = mam_pct_exposed_min,
    `MAM Pop % exposed (max)` = mam_pct_exposed_max,
    `OND Pop exposed (min)` = ond_pop_exposed_24_min,
    `OND Pop exposed (max)` = ond_pop_exposed_24_max,
    `OND Pop % exposed (min)` = ond_pct_exposed_min,
    `OND Pop % exposed (max)` = ond_pct_exposed_max,
    `Final PiN` = final_pin
  )

list_of_datasets <- list("Region Level" = df_adm1_pop_nums_export, "District Level" = df_adm2_pop_nums_export)
write.xlsx(list_of_datasets, file = "ocha_somalia_flood_exposure_estimates_2024.xlsx")
