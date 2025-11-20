


box::use(
  dplyr[...],
  forcats[...],
  ggplot2[...],
  sf[...],
  gghdx,
  DBI,
  RPostgres,
  janitor[clean_names],
  lubridate[...],
  purrr,
  readr,
  scales,
  stats,
  stringr,
  tidyr[...]
)
# gdf <- download_fieldmaps_sf("SOM","som_adm1")
# gdf <- gdf |>
#   clean_names()

gghdx$gghdx()

con <- cumulus::pg_con(stage = "prod",write= FALSE)


gdf_adm1 <- search_datasets(query = "cod-ab-som") %>%
  pluck(1) %>%
  get_resource(2) %>%
  # get_resource_layers() %>%
  read_resource(layer = "som_admbnda_adm1_ocha_20230308") %>%
  clean_names() %>%
  select(matches("^adm\\d_"))

db_seas5 <- tbl(con, "seas5") |>
  filter(
    iso3 == "SOM",
    adm_level ==1
  ) |>
  collect()

db_seas5 <- db_seas5 |>
  mutate(
    yr_issued = floor_date(issued_date, "year"),
    mm = mean * days_in_month(valid_date),
    valid_month = month(valid_date, label = T)
  ) |>
  relocate (
    mm,.before = "mean"
  )

db_seas5_seasonal_sum <- db_seas5 |>
  filter(
    month(issued_date)==10,
    month(valid_date) %in% c(3,4)
    ) |>
  mutate(
    yr_issued = floor_date(issued_date, "year"),
    mm = mean * days_in_month(valid_date)
  ) |>
  group_by(iso3, pcode, yr_issued) |>
  summarise(
    mm = sum(mm)
  ) |>
  mutate(
    latest_year = yr_issued ==max(yr_issued)
  )




db_seas5_seasonal_sum |>
  ggplot(
    aes(x= pcode, y= mm, color = latest_year, alpha = latest_year)
  )+
  geom_boxplot(fill = "grey", color = "black", alpha = 0.3)+
  geom_jitter(
  )+
  scale_color_manual(
    values = c("TRUE" ="red", "FALSE" = "black")
  )+
  scale_alpha_manual(
    values = c("TRUE" =1, "FALSE" = 0.2)
  ) +
  coord_flip() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "March & April Total Rainfall Predictions From the Month of October",
    subtitle = "Current year (red) has a higher than average/usual rainfall prediction"
  )

db_seas5_seasonal_sum  |>
  mutate(
    pctile_rank = percent_rank(mm)
  ) |>
  filter(yr_issued =="2024-01-01")



# Can see exceptionally low ONDJ
db_seas5 |>
  filter(
    month(issued_date)==10
  ) |>
  mutate(
    latest_year = yr_issued ==max(yr_issued),
    valid_month = fct_relevel(valid_month, "Oct","Nov","Dec","Jan","Feb","Mar","Apr")
  ) |>
  ggplot(
    aes(x= pcode, y= mm, color = latest_year, alpha = latest_year)
  )+
  geom_boxplot(fill = "grey", color = "black", alpha = 0.3)+
  geom_jitter(
  )+
  scale_color_manual(
    values = c("TRUE" ="red", "FALSE" = "black")
  )+
  scale_alpha_manual(
    values = c("TRUE" =1, "FALSE" = 0.2)
  ) +
  coord_flip() +
  facet_wrap(~valid_month,scales= "free_x")

db_seas5_oct <- db_seas5 |>
  ungroup() |>
  filter(
    month(issued_date) ==10
  ) |>
  mutate(
    valid_month = as_factor(month(valid_date,label = T, abbr=T)),
    valid_month = fct_drop(valid_month),
    valid_month = fct_relevel(valid_month, "Oct","Nov","Dec","Jan","Feb","Mar","Apr")
  ) |>

  group_by(pcode, yr_issued) |>
  arrange(pcode, valid_date) |>
  mutate(
    mm_cum = cumsum(mm),.after = "mm"
  )



db_seas5_oct_label <- db_seas5_oct |>
  left_join(
    gdf_adm1 |>
      st_drop_geometry(),
    by = c("pcode"="adm1_pcode")
  ) |>
  ungroup() |>
  mutate(
    latest_year = yr_issued==max(yr_issued)
  )

# Shabelles, Bay look pretty bad
p_cumulative_oct <- db_seas5_oct_label |>
  ggplot(
    aes(x=valid_month,
        y = mm_cum,
        group = yr_issued,
        color = latest_year,
        alpha= latest_year)
  )+
  geom_point()+
  geom_line()+
  scale_color_manual(
    values = c("TRUE" =hdx_hex("tomato-hdx"), "FALSE" = hdx_hex("sapphire-hdx"))
  )+
  scale_alpha_manual(
    values = c("TRUE" =1, "FALSE" = 0.1)
  ) +
  facet_wrap(
    ~adm1_en, scale ="free_y"
  ) +
  labs(
    title = "Cumulative Rainfall",
    # subtitle = "All october forecast publications: red line is current year",
    y = "Cumulative Rainfall (mm)",
    x = "Month"

  )+
  theme(
    legend.position = "none",
    axis.text.x =element_text(angle=90)
  )

box::use(ggtext[...])
# Look at monthly data yuo can see the cumulative lows
# are mostly due to the low oct-dec vals
p_individual_month_oct <- db_seas5_oct_label |>
  ggplot(
    aes(x=valid_month,
        y = mm,
        group = yr_issued,
        color = latest_year,
        alpha= latest_year)
  )+
  geom_point()+
  geom_line()+
  scale_color_manual(
    values = c("TRUE" =hdx_hex("tomato-hdx"), "FALSE" = hdx_hex("sapphire-hdx"))
  )+
  scale_alpha_manual(
    values = c("TRUE" =1, "FALSE" = 0.1)
  ) +
  facet_wrap(
    ~adm1_en, scale ="free_y"
  )+
  labs(
    title = "Rainfall per month",
    # title = "Predicted rainfall each month October -  April",
    # subtitle = "All october forecast publications: red line is current year",
    y = "Rainfall (mm)",
    x = "Month"

  )+
  theme(
    legend.position = "none",
    axis.text.x =element_text(angle=90)
  )

p_cumulative_oct +
  p_individual_month_oct+
  plot_annotation(
    # title = "Here <span style='color:#953011;'><strong>is a colourful title</strong></span>",
    title = "How <span style='color:#F2645A;'>2024 October Forecast</span> Compares to <span style='color:#0063B3;'>previous years</span>",
    # title = "How  <span style='color: '#F2645A';> 2024 October Forecast</span> Compares to previous years",
    theme =theme(plot.title = element_markdown(lineheight = 1.1))
  )

 db_seas5_oct_label |>
  group_by(
    pcode, adm1_en, valid_month
  ) |>
  summarise(
    anom = mm[year(yr_issued)==2024]-mean(mm[year(yr_issued)<=2023],na.rm=T),.groups="drop"
  ) |>
  arrange(
    valid_month, anom
  ) |>
  mutate(
    rank = row_number(),
    worst4_oct = ifelse(adm1_en %in% unique(adm1_en[valid_month == "Oct" & rank %in% 1:4]),adm1_en,"other")
    # worst4_oct = ifelse(valid_month == "Oct" & rank %in% 1:4,adm1_en, "other")
  ) |>
  ggplot(
    aes(x=valid_month,
        y = anom,
        group = adm1_en,
        color = worst4_oct,
        alpha= worst4_oct
        )
  )+
  geom_point(
    # alpha=0.5,
    size =3)+
  geom_line(
    # alpha= 0.8
    )+
  scale_color_brewer(palette = "Set1") +
  scale_alpha_manual(
    values = c(1,1,1,1,0.25)
    )+
   theme(
     legend.title = element_blank()
   )+
   labs(
     title = "Predicted rainfall anomaly per month October 24' -  April 25'",
     subtitle = "In regions with below average rainfall, the rainfall deficit is most extreme in Oct, Nov, Dec",
     y = "Anomaly (mm)",
     x = "Month"
   )

  scale_color_manual(
    values = c("TRUE" =hdx_hex("tomato-hdx"), "FALSE" = hdx_hex("sapphire-hdx"))
  )+
  scale_alpha_manual(
    values = c("TRUE" =1, "FALSE" = 0.1)
  ) +
  facet_wrap(
    ~adm1_en, scale ="free_y"
  )+
  labs(
    title = "Rainfall per month",
    # title = "Predicted rainfall each month October -  April",
    # subtitle = "All october forecast publications: red line is current year",
    y = "Rainfall (mm)",
    x = "Month"

  )+
  theme(
    legend.position = "none",
    axis.text.x =element_text(angle=90)
  )


db_seas5_oct_label |>
  group_by(
    pcode, adm1_en, leadtime,valid_month
    ) |>
  mutate(
    pct_rank = percent_rank(mm),.after= "mm",
    pct_rank_rev= 1- pct_rank
  ) |>
  ungroup() |>
  ggplot(
    aes(x=valid_month,
        y = 1-pct_rank,
        # y = 1/pct_rank,
        group = yr_issued,
        color = latest_year,
        alpha= latest_year)
  )+
  geom_point()+
  geom_line()+
  scale_color_manual(
    values = c("TRUE" ="red", "FALSE" = "black")
  )+
  scale_alpha_manual(
    values = c("TRUE" =1, "FALSE" = 0.05)
  ) +
  scale_y_continuous(limits = c(1,10))+
  facet_wrap(
    ~adm1_en, scale ="free_y"
  )


# could potentially split this one up into groups

admin_min_cum <- db_seas5_oct_label |>
  group_by(pcode) |>
  summarise(
    cum_max = max(mm_cum)
  ) |>
  slice_min(cum_max,n=5)

db_seas5_oct_pctile <- db_seas5_oct_label |>
  group_by(
    pcode, adm1_en, leadtime,valid_month
    ) |>
  mutate(
    min_5 = ifelse(pcode %in% admin_min_cum$pcode,adm1_en, "Other"),
    pct_rank = percent_rank(mm),.after= "mm",
    pct_rank_rev= 1- pct_rank
  ) |>
  ungroup() |>
  filter(year(yr_issued )==2024)

db_seas5_oct_pctile |>
  group_by(
    pcode, adm1_en,
  ) |>
  summarise(
    pct_rank = mean(pct_rank),.groups="drop"
  ) |>
  slice_max(order_by = pct_rank,n =5)
ggplot(
    aes(x=valid_month,
        y = 1-pct_rank,
        # y = 1/pct_rank,
        group = adm1_en,
        color =min_5
        # color = adm1_en
        # alpha= latest_year
        )
  )+
  geom_point()+
  geom_line()+
  scale_y_continuous(labels =scales::label_percent())+
  labs(
    title = "drier than x% of years"
  )
  # scale_color_manual(
  #   values = c("TRUE" ="red", "FALSE" = "black")
  # )+
  # scale_alpha_manual(
  #   values = c("TRUE" =1, "FALSE" = 0.05)
  # )
  # # scale_y_continuous(limits = c(1,10))+
  # facet_wrap(
  #   ~adm1_en, scale ="free_y"
  # )


gdf_adm1 |>
  left_join(
    db_seas5_oct_label
  )
# tbl(con,"iso3")
