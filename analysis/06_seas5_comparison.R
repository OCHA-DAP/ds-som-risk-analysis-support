# SEAS5 Forecast Comparison Analysis for Somalia
# This script analyzes ECMWF SEAS5 seasonal rainfall forecasts for Somalia,
# comparing current year predictions against historical forecasts to identify
# rainfall anomalies and drought risk patterns.
# This was originally madea as a one-off script in Oct 2024 to support
# 2025 HNRP risk analysis suggestions, but since support was requested again
# in Nov 2025 (for 2026 HNRP) I made a few more parameters configurable so
# User can adjust in beginning of script.

# the following objects were used in slides:
# [Somalia_Risk_Analysis_Support_Update_2026_HNRP](
# https://docs.google.com/presentation/d/1ZwjHaSN1L3Eh3dH6j3EFLxH8E0Rno1_B_v6zp2uqB94/edit?slide=id.g2456d9f415a_0_102#slide=id.g2456d9f415a_0_102)
#' - p_cumulative_individual_months
#' - p_monthly_anomaly
#' - map_rp_binned

# Load required packages ----
box::use(
  dplyr[...],
  forcats[...],
  ggplot2[...],
  sf[...],
  gghdx[...],
  DBI,
  RPostgres,
  janitor[clean_names],
  lubridate[...],
  purrr,
  readr,
  scales,
  stats,
  stringr,
  tidyr[...],
  ggtext[...],
  patchwork[...],
  glue[...]
)

# Configuration ----
# Set the forecast issue month (10=Oct, 11=Nov, 12=Dec)
FORECAST_ISSUED_MONTH <- c(10, 11, 12)[2]
ISSUED_YEAR_DATE <- c("2025-01-01")

# Depending when run, not all of MAM season may be available in forecast.
MAM_VALID_MONTHS_AVAILABLE <- c(3, 4, 5)
HDX_GREEN <- "#1bb580"

# Setup ----
# Initialize HDX theme for visualizations
gghdx()
# Connect to production database (read-only)
con <- cumulus::pg_con(stage = "prod", write = FALSE)
# Load Somalia Admin Level 1 boundaries
gdf_adm1 <- cumulus::download_fieldmaps_sf("SOM", "som_adm1")$som_adm1 |>
  clean_names()


# Load SEAS5 Data ----
# Query SEAS5 forecast data from database for Somalia at Admin Level 1
db_seas5 <- tbl(con, "seas5") |>
  filter(
    iso3 == "SOM",
    adm_level == 1
  ) |>
  collect()

# Process SEAS5 data: convert mean rainfall to mm and add temporal variables
db_seas5 <- db_seas5 |>
  mutate(
    yr_issued = floor_date(issued_date, "year"),
    mm = mean * days_in_month(valid_date), # Convert mean daily to monthly total
    valid_month = month(valid_date, label = T)
  ) |>
  relocate(
    mm,
    .before = "mean"
  )


# Seasonal Gu Summary Analysis ----
# Calculate March-April total rainfall from forecasts issued in specified month
# This focuses on the Gu (main rainy) season
db_seas5_gu_seasonal_sum <- db_seas5 |>
  filter(
    month(issued_date) == FORECAST_ISSUED_MONTH,
    month(valid_date) %in% MAM_VALID_MONTHS_AVAILABLE # Gu season
  ) |>
  mutate(
    yr_issued = floor_date(issued_date, "year"),
    mm = mean * days_in_month(valid_date)
  ) |>
  group_by(iso3, pcode, yr_issued) |>
  summarise(
    mm = sum(mm) # Total rainfall across March & April
  ) |>
  mutate(
    latest_year = yr_issued == max(yr_issued)
  )

# Visualization: March-April Total Rainfall Distribution ----
# Compare current year forecast against historical distribution
db_seas5_gu_seasonal_sum |>
  ggplot(
    aes(x = pcode, y = mm, color = latest_year, alpha = latest_year)
  ) +
  geom_boxplot(fill = "grey", color = "black", alpha = 0.3) +
  geom_jitter() +
  scale_color_manual(
    values = c("TRUE" = "red", "FALSE" = "black")
  ) +
  scale_alpha_manual(
    values = c("TRUE" = 1, "FALSE" = 0.2)
  ) +
  coord_flip() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "March & April Total Rainfall Predictions From the Month of October",
    subtitle = "Current year (red) has a higher than average/usual rainfall prediction"
  )

# Calculate percentile ranking for 2025 forecast
db_seas5_seasonal_sum |>
  mutate(
    pctile_rank = percent_rank(mm)
  ) |>

  filter(yr_issued == ISSUED_YEAR_DATE)

# Define valid forecast months ----
# Creates a vector of month numbers (1-12) that cycles correctly
# e.g., if FORECAST_ISSUED_MONTH=11, returns c(11,12,1,2,3,4,5)
VALID_MONTHS_INT = ((FORECAST_ISSUED_MONTH - 1 + 0:6) %% 12) + 1

# Monthly Rainfall Distribution by Admin Unit ----
# Faceted visualization showing each forecast month's rainfall distribution
db_seas5 |>
  filter(
    month(issued_date) == FORECAST_ISSUED_MONTH,
  ) |>
  mutate(
    latest_year = yr_issued == max(yr_issued),
    # Reorder months chronologically based on forecast period
    valid_month = fct_relevel(valid_month, month.abb[VALID_MONTHS_INT])
  ) |>
  ggplot(
    aes(x = pcode, y = mm, color = latest_year, alpha = latest_year)
  ) +
  geom_boxplot(fill = "grey", color = "black", alpha = 0.3) +
  geom_jitter() +
  scale_color_manual(
    values = c("TRUE" = "red", "FALSE" = "black")
  ) +
  scale_alpha_manual(
    values = c("TRUE" = 1, "FALSE" = 0.2)
  ) +
  coord_flip() +
  facet_wrap(~valid_month, scales = "free_x")


# Prepare Data for Time Series Analysis ----
# Filter forecasts by issue month and calculate cumulative rainfall
db_seas5_issued_mo <- db_seas5 |>
  ungroup() |>
  filter(
    month(issued_date) == FORECAST_ISSUED_MONTH
  ) |>
  mutate(
    valid_month = as_factor(month(valid_date, label = T, abbr = T)),
    valid_month = fct_drop(valid_month),
    # Order months chronologically for proper time series display
    valid_month = fct_relevel(valid_month, month.abb[VALID_MONTHS_INT])
  ) |>
  group_by(pcode, yr_issued) |>
  arrange(pcode, valid_date) |>
  mutate(
    # Calculate cumulative rainfall across the forecast period
    mm_cum = cumsum(mm),
    .after = "mm"
  )

# Join with admin labels for plotting ----
db_seas5_issued_mo_label <- db_seas5_issued_mo |>
  left_join(
    gdf_adm1 |>
      st_drop_geometry(),
    by = c("pcode" = "adm1_pcode")
  ) |>
  ungroup() |>
  mutate(
    latest_year = yr_issued == max(yr_issued)
  )

# Visualization: Cumulative Rainfall Over Time ----
# Shows how cumulative rainfall builds up across the forecast period
# Highlights current year (red) vs historical years (blue)
p_cumulative_month <- db_seas5_issued_mo_label |>
  ggplot(
    aes(
      x = valid_month,
      y = mm_cum,
      group = yr_issued,
      color = latest_year,
      alpha = latest_year
    )
  ) +
  geom_point() +
  geom_line() +
  scale_color_manual(
    values = c(
      "TRUE" = hdx_hex("tomato-hdx"),
      "FALSE" = hdx_hex("sapphire-hdx")
    )
  ) +
  scale_alpha_manual(
    values = c("TRUE" = 1, "FALSE" = 0.1)
  ) +
  facet_wrap(
    ~adm1_en,
    scale = "free_y"
  ) +
  labs(
    title = "Cumulative Rainfall",
    y = "Cumulative Rainfall (mm)",
    x = "Month"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90)
  )

# Visualization: Monthly Rainfall (Non-Cumulative) ----
# Individual monthly values help identify which specific months
# contribute most to cumulative deficits (e.g., Oct-Dec lows)
p_individual_month <- db_seas5_issued_mo_label |>
  ggplot(
    aes(
      x = valid_month,
      y = mm,
      group = yr_issued,
      color = latest_year,
      alpha = latest_year
    )
  ) +
  geom_point() +
  geom_line() +
  scale_color_manual(
    values = c(
      "TRUE" = hdx_hex("tomato-hdx"),
      "FALSE" = hdx_hex("sapphire-hdx")
    )
  ) +
  scale_alpha_manual(
    values = c("TRUE" = 1, "FALSE" = 0.1)
  ) +
  facet_wrap(
    ~adm1_en,
    scale = "free_y"
  ) +
  labs(
    title = "Rainfall per month",
    y = "Rainfall (mm)",
    x = "Month"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90)
  )



# Combined Plot: Side-by-side comparison of cumulative and monthly rainfall
p_cumulative_individual_months <- p_cumulative_month +
  p_individual_month +
  plot_annotation(
    title = "How <span style='color:#F2645A;'>2025 November Forecast</span> Compares to <span style='color:#0063B3;'>previous years</span>",
    theme = theme(plot.title = element_markdown(lineheight = 1.1))
  )
p_cumulative_individual_months
# Rainfall Anomaly Analysis ----
# Calculate current year anomalies relative to historical mean (<=(year- 1)
# Highlights regions with worst deficits in the forecast issue month

current_year <-  year(ISSUED_YEAR_DATE)
prev_year <- current_year -1
current_year_title_component <- stringr::str_sub(current_year, start=3,end=4)
next_year_title_component <- stringr::str_sub(current_year+1, start=3,end=4)


p_monthly_anomaly <- db_seas5_issued_mo_label |>
  group_by(
    pcode,
    adm1_en,
    valid_month
  ) |>
  summarise(
    # Anomaly = current year forecast minus historical average
    anom = mm[year(yr_issued) == current_year] -
      mean(mm[year(yr_issued) <= prev_year], na.rm = T),
    .groups = "drop"
  ) |>
  arrange(
    valid_month,
    anom
  ) |>
  mutate(
    rank = row_number(),
    # Identify 4 worst-affected regions in the forecast issue month
    worst4_oct = ifelse(
      adm1_en %in%
        unique(adm1_en[
          valid_month == month.abb[FORECAST_ISSUED_MONTH] & rank %in% 1:4
        ]),
      adm1_en,
      "other"
    )
  ) |>
  ggplot(
    aes(
      x = valid_month,
      y = anom,
      group = adm1_en,
      color = worst4_oct,
      alpha = worst4_oct
    )
  ) +
  geom_point(
    size = 3
  ) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  scale_alpha_manual(
    values = c(1, 1, 1, 1, 0.25)
  ) +
  theme(
    legend.title = element_blank()
  ) +
  labs(
    title = glue("Predicted rainfall anomaly per month Nov {current_year_title_component}' -  May {next_year_title_component}'"),
    subtitle = "In regions with below average rainfall, the rainfall deficit is most extreme in Nov-Dec",
    y = "Anomaly (mm)",
    x = "Month"
  )
p_monthly_anomaly


# Identify Most Affected Regions ----
# Find 5 admin units with lowest cumulative rainfall forecasts
admin_min_cum <- db_seas5_issued_mo_label |>
  group_by(pcode) |>
  summarise(
    cum_max = max(mm_cum)
  ) |>
  slice_min(cum_max, n = 5)

# Calculate percentile ranks for current year, highlighting most affected regions
db_seas5_issued_mo_pctile <- db_seas5_issued_mo_label |>
  group_by(
    pcode,
    adm1_en,
    leadtime,
    valid_month
  ) |>
  mutate(
    min_5 = ifelse(pcode %in% admin_min_cum$pcode, adm1_en, "Other"),
    pct_rank = percent_rank(mm),
    .after = "mm",
    pct_rank_rev = 1 - pct_rank
  ) |>
  ungroup() |>
  filter(year(yr_issued) == current_year)

# Summary: Average percentile rank across all months for top 5 wettest regions
db_seas5_issued_mo_pctile |>
  group_by(
    pcode,
    adm1_en,
  ) |>
  summarise(
    pct_rank = mean(pct_rank),
    .groups = "drop"
  ) |>
  slice_max(order_by = pct_rank, n = 5)

# Visualization: Drought severity over time for most affected regions
ggplot(
  data = db_seas5_issued_mo_pctile,
  aes(x = valid_month, y = 1 - pct_rank, group = adm1_en, color = min_5)
) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title = "drier than x% of years"
  )



# ============================================================================
# RETURN PERIOD ANALYSIS
# ============================================================================
# Calculate empirical return periods for rainfall deficit events
# Higher RP = rarer/more severe low rainfall event

# Function: Calculate Empirical Return Period ----
# direction="1": low values get high RP (for drought analysis)
# direction="-1": high values get high RP (for flood analysis)
rp_empirical <- function(x, direction = c("1", "-1"), ties_method = "average") {
  direction <- as.numeric(rlang::arg_match(direction))
  rank <- rank(x * direction, ties.method = ties_method)
  q_rank <- rank / (length(x) + 1) # Quantile rank
  rp <- 1 / q_rank # Return period
  return(rp)
}

# Load Historical SEAS5 Data ----
df_seas5 = cumulus::pg_load_seas5_historical(iso3 = "som", adm_level = 1)


# Calculate Return Periods and Terciles ----
# For each admin unit and month, calculate empirical return periods
df_seas5_rp <- df_seas5 |>
  mutate(
    valid_month = month(valid_date, label = TRUE, abbr = TRUE),
    issue_month = month(issued_date, label = TRUE, abbr = TRUE),
  ) |>
  # Filter to forecasts issued in specified month
  filter(issue_month == month.abb[FORECAST_ISSUED_MONTH]) |>
  group_by(iso3, pcode, name, valid_month) |>
  mutate(
    # Calculate empirical return period (direction="1" = low values = high RP)
    rp_empirical = rp_empirical(
      mean,
      ties_method = "average",
      direction = "1"
    ),
    mm = days_in_month(valid_date) * mean, # Convert to monthly total
    abs_anomaly = mm - mean(mm, na.rm = TRUE), # Absolute anomaly
    tercile = ntile(mm, 3), # Divide into three equal groups
    is_lower_tercile = tercile == 1, # Flag lowest tercile (driest conditions)
    current_year = year(valid_date) == current_year,
    label_txt = ifelse(current_year, year(valid_date), NA)
  ) |>
  arrange(
    pcode,
    valid_date
  ) |>
  ungroup()


df_sel <- df_seas5_rp |>
  filter(
    year(valid_date) == 2025
  )


gdf_sel <- gdf_adm1 %>%
  full_join(df_sel, by = setNames("pcode", "adm1_pcode")) |>
  filter(
    valid_month %in% month.abb[VALID_MONTHS_INT]
  ) |>
  mutate(
    valid_month_fct = fct_relevel(valid_month, month.abb[VALID_MONTHS_INT])
  )

ggplot() +
  geom_sf(
    data = gdf_sel,
    aes(fill = rp_empirical)
  ) +
  facet_wrap(~valid_month_fct) +
  gghdx::scale_fill_gradient_hdx_tomato(
    na.value = "lightgrey",
    name = "Rainfall Return Period"
  ) +
  # scale_fill_hdx_tomato()+
  theme_void()

gdf_sel_bin <- gdf_sel |>
  mutate(
    rp_bin = cut(
      rp_empirical,
      breaks = c(-Inf, 2, 5, 10, 20, Inf),
      labels = c("<2", "2-5", "5-10", "10-20", "≥20"),
      right = FALSE
    )
  )


map_rp_binned <- ggplot() +
  geom_sf(
    data = gdf_sel_bin,
    aes(fill = rp_bin),
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "<2" = "#ffffff",

      "2-5" = "#ffffe5",
      "5-10" = "#ffeda0",
      "10-20" = "#feb24c",
      "≥20" = "#f03b20"
    ),
    name = "Return Period"
  ) +
  labs(
    title = "November 2025 Issued Forecast",
    subtitle = "Monthly rainfall defecit empirical return periods"
  ) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  facet_wrap(~valid_month_fct) +
  theme_void()


map_rp_binned

df_seas5_rp |>
  filter(
    valid_month %in% month.abb[VALID_MONTHS_INT],
  ) |>
  ggplot(
    aes(x = name, y = mm, color = current_year, alpha = current_year)
  ) +
  labs(
    x = "Rainfall (mm)",
    y = "Province"
  ) +
  geom_violin() +
  geom_jitter() +
  geom_text(aes(label = label_txt)) +
  facet_wrap(~valid_month) +

  coord_flip() +
  theme(
    panel.border = element_rect(fill = NA, color = "black"),
    strip.background = element_rect(fill = HDX_GREEN),
    strip.text = element_text(color = "white"),
    legend.position = "none"
  )



ggplot() +
  geom_sf(
    data = filter(gdf_sel, valid_month %in% month.abb[VALID_MONTHS_INT]),
    aes(fill = abs_anomaly)
  ) +
  geom_sf(data = gdf_lower_filt, color = "tomato", fill = NA) +
  scale_color_manual(
    name = NULL, # No title for boundary legend
    values = c("Lower tercile rainfall" = "tomato") # Color for the boundary lines
  ) +
  scale_fill_gradient_hdx_tomato(
    na.value = "lightgrey",
    name = "Rainfall Anomaly (mm)"
  ) +
  labs(
    title = glue("Rainfall Return Periods"),
    caption = "Higher return periods indicate rarer low-rainfall events"
  ) +
  facet_wrap(~valid_month)
