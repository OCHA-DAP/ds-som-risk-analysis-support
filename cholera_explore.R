library(tidyverse)
library(readxl)
library(janitor)
library(ISOweek)
library(gghdx)
gghdx()
fp_cholera_data <- file.path(Sys.getenv("AA_DATA_DIR"),
          "private",
          "raw",
          "som","Cholera data W1-51 2022.xlsx")

ldf_cholera <- set_names(excel_sheets(fp_cholera_data)) %>%
  map(\(sn){
    set_names(sn)
    read_xlsx(fp_cholera_data,sn) %>%
      clean_names()

  })

?ISOweek::
lubridate::epiweek()

df_weekly <- ldf_cholera$Sheet1 %>%
  # filter(is.na(epi_week))
  mutate(
    # w = paste0(year,"-W",epi_week,"-1"),
    w = paste0(year,"-W",formatC(epi_week, width=2, flag="0"),"-1"),
    date= ISOweek2date(w),
    .after= epi_week
  )

df_weekly %>%
  group_by(date) %>%
  summarise(
    total_cases = sum(total_cases,na.rm=T)
  ) %>%
  ggplot(
    aes(x= date, y= total_cases)
  )+
  geom_line()
library(snakecase)

df_region_weekly <-  df_weekly %>%
  mutate(
    region = to_snake_case(region)
  ) %>%
  group_by(region,date) %>%
  summarise(
    total_cases = sum(total_cases,na.rm=T),.groups = "drop"
  )

df_region_weekly %>%
  ggplot(
    aes(x= date, y= total_cases, color=region)
  )+
  geom_line()+
  scale_color_brewer(type = "qual",palette = "Paired")+
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%b")+
  scale_y_continuous_hdx(breaks = seq(0,900,100))+
  labs(
    y= "Total Cases",
    title = "Cholera Cases",
    subtitle = "Somalia 2022- 2023 by Region"
  )+
  theme(
    axis.title.x  = element_blank(),
    legend.title = element_blank(),
    axis.text.x = element_text(angle=90)
  )

df_region_weekly %>%
  mutate(
    year = as_factor(year(date))
  ) %>%
  ggplot(
    aes(
      x= reorder(region,total_cases),
      y=total_cases,
      fill=year,

      ),
    color="black"
  )+
  geom_boxplot(
    color="black",
    alpha= 0.5
    )+

  labs(
    x="Region",
    y="Total Cases",
    title="Somalia - Cholera Cases",
    subtitle = "Difference between 2022 & 2023 by region"
    )+
  scale_y_continuous(breaks=seq(0,800,100),expand = c(0,10))+
  coord_flip()+
  theme(
    legend.title = element_blank()
  )

df_region_yearly <- df_region_weekly %>%
  mutate(
    yr= year(date),
  ) %>%
  group_by(
    region,
    yr
  ) %>%
  summarise(
    total_cases= sum(total_cases)
  )

 df_region_yearly_dumbell <-  df_region_yearly %>%
  group_by(region) %>%
  filter(!all(total_cases==0)) %>%
  mutate(
    yr = as_factor(yr)
  ) %>%
  pivot_wider(
    names_from= yr,
    values_from= total_cases
  ) %>%
  mutate(
    class_diff = ifelse(`2023`>`2022`,"increase","decrease")
  ) %>%
  pivot_longer(
    cols = c("2022","2023"),
    names_to = "yr",values_to="total_cases"
  ) %>%
  ungroup()

 region_order <- df_region_yerly_dumbell %>%
   group_by(region) %>%
   summarise(
     mean_case = mean(total_cases)
   ) %>%
   arrange(mean_case) %>%
   pull(region)

p_dumbell <-  df_region_yearly_dumbell %>%
  mutate(
    region = fct_relevel(region, region_order)
  ) %>%
  ggplot(
    aes(x= region,
        y= total_cases,
        fill= yr
    )
  )+
  geom_point(aes(color=yr), size=3)+
  geom_line(aes(group=region), color="grey",alpha=0.5 , lwd=1)+
  labs(y= "Total Cases",
       title = "Somalia - Cholera",
       subtitle = "Total cases per regions (2022 & 2023)"
  )+
  coord_flip()+
  theme(
    legend.title = element_blank(),
    axis.title.y = element_blank()

  )


df_region_yearly_change <- df_region_yearly%>%
  ungroup() %>%
  pivot_wider(
    names_from = yr,
    values_from = total_cases
  ) %>%
  filter(!(`2023`==0 & `2022`==0)) %>%
  mutate(
    diff_change= `2023`-`2022`,
    pct_change = (`2023`-`2022`)/(`2022`),
    class_change = ifelse(pct_change<0,"negative","positive"),
    region = fct_relevel(region,region_order)
  )

p_bar_pct_change <- df_region_yearly_change %>%
  ggplot(
    aes(x= region,y= pct_change, fill=class_change)
  )+
  geom_bar(stat="identity", color="grey")+
  scale_fill_manual(
    values = c(`positive`= hdx_hex("tomato-hdx"),
               `negative`=hdx_hex("sapphire-hdx")))+
  scale_y_continuous(
    breaks = seq(-1,1, by=0.1),
    labels = paste0(c(seq(-100,90,10),"≥ 100"),"%"),
    limits = c(-1,1)
    )+
  coord_flip()+
  labs(
    title = "Somalia - Cholera Cases",
    subtitle = "% increase increase from 2022-23 by region"
  )+
  theme(
    axis.text.x = element_text(angle =90),
    legend.position = "none",
    axis.title.x = element_blank()
  )
p_bar_absolute_change<- df_region_yearly_change %>%
  ggplot(
    aes(x= region,y= diff_change, fill=class_change)
  )+
  geom_bar(stat="identity", color="grey")+
  scale_fill_manual(
    values = c(`positive`= hdx_hex("tomato-hdx"),
               `negative`=hdx_hex("sapphire-hdx")))+
  scale_y_continuous(
    breaks = seq(-2500,4000, by= 500)
    )+
  coord_flip()+
  labs(
    title = "Somalia - Cholera Cases",
    subtitle = "Increase/decrease from 2022-23 by region"
  )+
  theme(
    axis.text.x = element_text(angle =90),
    legend.position = "none",
    axis.title.x = element_blank()
  )



library(patchwork)
p_dumbell_region+
  p_bar_absolute_change+
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )+
  plot_layout(ncol=2)
df_region_weekly %>%
  mutate(
    yr= year(date),

    mo = floor_date(date, "month")
  ) %>%
  group_by(
    region,
    mo
  ) %>%
  summarise(
    total_cases= sum(total_cases)
  ) %>%
  mutate(
    mo_lab=month(mo, label=T)
  ) %>%
  arrange(region,mo_lab)

