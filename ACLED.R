# remotes::install_gitlab("chris-dworschak/acled.api")
# install.packages("acled.api")
library(gghdx)
library(tidyverse)
library(acled.api)
gghdx()
# library(UCDPtools)

#
# data(UCDPindex)
# browseURL(UCDPindex$codebook_link[UCDPindex$shortname=="One-Sided"])
# onesided <- getUCDP("One-Sided", rawdata=TRUE, rawnames=TRUE)

fp_upsala <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public",
  "raw",
  "som",
  "upsala",
  "gedevents-2023-09-18.csv")

df_upsala <-  read_csv(fp_upsala)

df_upsala_yr_mo_summary_adm0 <- df_upsala %>%
  mutate(
    date= mdy_hms(date_end) %>% as_date(),
    date = floor_date(date,"month")
  ) %>%
  # select(starts_with("date"),yr_mo)
  group_by(
    date
  ) %>%
  summarise(
    fatalities = sum(best_est,na.rm = T),.groups="drop"
  ) %>%
  mutate(dataset = "Upsala")


df_acled_short <- acled.api(
  country = "Somalia",
  start.date = "202-01-01",
  access.key = Sys.getenv("ACLED_ACCESS_KEY"),
  email.address = Sys.getenv("ACLED_USER_EMAIL"),
  end.date = Sys.Date()
)
df_acled_long <- acled.api(
  country = "Somalia",
  start.date = "2010-01-01",
  access.key = Sys.getenv("ACLED_ACCESS_KEY"),
  email.address = Sys.getenv("ACLED_USER_EMAIL"),
  end.date = Sys.Date()
)
df_acled_long %>%
  View()


df_acled_long %>%
  filter(event_date>=as_date("2018-01-01")) %>%
  count(event_type) %>%
  ggplot(aes(x= reorder(event_type,n), y= n))+
  geom_bar(stat='identity')+
  coord_flip()+
  labs(x= 'Number of events',y="Event Type", title = "Event frequencies since 2018", subtitle = "ACLED: Somalia")

filter(event_type %in% c("Battles",
                           "Riots",
                           "Violence against civilians")) %>%
  tibble()

df_freq_sub_events_violent <- df_acled_long %>%
  filter(event_date>=as_date("2018-01-01")) %>%
  filter(event_type %in% c("Battles",
                           # "Riots",
                           "Explosions/Remote violence",
                           "Violence against civilians")) %>%
  count(event_type,sub_event_type) %>%
  tibble()

df_freq_sub_events_violent %>%

  ggplot(aes(x= reorder(sub_event_type,n), y= n))+
  geom_bar(stat='identity')+

  coord_flip()+
  labs(y= 'Number of events (sub events)',x="Violet sub-event type",
       title = "Event frequencies since 2018", subtitle = "ACLED: Somalia")+
  facet_wrap(~event_type,nrow=4,scales = "free_y")

df_acled_violent %>%
  View()



# filter to just violent events
df_acled_violent <- df_acled_short %>%
  filter(event_type %in% c("Battles",
                           "Riots",
                           "Violence against civilians")) %>%
  tibble()



# ADM1 2022 vs 2022 --------------------------------------------------------


# filter to just violent events
df_acled_violent <- df_acled_long %>%
  filter(event_type %in% c("Battles",
                           "Riots",
                           "Violence against civilians")) %>%
  tibble()

df_acled_violent %>%
  filter(year %in% c(2022,2023)) %>%
  group_by(year,admin1) %>%
  summarise(
    across(c("interaction","fatalities"),~sum(.x))
  ) %>%
  ggplot(
    aes(x= reorder(admin1,fatalities), y= fatalities, fill= as_factor(year))
  )+
  geom_bar(stat="identity",position="dodge")+
  coord_flip()+
  labs(
    title= "Somalia ACLED Data",
    subtitle = "Fatalities per admin 1 2022 vs 2023"
  )+

  theme(
    legend.title = element_blank(),
    axis.title.y = element_blank()
  )
df_acled_violent %>%
  filter(year %in% c(2022,2023)) %>%
  group_by(year,admin1) %>%
  summarise(
    across(c("interaction","fatalities"),~sum(.x))
  ) %>%
  select(-interaction) %>%
  pivot_wider(names_from = year, values_from = fatalities) %>%

  mutate(
    diff= `2023`-`2022`,
    diff_class= ifelse(diff<0,"neg","pos")
  ) %>%
  ggplot(
    aes(x= admin1, y= diff, fill=diff_class)
  )+
  geom_bar(stat="identity")+
  scale_y_continuous(breaks = seq(-600,700,50))+
  coord_flip()+
  scale_fill_manual(values = c(hdx_hex("sapphire-hdx"),hdx_hex("tomato-hdx")))+
  labs(
    title= "Somalia ACLED Data",
    subtitle = "Increase/Decrease Fatalities 2022-2023"
  )+
  theme(
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle=90),
    axis.title.x = element_blank()
  )





df_acled_long %>%
  filter(event_date>=as_date("2018-01-01")) %>%
  count(event_type)

df_acled_violent %>%
  count(event_type, sub_event_type) %>%
  tibble()
df_acled_violent_long %>%
  count(event_type, sub_event_type) %>%
  tibble()


# summarise figs at adm0 level
df_yr_mo_summary_adm0 <- df_acled_violent %>%
  tibble() %>%
  mutate(
    date = floor_date(as_date(event_date),"month")
  ) %>%
  group_by(date) %>%
  summarise(
    across(.cols = c("interaction","fatalities"),~sum(.x,na.rm=T))
  )
# summarise figs at adm0 level
df_yr_mo_summary_adm0_long <- df_acled_violent_long %>%
  tibble() %>%
  mutate(
    date = floor_date(as_date(event_date),"month")
  ) %>%
  group_by(date) %>%
  summarise(
    across(.cols = c("interaction","fatalities"),~sum(.x,na.rm=T)),.groups = "drop"
  ) %>%
  mutate(
    dataset= "ACLED"
  )

df_yr_mo_summary_adm0_long %>%
  bind_rows(
    df_upsala_yr_mo_summary_adm0
  ) %>%
  ggplot(
    aes(x=date, y= fatalities, color = dataset, group=dataset)
  ) +
  geom_point(alpha=0.2)+
  geom_line(alpha=0.7)+
  labs(title = "Monthly fatality records by data set",
       subtitle= "Somalia", y="Fatalities")+
  scale_x_date(date_breaks = "1 year",date_labels ="%Y")+
  scale_y_continuous_hdx()+
  theme(
    axis.text.x = element_text(angle=90),
    axis.title.x = element_blank(),
    legend.title = element_blank()
  )

# summarise figs at adm 1 level
df_yr_mo_summary_adm1_long <- df_acled_violent_long  %>%
  tibble() %>%
  mutate(
    date = floor_date(as_date(event_date),"month")
  ) %>%
  group_by(date,admin1) %>%
  summarise(
    across(.cols = c("interaction","fatalities"),~sum(.x,na.rm=T)),
    .groups="drop"
  )
df_yr_mo_summary_adm1 <- df_acled_violent  %>%
  tibble() %>%
  mutate(
    date = floor_date(as_date(event_date),"month")
  ) %>%
  group_by(date,admin1) %>%
  summarise(
    across(.cols = c("interaction","fatalities"),~sum(.x,na.rm=T)),
    .groups="drop"
  )


# country level fatalities/violent interactions/mo
df_yr_mo_summary_adm0 %>%
  pivot_longer(-date) %>%
  ggplot(aes(x= date, y= value, color = name, group=name))+
  geom_point()+
  geom_line()

# country level fatalities/violent interactions/mo
df_yr_mo_summary_adm0_long %>%
  pivot_longer(-date) %>%
  ggplot(aes(x= date, y= value, color = name, group=name))+
  geom_point()+
  geom_line()


# All admins - fatalities per month
df_yr_mo_summary_adm1 %>%
  ggplot(aes(x= date, y= fatalities, group=admin1))+
  scale_x_date(date_breaks = "1 month",date_labels ="%y-%b")+
  # geom_point(alpha=0.2)+
  geom_line(alpha=0.3)+
  theme(
    axis.text.x = element_text(angle=90)
  )
df_yr_mo_summary_adm1_long %>%
  ggplot(aes(x= date, y= fatalities, group=admin1))+
  scale_x_date(date_breaks = "3 month",date_labels ="%y-%b")+
  # geom_point(alpha=0.2)+
  geom_line(alpha=0.3)+
  labs(title = "Fatalities per month",subtitle = "Each line is an admin 1")+
  theme(
    axis.text.x = element_text(angle=90)
  )



# admins w/ greater than n (300) fatailies in a year
df_yr_mo_summary_adm1 %>%
  ungroup() %>%
  group_by(yr= year(date),admin1) %>%
  filter(
    sum(fatalities)>300
  ) %>%

  ggplot(aes(x= date, y= fatalities, color= admin1,group=admin1))+
  scale_x_date(date_breaks = "1 month",date_labels ="%y-%b")+
  geom_point()+
  geom_line()+
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    axis.text.x = element_text(angle=90)
  )

library(ggiraph)
library(glue)
p_acled_summary <- df_acled_violent %>%
  rename(
    date= "event_date"
  ) %>%
  # filter(admin1=="Galgaduud") %>%
  ggplot(
    aes(x=as_date(date), y= fatalities)
  )+
  geom_col(
    data= df_yr_mo_summary_adm1, #%>%
      # filter(admin1=="Galgaduud"),
    aes(x=date,
        y= fatalities
        ,
        tooltip=glue("date: {date}
                     fatalities: {fatalities}")
    ),
    fill="lightgrey",

    just=-.25,

    # I want it so there are no gaps between bars/cols
    # position = position_stack(reverse = TRUE),
    color="black"
  )+
  geom_point(aes(color=event_type
                 # ,
                 #             tooltip=glue("date: {date}
                 #                          fatalities: {fatalities}")
                             ),
             alpha=0.3)+
  scale_x_date(date_breaks = "1 month",date_labels = "%y-%b")+
  # scale_y_continuous(trans = scales::pseudo_log_trans())+
  # scale_y_log10()+
  facet_wrap(~admin1
             # ,scales = "free_y"
             )+
  theme(
    axis.text.x = element_text(angle=90)
  )
p_acled_summary
# ggiraph(ggobj = p_acled_summary)


df_acled_violent %>%
  rename(
    date= "event_date"
  ) %>%
  filter(admin1=="Galgaduud")

