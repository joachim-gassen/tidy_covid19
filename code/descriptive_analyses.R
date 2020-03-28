library(tidyverse)
library(lubridate)
library(gghighlight)
library(ggrepel)


# Prior to sourcing this file you need to pull the data. 
# The quickest way to do that is to hit 'Build all' in the Build tab of RStudio.
# Or, at the Termninal, enter 'make all'

# --- Some visuals showing the spread of Covid-19 ------------------------------

merged <- read_csv("data/merged.csv", 
                col_types = cols()) %>%
  mutate(date = ymd(date))

merged %>% 
  group_by(iso3c) %>%
  filter(confirmed >= 100) %>%
  summarise(edate_confirmed = min(date)) -> edates_confirmed

merged %>% 
  group_by(iso3c) %>%
  filter(deaths >= 10) %>%
  summarise(edate_deaths = min(date)) -> edates_deaths

edates <- edates_confirmed  %>% full_join(edates_deaths, by = "iso3c")

merged %>% 
  left_join(edates, by = "iso3c") %>%
  mutate(
    edate_confirmed = as.numeric(date - edate_confirmed),
    edate_deaths = as.numeric(date - edate_deaths)
  ) %>%
  filter(!is.na(edate_confirmed) | !is.na(edate_deaths)) %>%
  mutate(
    confirmed_1e5pop = 1e5*confirmed/population,
    deaths_1e5pop = 1e5*deaths/population,
    recovered_1e5pop = 1e5*recovered/population
  ) -> df

df %>% 
  filter(edate_deaths == 0) %>%
  group_by(date) %>%
  summarise(
    count = n(),
    ctries = paste(iso3c, collapse = " ")
  ) %>%
  ggplot(aes(x = date, y = count)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = NA) + 
  geom_label_repel(aes(label = ctries), min.segment.length = unit(0, 'lines')) +
  labs(title = "Covid-19 Country-level Outbreaks over Time",
       x = "Date where reported deaths first reached 10",
       y = "Number of countries") + 
  theme_minimal()

df %>%
  filter(!is.na(edate_confirmed)) %>%
  filter(edate_confirmed >= 0) %>%
  group_by(country) %>%
  filter (n() >= 7) -> df_confirmed

df %>%
  filter(!is.na(edate_deaths)) %>%
  filter(edate_deaths >= 0) %>%
  group_by(country) %>%
  filter (n() >= 7) -> df_deaths


lab_notes <- paste0(
  "Data as provided by Johns Hopkins University Center for Systems Science ", 
  "and Engineering (JHU CSSE) and obtained on March 27, 2020.\n",
  "The sample is limited to countries with at least seven days of positve ", 
  "event days data. Code: https://github.com/joachim-gassem/tidy_covid19"
)

lab_x_axis_confirmed <- 
  "Days since confirmed cases reached 100 cases\n"

lab_x_axis_deaths <- 
  "Days since reported deaths reached 10\n"

gg_my_blob <- list(
  scale_y_continuous(trans='log10', labels = scales::comma),  
  theme_minimal(), 
  theme(
    plot.title.position = "plot", 
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1),
  ),
  labs(caption = lab_notes),
  gghighlight(TRUE,  label_key = country, use_direct_label = TRUE,
              label_params = list(segment.color = NA, nudge_x = 1))
)


gg_my_blob_confirmed <- c(gg_my_blob, 
                          list(labs(x = lab_x_axis_confirmed,
                                    y = "Confirmed cases (logarithmic scale)")))
gg_my_blob_deaths <- c(gg_my_blob, 
                       list(labs(x = lab_x_axis_deaths,
                                 y = "Redorted deaths (logarithmic scale)")))

ggplot(df_confirmed %>% filter (edate_confirmed <= 30), 
       aes(x = edate_confirmed, color = country, y = confirmed)) +
  geom_line() +
  labs(
    title = "Focus on the first month: Confirmed cases\n"
  ) +
  gg_my_blob_confirmed

ggplot(df_deaths %>% filter (edate_deaths <= 30), 
       aes(x = edate_deaths, color = country, y = deaths)) +
  geom_line() +
  labs(
    title = "Focus on the first month: Deaths\n"
  ) +
  gg_my_blob_deaths  

ggplot(df_deaths %>% 
         filter(
           recovered > 0,
           edate_deaths <= 30,
           max(recovered) >= 100
         ), 
       aes(x = edate_deaths, color = country, y = recovered)) +
  geom_line() +
  gg_my_blob_deaths +
  labs(
    title = "Remember: Recovered cases are increasing as well! Keep #FlattenTheCurve\n",
    y = "Recovered cases (logarithmic scale)"
  )

ggsave("media/recoveries.png", width = 8, height = 4, dpi = 160)

single_out_countries <- function(df, countries, var, relative = FALSE) {
  palette <-  function(n) {
    hues = seq(15, 375, length = n)
    c("#B0B0B0", hcl(h = hues, l = 65, c = 100)[1:n])
  }
  df %>%
    mutate(
      country = factor(
        ifelse(country %in% countries, country, "Other"),
        levels = c("Other", rev(countries))
      )
    ) %>%
    group_by(date, country) %>%
    summarise(val = sum(!! sym(var))) -> df
  
  if (relative) df <- df %>%
    group_by(date) %>%
    mutate(val = val/sum(val))
  
  df %>%
    arrange(date, country) %>%
    ggplot(aes(x = date, y = val, fill = country)) +
    geom_area() +
    scale_fill_manual(values = palette(length(countries))) +
    xlab("Calendar Date") +
    theme_minimal() -> p
  if (relative) p + scale_y_continuous(labels = scales::percent) else 
    p
}

single_out_countries(
  merged, c("China", "Iran", "Italy", "France", "Spain", "US"), 
  "deaths"
)

single_out_countries(
  merged, c("China", "Iran", "Italy", "France", "Spain", "US"), 
  "deaths", relative = TRUE
)


# --- Some descriptives for the implentation of NPIs over time -----------------

read_csv("data/acaps_npi.csv", col_types = cols()) %>%
  mutate(npi_date = ymd(date_implemented)) %>%
  rename(npi_type = category) %>%
  mutate(
    npi_regional = !is.na(admin_level_name),
    npi_targeted_pop_group = targeted_pop_group == "Yes",
    npi_lockdown = str_detect(measure, "General lockdown")
  ) %>%
  select(iso3c, npi_date, npi_type, npi_regional, 
         npi_targeted_pop_group, npi_lockdown) -> npi

merged %>% 
  group_by(iso3c) %>%
  filter(deaths >= 10) %>%
  summarise(edate = min(date)) -> ctry_edate

merged %>%
  select(iso3c, country) %>%
  unique() -> ctry_names

npi %>%
  left_join(ctry_edate, by = "iso3c") %>%
  filter(!is.na(edate)) %>%
  mutate(npi_edate = as.numeric(npi_date - edate)) %>%
  left_join(ctry_names, by = "iso3c") %>%
  select(iso3c, country, npi_date, npi_edate, npi_type, npi_lockdown) -> npi_edates

lab_x <- "Days relative to the date where reported deaths reached 10"

ggplot(npi_edates, aes(x = npi_edate, fill = npi_type)) + 
  geom_bar(position = "stack") + theme_minimal() +
  labs(title = "Implementation of Interventions over Time",
       x = lab_x,
       y = "Number of interventions")

p <- npi_edates %>%
  group_by(npi_edate, npi_type) %>%
  summarise(
    npi_count = n()
  ) %>%
  ungroup() %>%
  arrange(npi_type, npi_edate) %>%
  group_by(npi_type) %>%
  mutate(npi_count =  cumsum(npi_count)) %>%
  complete(npi_edate = min(npi_edates$npi_edate):max(npi_edates$npi_edate)) %>%
  fill(npi_count) %>% 
  replace_na(list(npi_count = 0)) %>%
  ggplot(aes(x = npi_edate, fill = npi_type, y = npi_count)) +
  theme_minimal() + labs(x = lab_x)

p + geom_area()
p + geom_area(position = "fill") + scale_y_continuous(labels = scales::percent)

df %>%
  group_by(date) %>%
  summarise(
    confirmed = sum(confirmed),
    deaths = sum(deaths),
    npi = sum(mov_rest + pub_health + soc_dist + soc_econ)
  ) %>%
  pivot_longer(2:4, names_to = "stat", values_to = "count") -> total

ggplot(total, aes(x = date, y = count, color = stat)) +
  geom_line() +  
  scale_y_continuous(trans='log10', labels = scales::comma) +
  theme_minimal()


merged %>%
  inner_join(ctry_edate, by = "iso3c") %>%
  mutate(edate = as.numeric(date - edate)) %>%
  group_by(iso3c) %>%
  mutate(
    lockdown_ctry = max(lockdown) > 0,
    soc_dist_ctry = max(soc_dist) 
  ) %>%
  ungroup() %>%
  mutate(soc_dist_ctry = soc_dist_ctry > median(soc_dist_ctry)) -> df

df %>%
  select(country, soc_dist_ctry, lockdown_ctry) %>%
  unique() %>%
  arrange(country) -> npi_ctry

ggplot(npi_ctry, aes(x = soc_dist_ctry, y = lockdown_ctry)) +
  geom_label_repel(aes(label = country)) +
  theme_minimal() +
  labs(
    x = "More than median amount of social distancing measures",
    y = "Lockdown initiated",
    caption = paste0(
      "Government intervention measures as provided by ",
      "Assessment Capacities Project (ACAPS). Data as of March 27, 2020.\n",
      "All countries with 10 or more reported deaths are included. ",
      "Code: https://github.com/joachim-gassem/tidy_covid19"
    )
  )

compare_death_growth <- function(df, var) {
  lab_caption <- paste0(
    "Deaths data as provided by Johns Hopkins University Center for Systems Science ", 
    "and Engineering (JHU CSSE).\nGovernment intervention measures as provided by ",
    "Assessment Capacities Project (ACAPS). Data as of March 27, 2020.\n",
    "At least five daily country-level observations required by group for ", 
    "estimation. Code: https://github.com/joachim-gassem/tidy_covid19"
  )
  lab_color <- case_when(
    var == "soc_dist_ctry" ~
      "More than median amount of\nsocial distancing measures",
    var == "lockdown_ctry" ~ "Lockdown initiated",
    TRUE ~ var
  )
  df %>%
    mutate(pct_inc_deaths = deaths/lag(deaths) - 1) %>%
    filter(edate >= 0) %>%
    group_by(edate, !! sym(var)) %>%
    filter(n() >= 5) %>%
    summarise(
      mean = mean(pct_inc_deaths),
      std_err = sd(pct_inc_deaths)/sqrt(n()),
      n = n()
    ) %>%
    ggplot(aes(x = edate, y = mean, color = !! sym(var))) +
    geom_pointrange(
      aes(ymin = mean-1.96*std_err, ymax = mean+1.96*std_err),
      position=position_dodge(0.4)
    ) + labs(
      x = lab_x,
      y = "Average daily percentage increase in reported deaths by group",
      caption = lab_caption,
      color = lab_color
    ) + 
    theme_minimal() + 
    theme(
      legend.position = c(0.75, 0.75),
      plot.title.position = "plot", 
      plot.caption.position =  "plot",
      plot.caption = element_text(hjust = 0),
      axis.title.x = element_text(hjust = 1),
      axis.title.y = element_text(hjust = 1),
    ) +
    scale_y_continuous(labels = scales::percent) 
}

compare_death_growth(df, "soc_dist_ctry")
compare_death_growth(df, "lockdown_ctry")

