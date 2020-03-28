library(tidyverse)
library(lubridate)
library(rvest)
library(countrycode)

clean_jhd_to_long <- function(df) {
  df_str <- deparse(substitute(df))
  var_str <- substr(df_str, 1, str_length(df_str) - 4)
  
  df %>% 
    select(-`Province/State`, -Lat, -Long) %>%
    rename(country = `Country/Region`) %>%
    mutate(iso3c = countrycode(country,
                               origin = "country.name",
                               destination = "iso3c")) %>%
    select(-country) %>%
    filter(!is.na(iso3c)) %>%
    group_by(iso3c) %>%
    summarise_at(vars(-group_cols()), sum) %>% 
    pivot_longer(
      -iso3c, 
      names_to = "date_str", 
      values_to = var_str
    ) %>%
    ungroup() %>%
    mutate(date = mdy(date_str)) %>%
    select(iso3c, date, !! sym(var_str)) 
}

confirmed_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", col_types = cols())
deaths_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", col_types = cols())
recovered_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", col_types = cols())

jh_covid19_data <- clean_jhd_to_long(confirmed_raw) %>%
  full_join(clean_jhd_to_long(deaths_raw), by = c("iso3c", "date")) %>%
  full_join(clean_jhd_to_long(recovered_raw), by = c("iso3c", "date"))

jhd_countries <- tibble(
  country = unique(confirmed_raw$`Country/Region`),
  iso3c = countrycode(country,
                      origin = "country.name",
                      destination = "iso3c")
) %>% filter(!is.na(iso3c))

old_jhd_countries <- tibble(
  country = unique(recovered_raw$`Country/Region`),
  iso3c = countrycode(country,
                      origin = "country.name",
                      destination = "iso3c")
) %>% filter(!is.na(iso3c),
             ! iso3c %in% jhd_countries$iso3c)

jhd_countries <- rbind(jhd_countries, old_jhd_countries)

jh_covid19_data %>%
  left_join(jhd_countries, by = "iso3c") %>%
  select(country, iso3c, date, confirmed, deaths, recovered) -> jh_covid19_data

write_csv(jh_covid19_data, "data/jh_covid19_ctry_level.csv")
