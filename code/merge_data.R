library(tidyverse)
library(lubridate)

cases <- read_csv("data/jh_covid19_data_ctry_level_long_format.csv", 
                  col_types = cols()) %>%
  mutate(date = ymd(date))

read_csv("data/npi_acaps.csv", col_types = cols()) %>%
  mutate(npi_date = ymd(date_implemented)) %>%
  rename (npi_type = category) %>%
  select(iso3c, npi_date, npi_type) -> npi

calc_npi_measure <-function(type, var_name) {
  my_npi <- npi %>% filter(npi_type == type)
  cases %>%
    left_join(
      my_npi %>%
        rename(date = npi_date) %>%
        mutate(npi = TRUE) %>%
        select(iso3c, date, npi) %>%
        group_by(iso3c, date) %>%
        summarise(npi = sum(npi)), 
      by = c("iso3c", "date")
    ) %>%
    group_by(iso3c) %>%
    mutate(
      npi = ifelse(is.na(npi), 0, npi),
      sum_npi = cumsum(npi)
    ) %>%
    ungroup() %>%
    select(iso3c, date, sum_npi) -> df
  
  names(df)[3] <- var_name
  df
}

cases %>%
  left_join(
    calc_npi_measure("Social distancing", "soc_dist"), 
    by = c("iso3c", "date")
  ) %>%
  left_join(
    calc_npi_measure("Movement restrictions", "mov_rest"), 
    by = c("iso3c", "date")
  ) %>%
  left_join(
    calc_npi_measure("Public health measures", "pub_health"), 
    by = c("iso3c", "date")
  ) %>%
  left_join(
    calc_npi_measure("Social and economic measures", "soc_econ"), 
    by = c("iso3c", "date")
  ) %>%
  left_join(
    calc_npi_measure("Lockdown", "lockdown"), 
    by = c("iso3c", "date")
  ) %>%
  left_join(
    read_csv("data/wbank_data.csv", col_types = cols()),
    by = "iso3c"
  )  -> raw_sample

write_csv(raw_sample, "data/merged_data.csv")
