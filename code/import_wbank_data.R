library(tidyverse)
library(wbstats)

pull_worldbank_data <- function(vars) {
  new_cache <- wbcache()
  all_vars <- as.character(unique(new_cache$indicators$indicatorID))
  data_wide <- wb(indicator = vars, mrv = 10, return_wide = TRUE)
  new_cache$indicators[new_cache$indicators[,"indicatorID"] %in% vars, ] %>%
    rename(var_name = indicatorID) %>%
    mutate(var_def = paste(indicator, "\nNote:",
                           indicatorDesc, "\nSource:", sourceOrg)) %>%
    select(var_name, var_def) -> wb_data_def
  new_cache$countries %>%
    select(iso3c, iso2c, country, region, income) -> ctries
  left_join(data_wide, ctries, by = "iso3c") %>%
    rename(year = date,
           iso2c = iso2c.y,
           country = country.y) %>%
    select(iso3c, iso2c, country, region, income, everything()) %>%
    select(-iso2c.x, -country.x) %>%
    filter(!is.na(NY.GDP.PCAP.KD),
           region != "Aggregates") -> wb_data
  wb_data$year <- as.numeric(wb_data$year)
  wb_data_def<- left_join(data.frame(var_name = names(wb_data),
                                     stringsAsFactors = FALSE),
                          wb_data_def, by = "var_name")
  wb_data_def$var_def[1:6] <- c(
    "Three letter ISO country code as used by World Bank",
    "Two letter ISO country code as used by World Bank",
    "Country name as used by World Bank",
    "World Bank regional country classification",
    "World Bank income group classification",
    "Calendar year of observation"
  )
  wb_data_def$type = c("cs_id", rep("factor",  4), "ts_id",
                       rep("numeric", ncol(wb_data) - 6))
  return(list(wb_data, wb_data_def))
}

# The below is just an example list of data items. See
# See https://data.worldbank.org
# for additional variables that you can pull

vars <- c("SP.POP.TOTL", "AG.LND.TOTL.K2", "EN.POP.DNST", "EN.URB.LCTY", "SP.DYN.LE00.IN", "NY.GDP.PCAP.KD")

wb_list <- pull_worldbank_data(vars)
wb_data <- wb_list[[1]]
wb_data_def <- wb_list[[2]]

wb_data %>%
  group_by(iso3c) %>%
  arrange(iso3c, year) %>%
  summarise(
    population = last(na.omit(SP.POP.TOTL)),
    land_area_skm = last(na.omit(AG.LND.TOTL.K2)),
    pop_density = last(na.omit(EN.POP.DNST)),
    pop_largest_city = last(na.omit(EN.URB.LCTY)),
    gdp_capita = last(na.omit(NY.GDP.PCAP.KD)),
    life_expectancy = last(na.omit(SP.DYN.LE00.IN))
  ) %>% left_join(wb_data %>% select(iso3c, region, income) %>% distinct()) -> wb_cs

write_csv(wb_cs, "data/wbank.csv")

