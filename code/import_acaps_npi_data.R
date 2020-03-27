# https://www.acaps.org/projects/covid19

library(tidyverse)
library(rvest)
library(readxl)

url <- "https://data.humdata.org/dataset/acaps-covid19-government-measures-dataset"

selector_path <- paste0(
  "#data-resources-0 > div > ul > li > ",
  "div.hdx-btn-group.hdx-btn-group-fixed > ",
  "a.btn.btn-empty.btn-empty-blue.hdx-btn.resource-url-analytics.ga-download"
)

dta_url <- read_html(url) %>% 
  html_node(css = selector_path) %>% html_attr('href')

tmp_file <- tempfile(".xlsx")
download.file(paste0("https://data.humdata.org", dta_url), tmp_file)
raw_dta <- read_excel(tmp_file, sheet = "Database")

df <- raw_dta
names(df) <-tolower(names(df))
names(df)[16] <- "alternative_source"  

# Some spelling inconsistencies:
df$category[df$category == "Movement Restriction"] <- "Movement restrictions"
df$category[df$category == "Movement Restrictions"] <- "Movement restrictions"
df$category[df$category == "Social and Economic Measures"] <- "Social and economic measures"
df$category[df$category == "Social Distancing"] <- "Social distancing"

df %>% 
  select(-pcode) %>% # 2020-03-25 is all NA 
  filter(!is.na(date_implemented),
         !is.na(category)) %>%
  rename (iso3c = iso) -> npi_acaps

write_csv(npi_acaps, "data/npi_acaps.csv")
