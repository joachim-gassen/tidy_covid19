library(tidyverse)
library(gtrendsR)
library(countrycode)

time <- ("2020-01-01 2020-03-27")
search_term <- "coronavirus"

trends_global <- gtrends(search_term, time = time)

trends_global$interest_by_country %>%
  filter(!is.na(hits)) %>%
  rename(gtrends_score =  hits) %>%
  mutate(iso3c = countrycode(location, origin = "country.name", 
                             destination = "iso3c"),
         iso2c = countrycode(location, origin = "country.name", 
                             destination = "iso2c")) %>%
  select(iso3c, iso2c, gtrends_score) -> gtrends_global


pull_gt_country_data <- function(iso2c) {
  message(
    sprintf("Pulling Google trend data for %s ...", iso2c), 
    appendLF = FALSE
  )
  gl <- gtrends(search_term, geo = iso2c, time = time)
  message("done!")
  
  # Be nice to Google and sleep a little
  
  Sys.sleep(runif(1, min = 2, max = 5))
  c(iso2c = iso2c, gl)
} 

fix_hits <- function(v) {
  if(is.numeric(v)) v 
  else {
    v[v == "<1"] <- 0.5
    as.numeric(v)
  }
}

parse_gt_list <- function(gtl) {
  gtl$interest_over_time %>%
    filter(hits != "NA") %>%
    mutate(
      gtrends_score = fix_hits(hits),
      iso2c = geo
    ) %>%
    select(iso2c, date, gtrends_score) -> gt_by_time
  if (is.data.frame(gtl$interest_by_region)) {
    gtl$interest_by_region %>%
      filter(hits != "NA") %>%
      mutate(
        gtrends_score = fix_hits(hits),
        iso2c = geo,
        region = location
      ) %>%
      select(iso2c, region, gtrends_score) -> gt_by_region
  } else gt_by_region <- NULL
  if (is.data.frame(gtl$interest_by_city)) {
    gtl$interest_by_city %>%
    filter(hits != "NA") %>%
    mutate(
      gtrends_score = fix_hits(hits),
      iso2c = geo,
      city = location
    ) %>%
    select(iso2c, city, gtrends_score) -> gt_by_city
  } else gt_by_city <- NULL
  list(gt_by_time, gt_by_region, gt_by_city)
}

extract_tibble_from_list <- function(lst, pos) {
  tibble_list <- lapply(lst, function(x) x[[pos]])
  tibble_list <- tibble_list[! sapply(tibble_list, is.null)]
  df <- do.call(rbind, tibble_list) %>%
    mutate(iso3c = countrycode(iso2c, origin = "iso2c", 
                               destination = "iso3c")) %>%
    select(iso3c, 2:3)
}

gt_ctry_lists <- lapply(gtrends_global$iso2c, pull_gt_country_data)
gt_parsed_list <- lapply(gt_ctry_lists, parse_gt_list)

gtrends_country_day <- extract_tibble_from_list(gt_parsed_list, 1)
gtrends_region <- extract_tibble_from_list(gt_parsed_list, 2)
gtrends_city <- extract_tibble_from_list(gt_parsed_list, 3)

write_csv(gtrends_global %>% select(-iso2c), "data/gtrends_country.csv") 
write_csv(gtrends_country_day, "data/gtrends_country_day.csv") 
write_csv(gtrends_region, "data/gtrends_region.csv") 
write_csv(gtrends_city, "data/gtrends_city.csv") 

