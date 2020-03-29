library(tidyverse)
library(lubridate)
library(gghighlight)
library(ggrepel)


# Prior to sourcing this file you need to pull the data. 
# The quickest way to do that is to hit 'Build all' in the Build tab of RStudio.
# Or, at the Termninal, enter 'make all'

# --- Load and prep data -------------------------------------------------------

# The following relies on you being in an English locale
# Consider setting Sys.setlocale("LC_TIME", "C")
# if you are not

data_date_str <- format(as_date(file.info("data/merged.csv")$ctime), "%B %d, %Y")

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


# --- Some visuals showing the spread of Covid-19 ------------------------------

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


plot_covid19_spread <- function(df, type = "deaths", 
                                min_cases = ifelse(type == "deaths", 10, 100),
                                min_by_ctry_obs = 7,
                                edate_cutoff = 30,
                                per_capita = FALSE,
                                highlight = NULL,
                                intervention = NULL) {
  if(!type %in% c("confirmed", "deaths", "recovered")) 
    stop("Wrong 'type': Only 'confirmed', 'deaths', and 'recovered' are supported") 

  df %>% 
    group_by(iso3c) %>%
    filter(!! sym(type) >= min_cases) %>%
    mutate(edate = as.numeric(date - min(date))) %>%
    filter(!is.na(edate)) %>%
    group_by(country) %>%
    filter (n() >= min_by_ctry_obs) %>%
    ungroup() -> df
  
  if (per_capita) df <- df %>%
    mutate(!! type := 1e5*(!! sym(type))/population) %>% 
    filter(!is.na(!! sym(type)))

  if(!is.null(highlight) && !any(highlight %in% df$iso3c)) 
    stop(paste(
      "Non-NULL 'highlight' value but no countries matched in data", 
      "(Did you specify correct ISO3c codes?)"
    )) 
  
  if(!is.null(intervention) && ! intervention %in% names(df)) 
    stop(paste(
      "Non-NULL 'intervention' value but no variable present in data", 
      "(valid intervention types are 'lockdown', 'soc_dist', 'mov_rest',",
      "'pub_health', and 'soc_econ')."
    )) 
  
  
  caption_str <- paste(
    "Data as provided by Johns Hopkins University Center for Systems Science", 
    sprintf("and Engineering (JHU CSSE) and obtained on %s.", data_date_str),
    sprintf(
      "The sample is limited to countries with at least %d days of data.", 
      min_by_ctry_obs
    )
  )
  if (!is.null(intervention)) caption_str <- paste(
    caption_str,
    sprintf(
      "Dots indicate governmental interventions of type '%s'.", intervention 
    )
  )
  caption_str <- paste(strwrap(paste(
    caption_str, 
    "Code: https://github.com/joachim-gassem/tidy_covid19."
  ), width = 160), collapse = "\n")
  
  if (type == "deaths") {
    x_str <- sprintf("Days after %s reported death\n", 
                     scales::label_ordinal(big.mark = ",")(min_cases))
    if (per_capita)
      y_str <- "Reported deaths per 100,000 inhabitants (logarithmic scale)"
    else y_str <- "Reported deaths (logarithmic scale)"
    title_str <- sprintf("The First %d Days: Reported Deaths", edate_cutoff)
  }
  if (type == "confirmed") {
    x_str <- sprintf("Days after %s confirmed case\n", 
                     scales::label_ordinal(big.mark = ",")(min_cases))
    if (per_capita)
      y_str <- "Confirmed cases per 100,000 inhabitants (logarithmic scale)"
    else y_str <- "Confirmed cases (logarithmic scale)"
    title_str <- sprintf("The First %d Days: Confirmed Cases", edate_cutoff)
  }
  if (type == "recovered") {
    x_str <- sprintf("Days after %s recovered case\n", 
                     scales::label_ordinal(big.mark = ",")(min_cases))
    y_str <- "Recovered cases (logarithmic scale)"
    title_str <- sprintf("The First %d Days: Recovered Cases", edate_cutoff)
  }
  
  p <- ggplot(df %>% filter (edate <= edate_cutoff), 
         aes(x = edate, color = country, y = !! sym(type))) +
    geom_line() +
    labs(
      x = x_str,
      y = y_str,
      title = title_str,
      caption = caption_str
    ) +
    scale_y_continuous(trans='log10', labels = scales::comma) +  
    theme_minimal() +  
  theme(
    plot.title.position = "plot", 
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1),
  )
  
  if(!is.null(intervention)) p <- p + 
    geom_point(data = df %>% 
                 group_by(iso3c) %>% 
                 filter(!! sym(intervention) > lag(!! sym(intervention))))
  
  if(!is.null(highlight)) {
    p <- p + 
      gghighlight(iso3c %in% highlight,  
                  label_key = country, use_direct_label = TRUE,
                  label_params = list(segment.color = NA, nudge_x = 1),
                  use_group_by = FALSE)
  } else {
    p <- p + 
      gghighlight(TRUE,  
                  label_key = country, use_direct_label = TRUE,
                  label_params = list(segment.color = NA, nudge_x = 1))
  }
    
  p
}

plot_covid19_spread(merged, type = "confirmed", min_cases = 100)
plot_covid19_spread(merged, type = "confirmed", min_cases = 1000, per_capita = TRUE)
plot_covid19_spread(merged, type = "deaths")
plot_covid19_spread(merged, type = "deaths", per_capita = TRUE)
plot_covid19_spread(
  merged, edate_cutoff = 40, min_cases = 10, min_by_ctry_obs = 10,
  highlight = unique(merged$iso3c[merged$lockdown > 0]), 
  intervention = "lockdown"
)

plot_covid19_spread(merged, type = "recovered") + labs(
  title = "Remember: Recovered cases are increasing as well! Keep #FlattenTheCurve\n"
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
      sprintf("Assessment Capacities Project (ACAPS). Data as of %s.\n", data_date_str),
      "All countries with 10 or more reported deaths are included. ",
      "Code: https://github.com/joachim-gassem/tidy_covid19"
    )
  )

compare_death_growth <- function(df, var) {
  lab_caption <- paste0(
    "Deaths data as provided by Johns Hopkins University Center for Systems Science ", 
    "and Engineering (JHU CSSE).\nGovernment intervention measures as provided by ",
    sprintf("Assessment Capacities Project (ACAPS). Data as of %s.\n", data_date_str),
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


# --- Link of public attention (assessed by Google Trends) with NPIs -----------

merged %>%
  filter(gtrends_score == 100) %>%
  group_by(date) %>%
  summarise(
    count = n(),
    ctries = paste(iso3c, collapse = " ")
  ) %>%
  ggplot(aes(x = date, y = count)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = NA) + 
  geom_label_repel(aes(label = ctries), min.segment.length = unit(0, 'lines')) +
  labs(title = "Timing of Public Attention Spike",
       x = "Date where Google Trend reports highest hit score for 'coronavirus'",
       y = "Number of countries") + 
  theme_minimal()

ctry_level <- merged %>%
  inner_join(ctry_edate, by = "iso3c") %>%
  group_by(iso3c) %>%
  mutate(
    edate = as.numeric(edate - min(merged$date)),
    deaths = max(deaths),
    ln_deaths = log(deaths),
    confirmed = max(confirmed),
    ln_confirmed = log(confirmed),
    soc_dist = max(soc_dist),
    lockdown = max(lockdown) > 0,
    sum_npi = max(soc_dist + mov_rest + pub_health + soc_econ)
  ) %>%
  select(iso3c, country, edate, deaths, ln_deaths, confirmed, ln_confirmed,
         soc_dist, lockdown, sum_npi, 
         gdp_capita, pop_density, gtrends_country_score) %>%
  unique() %>%
  left_join(
    merged %>%
      inner_join(ctry_edate, by = "iso3c") %>%
      filter(gtrends_score == 100) %>%
      group_by(iso3c) %>%
      filter(row_number() == 1) %>%
      mutate(gtrends_spike_edate = as.numeric(date - edate)) %>%
      select(iso3c, gtrends_spike_edate),
    by = "iso3c"
  )

ggplot(ctry_level %>% filter(!is.na(gtrends_country_score)), 
       aes(x = deaths, y = gtrends_country_score)) +
  geom_point(aes(color = lockdown, size = soc_dist)) + 
  geom_smooth(method = "lm") + 
  theme_minimal() + 
  geom_label_repel(aes(group = NA, label = iso3c)) + labs(
    x = "Number of reported deaths (logarithmic scale)",
    y = "Public Attention on Covid-19" ,
    caption = paste0(
      "Data on deaths as provided by Johns Hopkins University Center for Systems Science ", 
      "and Engineering (JHU CSSE) \nGovernment intervention measures as provided by ",
      "Assessment Capacities Project (ACAPS). Public Attention\n",
      "as assessed by country rank on Google Trends, term 'coronavirus.'\n",
      sprintf("Data obtained on %s. ", data_date_str),
      "Code: https://github.com/joachim-gassem/tidy_covid19"
    ),
    color = "Lockdown intiated?",
    size = "Number of implemented\nsocial distancing measures"
  ) + 
  theme(
    plot.title.position = "plot", 
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1),
  ) +
  scale_x_continuous(trans='log10', labels = scales::comma)  

ggsave("media/pattention.png", width = 1.91*4, height = 4, dpi = 160)

