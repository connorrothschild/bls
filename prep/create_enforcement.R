library(tidyverse)
library(plotly)
library(cr)
source(here::here('prep/create_industry_lookup.R'))

enforcement_raw <- readr::read_csv(here::here("data/whd_whisard.csv"))
dict <- readr::read_csv(here::here("data/whd_data_dictionary.csv"))

enforcement <- enforcement_raw %>%
  mutate(industry_code = substr(naic_cd, 1,4)) %>%
  filter(nchar(industry_code) == 4) %>%
  left_join(lookup, by = 'industry_code')

# enforcement %>% filter(is.na(industry_title)) %>% select(industry_code, industry_title) %>% View()

houston_enforcement <- enforcement %>%
  filter(cty_nm == 'Houston', st_cd == 'TX')

max_year <- 2021
min_year <- 2015

## NUMBER OF INVESTIGATIONS PER ESTABLISHMENT
investigations <- houston_enforcement %>%
  filter(lubridate::year(findings_end_date) %in% c(min_year:max_year)) %>%
  count(industry_code, name = 'num') %>%
  right_join(lookup) %>%
  replace_na(list(num = 0))
