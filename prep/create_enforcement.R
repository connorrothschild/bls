library(tidyverse)
library(plotly)

source(here::here(here::here('utils/theme.R')))
source(here::here(here::here('utils/functions.R')))

set_bls_theme(font = 'IBM Plex Sans')

## Download from WHD Data catalog https://enforcedata.dol.gov/views/data_summary.php
enforcement_zip <- tempfile()
download.file('https://enfxfr.dol.gov/data_catalog/WHD/whd_whisard_20210415.csv.zip', enforcement_zip, mode = "wb")
unzip(enforcement_zip, exdir = 'data')
enforcement_raw <- readr::read_csv(here::here('data/whd_whisard.csv'))

## Download dictionary
dict_zip <- tempfile()
download.file('https://enfxfr.dol.gov/data_catalog/WHD/whd_data_dictionary_20210415.csv.zip', dict_zip, mode = "wb")
unzip(dict_zip, exdir = 'data')
dict <- readr::read_csv(here::here('data/whd_data_dictionary.csv'))

source(here::here('prep/create_industry_lookup.R'))

enforcement <- enforcement_raw %>%
  mutate(industry_code = substr(naic_cd, 1,4)) %>%
  filter(nchar(industry_code) == 4) %>%
  left_join(lookup, by = 'industry_code')

houston_enforcement <- enforcement %>%
  filter(cty_nm == 'Houston', st_cd == 'TX')

readr::write_csv(houston_enforcement, here::here('data/whd_houston.csv'))

max_year <- 2021
min_year <- 2015
