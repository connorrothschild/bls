library(tidyverse)

qcew <- readr::read_csv("./data/harris_q1_qcew.csv")
lookup <- readr::read_csv("./data/industry-titles.csv")

joined <- left_join(qcew, lookup, by = "industry_code") %>%
  relocate(industry_title, .after = "industry_code")

data <- joined %>%
  distinct(industry_code, .keep_all = TRUE) %>%
  # exclude any with dashes?
  filter(!str_detect(industry_code, "-")) %>%
  filter(nchar(industry_code) == 6) %>%
  mutate(
    two_digit = substr(industry_code, 1, 2),
    three_digit = substr(industry_code, 1, 3),
    four_digit = substr(industry_code, 1, 4),
    five_digit = substr(industry_code, 1, 6),
    six_digit = industry_code
  )

## For labels, grab two digit lookups, three digit lookups, etc.
two_dig <- joined %>%
  distinct(industry_code, .keep_all = TRUE) %>%
  filter(!str_detect(industry_code, "-")) %>%
  filter(nchar(industry_code) == 2) %>%
  select(industry_code, industry_title) %>%
  mutate(
    industry_title = str_replace_all(industry_title, "NAICS", ""),
    industry_title_and_code_two_dig = trimws(industry_title)
  ) %>%
  select(-industry_title)

three_dig <- joined %>%
  distinct(industry_code, .keep_all = TRUE) %>%
  filter(!str_detect(industry_code, "-")) %>%
  filter(nchar(industry_code) == 3) %>%
  select(industry_code, industry_title) %>%
  mutate(
    industry_title = str_replace_all(industry_title, "NAICS", ""),
    industry_title_and_code_three_dig = trimws(industry_title)
  ) %>%
  select(-industry_title)

four_dig <- joined %>%
  distinct(industry_code, .keep_all = TRUE) %>%
  filter(!str_detect(industry_code, "-")) %>%
  filter(nchar(industry_code) == 4) %>%
  select(industry_code, industry_title) %>%
  mutate(
    industry_title = str_replace_all(industry_title, "NAICS", ""),
    industry_title_and_code_four_dig = trimws(industry_title)
  ) %>%
  select(-industry_title)

five_dig <- joined %>%
  distinct(industry_code, .keep_all = TRUE) %>%
  filter(!str_detect(industry_code, "-")) %>%
  filter(nchar(industry_code) == 5) %>%
  select(industry_code, industry_title) %>%
  mutate(
    industry_title = str_replace_all(industry_title, "NAICS", ""),
    industry_title_and_code_five_dig = trimws(industry_title)
  ) %>%
  select(-industry_title)

six_dig <- joined %>%
  distinct(industry_code, .keep_all = TRUE) %>%
  filter(!str_detect(industry_code, "-")) %>%
  filter(nchar(industry_code) == 6) %>%
  select(industry_code, industry_title) %>%
  mutate(
    industry_title = str_replace_all(industry_title, "NAICS", ""),
    industry_title_and_code_six_dig = trimws(industry_title)
  ) %>%
  select(-industry_title)

# codes_lookup <- rbind(two_dig, three_dig, four_dig, five_dig, six_dig)
#
# codes_lookup_final <- codes_lookup %>%
#   mutate(industry_title = str_replace_all(industry_title, "\\d", ""),
#          industry_title = str_replace_all(industry_title, "NAICS", ""),
#          industry_title = trimws(industry_title))

treemap_df <- data %>%
  select(-industry_title) %>%
  left_join(two_dig, by = c('two_digit' = 'industry_code')) %>%
  left_join(three_dig, by = c('three_digit' = 'industry_code')) %>%
  left_join(four_dig, by = c('four_digit' = 'industry_code')) %>%
  left_join(five_dig, by = c('five_digit' = 'industry_code')) %>%
  left_join(six_dig, by = c('six_digit' = 'industry_code'))

# devtools::install_github("timelyportfolio/d3treeR")
library(d3treeR)
library(treemap)

p <- treemap(
  treemap_df,
  index = c(
    "industry_title_and_code_two_dig",
    "industry_title_and_code_three_dig",
    "industry_title_and_code_four_dig",
    "industry_title_and_code_five_dig",
    "industry_title_and_code_six_dig"
  ),
  vSize = "total_qtrly_wages",
  type = "index",
  palette = "Set2",
  bg.labels = c("white"),
  title = 'Total Wages in the First Quarter of 2020, by Industry',
  align.labels = list(c("center", "center"),
                      c("right", "bottom"))
)

inter <- d3tree2(p, rootname = "All")
# inter

htmlwidgets::saveWidget(inter, here::here("outputs/treemap.html"), title = 'Total Wages in the First Quarter of 2020, by Industry')
