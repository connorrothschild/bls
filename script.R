library(tidyverse)

qcew <- readr::read_csv("./data/harris_q1_4digit_qcew.csv")
lookup <- readr::read_csv("./data/industry-titles-csv.csv")

joined <- left_join(qcew, lookup, by = "industry_code") %>% 
  relocate(industry_title, .after = "industry_code")

df <- joined %>% filter(nchar(industry_code) == 4)

df %>% 
  # select(industry_title, avg_wkly_wage) %>% 
  arrange(avg_wkly_wage) %>% 
  # Those with a weekly average wage of 0 are likely bad data
  filter(avg_wkly_wage > 0) %>% 
  mutate(avg_hourly_wage = avg_wkly_wage / 40, .after = avg_wkly_wage) %>% View()
