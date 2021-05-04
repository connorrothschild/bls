library(tidyverse)
library(plotly)
library(cr)
source(here::here('prep/create_industry_lookup.R'))

q1 <- readr::read_csv("./data/harris_q1_qcew.csv") %>%
  filter(own_code == 5,
         nchar(industry_code) == 4) ## 4 digit industry
q2 <- readr::read_csv("./data/harris_q2_qcew.csv") %>%
  filter(own_code == 5,
         nchar(industry_code) == 4)

q1_q2_raw <- left_join(q1, q2, by = 'industry_code', suffix = c('_Q1', '_Q2'))

q1_q2 <- left_join(q1_q2_raw, lookup, by = "industry_code") %>%
  relocate(industry_title, .after = "industry_code")

wage_and_emp <- q1_q2 %>%
  mutate(avg_emplvl_Q1 = (month1_emplvl_Q1 + month2_emplvl_Q1 + month3_emplvl_Q1) / 3,
         avg_emplvl_Q2 = (month1_emplvl_Q2 + month2_emplvl_Q2 + month3_emplvl_Q2) / 3) %>%
  mutate(emp_diff = (avg_emplvl_Q2 - avg_emplvl_Q1) / avg_emplvl_Q1,
         wage_diff = (avg_wkly_wage_Q2 - avg_wkly_wage_Q1) / avg_wkly_wage_Q1) %>%
  select(industry_code, industry_title, emp_diff, wage_diff,
         avg_emplvl_Q1, avg_emplvl_Q2,
         avg_wkly_wage_Q1, avg_wkly_wage_Q2)
