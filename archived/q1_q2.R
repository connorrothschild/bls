library(tidyverse)
library(plotly)

q1 <- readr::read_csv("./data/harris_q1_qcew.csv") %>%
  filter(own_code == 5, ## Private only
         nchar(industry_code) == 4) ## 4 digit industry
q2 <- readr::read_csv("./data/harris_q2_qcew.csv") %>%
  filter(own_code == 5,
         nchar(industry_code) == 4)

set_bls_theme()

q1_q2 <- left_join(q1, q2, by = 'industry_code', suffix = c('_Q1', '_Q2'))
lookup <- readr::read_csv("./data/industry-titles.csv")

joined <- left_join(q1_q2, lookup, by = "industry_code") %>%
  relocate(industry_title, .after = "industry_code") %>%
  mutate(industry_title = str_replace_all(industry_title, "NAICS ", ""),
         industry_title = str_replace_all(industry_title, '[[:digit:]]+', ""),
         industry_title = trimws(industry_title))

joined %>%
  pivot_longer(cols = c('month1_emplvl_Q1','month2_emplvl_Q1','month3_emplvl_Q1','month1_emplvl_Q2','month2_emplvl_Q2','month3_emplvl_Q2'),
               names_to = 'month',
               values_to = 'employment') %>%
  select(industry_code, industry_title, month, employment) %>%
  mutate(better_month = case_when(month == 'month1_emplvl_Q1' ~ 1,
                                  month == 'month2_emplvl_Q1' ~ 2,
                                  month == 'month3_emplvl_Q1' ~ 3,
                                  month == 'month1_emplvl_Q2' ~ 4,
                                  month == 'month2_emplvl_Q2' ~ 5,
                                  month == 'month3_emplvl_Q2' ~ 6)) %>%
  filter(industry_code == 4522) %>%
  # group_by(industry_code, better_month) %>%
  # summarise(employment = sum(employment)) %>%
  ggplot(aes(x = better_month, y = employment, group = industry_code)) +
  geom_point() +
  geom_line() +
  scale_x_discrete(labels = month.abb, limits = month.abb[1:6]) +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma_format()) +
  labs(title = 'Monthly employment in department stores in Houston',
       x = element_blank(),
       y = 'Number of Employees')

top_20_industries <- joined %>%
  pivot_longer(cols = c('month1_emplvl_Q1','month2_emplvl_Q1','month3_emplvl_Q1','month1_emplvl_Q2','month2_emplvl_Q2','month3_emplvl_Q2'),
               names_to = 'month',
               values_to = 'employment') %>%
  select(industry_code, industry_title, month, employment) %>%
  mutate(better_month = case_when(month == 'month1_emplvl_Q1' ~ 1,
                                  month == 'month2_emplvl_Q1' ~ 2,
                                  month == 'month3_emplvl_Q1' ~ 3,
                                  month == 'month1_emplvl_Q2' ~ 4,
                                  month == 'month2_emplvl_Q2' ~ 5,
                                  month == 'month3_emplvl_Q2' ~ 6)) %>%
  filter(better_month == 3) %>%
  arrange(desc(employment)) %>%
  slice(1:20) %>% select(industry_title) %>% pull()

joined %>%
  filter(industry_title %in% top_20_industries) %>%
  pivot_longer(cols = c('month1_emplvl_Q1','month2_emplvl_Q1','month3_emplvl_Q1','month1_emplvl_Q2','month2_emplvl_Q2','month3_emplvl_Q2'),
               names_to = 'month',
               values_to = 'employment') %>%
  select(industry_code, industry_title, month, employment) %>%
  mutate(better_month = case_when(month == 'month1_emplvl_Q1' ~ 1,
                                  month == 'month2_emplvl_Q1' ~ 2,
                                  month == 'month3_emplvl_Q1' ~ 3,
                                  month == 'month1_emplvl_Q2' ~ 4,
                                  month == 'month2_emplvl_Q2' ~ 5,
                                  month == 'month3_emplvl_Q2' ~ 6)) %>%
  ggplot(aes(x = better_month, y = employment, group = industry_title)) +
  geom_point() +
  geom_line() +
  scale_x_discrete(labels = month.abb, limits = month.abb[1:6]) +
  scale_y_continuous(labels = scales::comma_format(),
                     limits = c(0, NA)) +
  labs(title = 'Monthly employment shifts during COVID-19',
       subtitle = 'Top 20 industries, measured by March employment',
       x = element_blank(),
       y = 'Number of Employees') +
  facet_wrap(~ industry_title, scales = 'free') +
  theme(strip.text = element_text(size = 8),
        strip.background = element_blank())

joined %>%
  filter(industry_title %in% top_20_industries) %>%
  pivot_longer(cols = c('month1_emplvl_Q1','month2_emplvl_Q1','month3_emplvl_Q1','month1_emplvl_Q2','month2_emplvl_Q2','month3_emplvl_Q2'),
               names_to = 'month',
               values_to = 'employment') %>%
  select(industry_code, industry_title, month, employment) %>%
  mutate(better_month = case_when(month == 'month1_emplvl_Q1' ~ 1,
                                  month == 'month2_emplvl_Q1' ~ 2,
                                  month == 'month3_emplvl_Q1' ~ 3,
                                  month == 'month1_emplvl_Q2' ~ 4,
                                  month == 'month2_emplvl_Q2' ~ 5,
                                  month == 'month3_emplvl_Q2' ~ 6)) %>%
  ggplot(aes(x = better_month, y = employment, group = industry_title)) +
  geom_point() +
  geom_line() +
  scale_x_discrete(labels = month.abb, limits = month.abb[1:6]) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = 'Monthly employment shifts during COVID-19',
       subtitle = 'Top 20 industries, measured by March employment',
       x = element_blank(),
       y = 'Number of Employees') +
  facet_wrap(~ industry_title, scales = 'free') +
  theme(strip.text = element_text(size = 8),
        strip.background = element_blank())


joined %>%
  filter(industry_title %in% top_20_industries) %>%
  pivot_longer(cols = c('avg_wkly_wage_Q1','avg_wkly_wage_Q2'),
               names_to = 'quarter',
               values_to = 'wages') %>%
  select(industry_code, industry_title, quarter, wages) %>%
  ggplot(aes(x = quarter, y = wages, group = industry_title)) +
  geom_col() +
  # geom_point() +
  # geom_line() +
  scale_x_discrete(labels = c('Q1', 'Q2')) +
  scale_y_continuous(labels = scales::dollar_format(),
                     limits = c(0, NA),
                     expand = c(0, .001)) +
  labs(title = 'Shifts in average weekly wages during COVID-19',
       subtitle = 'Top 20 industries, measured by March employment',
       x = element_blank(),
       y = 'Average Weekly Wage') +
  facet_wrap(~ industry_title, scales = 'free') +
  theme(strip.text = element_text(size = 8),
        strip.background = element_blank())
