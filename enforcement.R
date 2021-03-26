library(tidyverse)

source('./prep/create_sector_lookup.R')
source('./emp_vs_wage.R')

library(plotly)
library(cr)
set_cr_theme(font = 'IBM Plex Sans')

enforcement_raw <- readr::read_csv("./data/whd_whisard.csv")

dict <- readr::read_csv("./data/whd_data_dictionary.csv")
lookup <- readr::read_csv("./data/industry-titles-csv.csv")

enforcement <- enforcement_raw %>%
  mutate(industry_code = substr(naic_cd, 1,4)) %>%
  filter(nchar(industry_code) == 4) %>%
  left_join(lookup, by = 'industry_code') %>%
  mutate(industry_title = str_replace_all(industry_title, "NAICS", ""),
         industry_title = str_replace_all(industry_title, '[[:digit:]]+', ""),
         industry_title = trimws(industry_title))

houston_enforcement <- enforcement %>%
  filter(cty_nm == 'Houston', st_cd == 'TX')

houston_enforcement %>%
  # filter(lubridate::year(findings_end_date) %in% c(2015:2020)) %>%
  group_by(year = lubridate::year(findings_end_date)) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  drop_axis("y") +
  labs(x = element_blank(),
       y = 'Number of investigations',
       title = 'Investigations carried out by year',
       subtitle = 'In Houston, Texas')

# houston_enforcement %>%
#   filter(lubridate::year(findings_end_date) %in% c(2015:2020))%>%
#   group_by(industry_title) %>%
#   summarise(sum = sum(bw_atp_amt)) %>%
#   ungroup() %>%
#   arrange(desc(sum)) %>% slice(1:20) %>%
#   mutate(industry_title = fct_reorder(industry_title, sum)) %>%
#   ggplot(aes(x = industry_title, y = sum)) +
#   geom_col() +
#   coord_flip() +
#   cr::fix_bars(labels = scales::dollar_format()) +
#   labs(title = 'Highest violation industries',
#        subtitle = 'Measured by sum of backwages paid, 2015-2020',
#        y = element_blank(),
#        x = element_blank())

houston_enforcement %>%
  filter(lubridate::year(findings_end_date) %in% c(2015:2020)) %>%
  group_by(industry_title) %>%
  summarise(sum = n()) %>%
  arrange(desc(sum)) %>% slice(1:20) %>%
  mutate(industry_title = fct_reorder(industry_title, sum)) %>%
  ggplot(aes(x = industry_title, y = sum)) +
  geom_col() +
  coord_flip() +
  cr::fix_bars() +
  labs(title = 'Highest violation industries',
       subtitle = 'Measured by number of investigations, 2015-2020',
       y = element_blank(),
       x = element_blank())

## NUMBER OF INVESTIGATIONS PER ESTABLISHMENT
investigations <- houston_enforcement %>%
  filter(lubridate::year(findings_end_date) %in% c(2015:2020)) %>%
  count(industry_code, name = 'num')

### VIOLATIONS VS EMPLOYMENT SIZE
g <- q1_q2 %>%
  left_join(investigations) %>%
  filter(complete.cases(qtrly_estabs_Q1),
         complete.cases(num)) %>%
  # filter(qtrly_estabs_Q1 > 300) %>%
  group_by(industry_code) %>%
  ggplot(aes(x = qtrly_estabs_Q1, y = num,
             # color = ifelse(industry_code %in% suspect$industry_code, 'Suspect', 'Regular'),
             text = glue::glue('<b style="font-size: 1rem;">{industry_title}</b>
                               Investigations: {num}
                               Establishments: {qtrly_estabs_Q1}
                               Rate: {round((num/qtrly_estabs_Q1)*1000, 3)} per 1,000 establishments'))) +
  geom_point() +
  labs(title = 'Establishments vs investigations',
       x = 'Number of establishments',
       y = 'Number of investigations',
       color = NULL) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal')

g

ggplotly(g, tooltip = 'text')

q1_q2 %>%
  left_join(investigations) %>%
  filter(complete.cases(qtrly_estabs_Q1), complete.cases(num)) %>%
  filter(qtrly_estabs_Q1 > 100) %>%
  mutate(inv_per_est = (num / qtrly_estabs_Q1 * 1000)) %>%
  arrange(desc(inv_per_est)) %>% slice(1:20) %>%
  mutate(industry_title = fct_reorder(industry_title, inv_per_est)) %>%
  select(industry_title, num, qtrly_estabs_Q1, inv_per_est) %>% # View()
  ggplot(aes(x = industry_title, y = inv_per_est)) +
  geom_col() +
  coord_flip() +
  fix_bars() +
  labs(title = 'The most investigated industries, 2015-2020',
       subtitle = 'Only displaying industries with more than 100 establishments',
       x = element_blank(),
       y = 'Investigations per 1,000 establishments')

q1_q2 %>%
  left_join(investigations) %>%
  filter(complete.cases(qtrly_estabs_Q1), complete.cases(num)) %>%
  filter(qtrly_estabs_Q1 > 100) %>%
  mutate(inv_per_est = (num / qtrly_estabs_Q1 * 1000)) %>%
  arrange(desc(inv_per_est)) %>% slice_tail(n = 20) %>%
  mutate(industry_title = fct_reorder(industry_title, inv_per_est)) %>%
  select(industry_title, num, qtrly_estabs_Q1, inv_per_est) %>% # View()
  ggplot(aes(x = industry_title, y = inv_per_est)) +
  geom_col() +
  coord_flip() +
  fix_bars() +
  labs(title = 'The least investigated industries, 2015-2020',
       subtitle = 'Only displaying industries with more than 100 establishments',
       x = element_blank(),
       y = 'Investigations per 1,000 establishments')
