library(tidyverse)

library(cr)
set_cr_theme()

enforcement <- readr::read_csv("./data/whd_whisard.csv")

dict <- readr::read_csv("./data/whd_data_dictionary.csv")

dict

enforcement$naic_cd

enforcement %>%
  filter(cty_nm == 'Houston', st_cd == 'TX', lubridate::year(findings_end_date) %in% c(2015:2020))

enforcement %>%
  filter(cty_nm == 'Houston', st_cd == 'TX') %>%
  group_by(year = lubridate::year(findings_end_date)) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  drop_axis("y") +
  labs(x = element_blank(),
       y = 'Number of investigations',
       title = 'Investigations carried out by year',
       subtitle = 'In Houston, Texas')

enforcement %>%
  filter(cty_nm == 'Houston', st_cd == 'TX') %>%
  filter(lubridate::year(findings_end_date) %in% c(2015:2020)) %>%
  mutate(code = substr(naic_cd, 1,4)) %>%
  group_by(code) %>%
  summarise(sum = sum(bw_atp_amt)) %>%
  arrange(desc(sum)) %>% slice(1:20) %>%
  mutate(code = fct_reorder(code, sum)) %>%
  ggplot(aes(x = code, y = sum)) +
  geom_col() +
  coord_flip() +
  cr::fix_bars(labels = scales::dollar_format()) +
  labs(title = 'Highest violation industries',
       subtitle = 'Measured by sum of backwages paid, 2015-2020',
       y = element_blank(),
       x = element_blank())

enforcement %>%
  filter(cty_nm == 'Houston', st_cd == 'TX') %>%
  filter(lubridate::year(findings_end_date) %in% c(2015:2020)) %>%
  mutate(code = substr(naic_cd, 1,4)) %>%
  group_by(code) %>%
  summarise(sum = n()) %>%
  arrange(desc(sum)) %>% slice(1:20) %>%
  mutate(code = fct_reorder(code, sum)) %>%
  ggplot(aes(x = code, y = sum)) +
  geom_col() +
  coord_flip() +
  cr::fix_bars() +
  labs(title = 'Highest violation industries',
       subtitle = 'Measured by number of investigations, 2015-2020',
       y = element_blank(),
       x = element_blank())
