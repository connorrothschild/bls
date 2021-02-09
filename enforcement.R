enforcement <- readr::read_csv("./data/whd_whisard.csv")

dict <- readr::read_csv("./data/whd_data_dictionary.csv")

dict

enforcement$naic_cd

enforcement %>%
  filter(lubridate::year(findings_end_date) == 2019) %>%
  mutate(code = substr(naic_cd, 1,4)) %>%
  group_by(code) %>%
  summarise(sum = sum(bw_atp_amt)) %>%
  arrange(desc(sum)) %>% slice(1:20) %>%
  mutate(code = fct_reorder(code, sum)) %>%
  ggplot(aes(x = code, y = sum)) +
  geom_col() +
  coord_flip() +
  fix_bars(labels = scales::dollar_format()) +
  labs(title = 'Highest violation industries',
       subtitle = 'Measured by sum of backwages paid, in 2019',
       y = element_blank(),
       x = element_blank())

enforcement %>%
  filter(lubridate::year(findings_end_date) == 2019) %>%
  mutate(code = substr(naic_cd, 1,4)) %>%
  group_by(code) %>%
  summarise(sum = n()) %>%
  arrange(desc(sum)) %>% slice(1:12) %>%
  mutate(code = fct_reorder(code, sum)) %>%
  ggplot(aes(x = code, y = sum)) +
  geom_col() +
  coord_flip() +
  fix_bars() +
  labs(title = 'Highest violation industries',
       subtitle = 'Measured by number of violations, in 2019',
       y = element_blank(),
       x = element_blank())
