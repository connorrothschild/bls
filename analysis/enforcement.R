library(tidyverse)
library(plotly)

source(here::here(here::here('utils/theme.R')))
source(here::here(here::here('utils/functions.R')))

set_bls_theme(font = 'IBM Plex Sans')

source(here::here('prep/create_enforcement.R'))

max_year <- 2021
min_year <- 2015

## NUMBER OF INVESTIGATIONS PER ESTABLISHMENT
investigations <- houston_enforcement %>%
  filter(lubridate::year(findings_end_date) %in% c(min_year:max_year)) %>%
  count(industry_code, name = 'num') %>%
  right_join(lookup) %>%
  replace_na(list(num = 0))

houston_enforcement %>%
  group_by(year = lubridate::year(findings_end_date)) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = year, y = n)) +
  geom_rect(
    xmin = min_year,
    xmax = max_year,
    ymin = 0,
    ymax = 500,
    fill = '#cecece10'
  ) +
  annotate(
    'text',
    label = 'Our analysis',
    x = max_year - ((max_year - min_year) / 2),
    y = 450,
    hjust = 0.5,
    size = 5
  ) +
  geom_line() +
  drop_axis("y") +
  labs(
    x = element_blank(),
    y = 'Number of investigations',
    title = 'Investigations carried out by year',
    subtitle = 'In Houston, Texas'
  )

# houston_enforcement %>%
#   filter(lubridate::year(findings_end_date) %in% c(min_year:max_year))%>%
#   group_by(industry_title) %>%
#   summarise(sum = sum(bw_atp_amt)) %>%
#   ungroup() %>%
#   arrange(desc(sum)) %>% slice(1:20) %>%
#   mutate(industry_title = fct_reorder(industry_title, sum)) %>%
#   ggplot(aes(x = industry_title, y = sum)) +
#   geom_col() +
#   coord_flip() +
#   fix_bars(labels = scales::dollar_format()) +
#   labs(title = 'Highest violation industries',
#        subtitle = 'Measured by sum of backwages paid, min_year-max_year',
#        y = element_blank(),
#        x = element_blank())

houston_enforcement %>%
  filter(lubridate::year(findings_end_date) %in% c(min_year:max_year)) %>%
  group_by(industry_title) %>%
  summarise(sum = n()) %>%
  arrange(desc(sum)) %>% slice(1:20) %>%
  mutate(industry_title = fct_reorder(industry_title, sum)) %>%
  ggplot(aes(x = industry_title, y = sum)) +
  geom_col() +
  coord_flip() +
  fix_bars() +
  labs(
    title = 'Highest violation industries',
    subtitle = glue::glue('Measured by number of investigations, {min_year}-{max_year}'),
    y = 'Number of investigations',
    x = element_blank()
  )
