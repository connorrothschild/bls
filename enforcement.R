library(tidyverse)
library(plotly)
library(cr)
set_cr_theme(font = 'IBM Plex Sans')

max_year <- 2021
min_year <- 2015

source(here::here('prep/create_enforcement.R'))

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
#   cr::fix_bars(labels = scales::dollar_format()) +
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
  cr::fix_bars() +
  labs(
    title = 'Highest violation industries',
    subtitle = glue::glue('Measured by number of investigations, {min_year}-{max_year}'),
    y = 'Number of investigations',
    x = element_blank()
  )

if (!exists('q1_q2')) {
  source(here::here('prep/create_q1_q2.R'))
}

### VIOLATIONS VS EMPLOYMENT SIZE
q1_q2 %>%
  mutate(suspect = ifelse(industry_code %in% suspect$industry_code, "Suspect", 'Not suspect')) %>%
  # filter(suspect == 'Suspect') %>%
  left_join(investigations) %>%
  filter(complete.cases(qtrly_estabs_Q1),
         complete.cases(num)) %>%
  arrange(suspect) %>%
  ggplot(aes(
    x = qtrly_estabs_Q1,
    y = num,
    fill = 'blue',
    text = glue::glue(
      '<b style="font-size: 1rem;">{industry_title}</b>
                               Investigations: {num}
                               Establishments: {qtrly_estabs_Q1}
                               Rate: {round((num/qtrly_estabs_Q1)*1000, 3)} per 1,000 establishments'
    )
  )) +
  geom_point(size = 4, color = 'white', pch = 21, show.legend = FALSE) +
  # scale_alpha_manual(guide = 'none', values = c(0.5, 1)) +
  scale_x_continuous(labels = scales::comma_format(), limits = c(0,6000)) +
  labs(
    title = 'Frequency of WHD investigation, scaled to industry size',
    subtitle = 'Between 2015-2021, in Houston',
    x = 'Number of establishments',
    y = 'Number of investigations',
    fill = NULL
  ) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal')

g <- q1_q2 %>%
  mutate(suspect = ifelse(industry_code %in% suspect$industry_code, "Suspect", 'Not suspect')) %>%
  # filter(suspect == 'Suspect') %>%
  left_join(investigations) %>%
  filter(complete.cases(qtrly_estabs_Q1),
         complete.cases(num)) %>%
  arrange(suspect) %>%
  ggplot(aes(
    x = qtrly_estabs_Q1,
    y = num,
    fill = suspect,
    alpha = suspect,
    text = glue::glue(
      '<b style="font-size: 1rem;">{industry_title}</b>
                               Investigations: {num}
                               Establishments: {qtrly_estabs_Q1}
                               Rate: {round((num/qtrly_estabs_Q1)*1000, 3)} per 1,000 establishments'
    )
  )) +
  geom_point(size = 4, color = 'white', pch = 21) +
  scale_alpha_manual(guide = 'none', values = c(0.5, 1)) +
  scale_x_continuous(labels = scales::comma_format(), limits = c(0,6000)) +
  labs(
    title = 'Frequency of WHD investigation, scaled to industry size',
    subtitle = 'Between 2015-2021, in Houston',
    x = 'Number of establishments',
    y = 'Number of investigations',
    fill = NULL
  ) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal')

g

plotly::ggplotly(g, tooltip = 'text')

q1_q2 %>%
  mutate(suspect = ifelse(
    industry_code %in% suspect$industry_code,
    "Suspect",
    'Not suspect'
  )) %>%
  # filter(suspect == 'Suspect') %>%
  left_join(investigations) %>%
  filter(complete.cases(qtrly_estabs_Q1),
         complete.cases(num)) %>%
  arrange(suspect) %>%
  left_join(investigations) %>%
  # filter(qtrly_estabs_Q1 > 100) %>%
  mutate(inv_per_est = (num / qtrly_estabs_Q1 * 1000)) %>%
  group_by(suspect) %>%
  summarise(inv_per_est = mean(inv_per_est, na.rm = T)) %>%
  ggplot(aes(x = suspect, y = inv_per_est, fill = suspect)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_label(aes(
    label = paste0(round(inv_per_est, 1), " per 1,000")),
    hjust = 1.25,
    fill = 'whitesmoke'
  ) +
  fix_bars() +
  labs(x = element_blank(),
       y = element_blank(),
       title = 'WHD attention to suspect industries vs. non-suspect industries',
       # caption = 'Note: Only includes industries with more than 100 establishments',
       subtitle = 'Investigations per 1,000 establishments')

q1_q2 %>%
  mutate(suspect = ifelse(
    industry_code %in% suspect$industry_code,
    "Suspect",
    'Not suspect'
  )) %>%
  # filter(suspect == 'Suspect') %>%
  left_join(investigations) %>%
  filter(complete.cases(qtrly_estabs_Q1),
         complete.cases(num)) %>%
  arrange(suspect) %>%
  left_join(investigations) %>%
  # filter(qtrly_estabs_Q1 > 100) %>%
  mutate(inv_per_est = (num / qtrly_estabs_Q1 * 1000)) %>%
  filter(inv_per_est != 1000) %>%
  # group_by(suspect) %>%
  # summarise(inv_per_est = mean(inv_per_est)) %>%
  ggplot(aes(y = suspect, x = inv_per_est, color = suspect)) +
  geom_jitter(shape=16, position=position_jitter(0.3), alpha = .8, show.legend = FALSE) +
  labs(x = 'Investigations per 1,000 establishments',
       y = element_blank(),
       title = 'WHD attention to suspect industries vs. non-suspect industries',
       subtitle = 'Each point is an industry')

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
  labs(
    title = glue::glue('The most investigated industries, {min_year}-{max_year}'),
    subtitle = 'Only displaying industries with more than 100 establishments',
    x = element_blank(),
    y = 'Investigations per 1,000 establishments'
  )

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
  fix_bars(breaks = c(0, 1, 2)) +
  labs(
    title = glue::glue('The least investigated industries, {min_year}-{max_year}'),
    subtitle = 'Only displaying industries with more than 100 establishments',
    x = element_blank(),
    y = 'Investigations per 1,000 establishments'
  )
