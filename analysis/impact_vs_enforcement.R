library(tidyverse)
library(plotly)

source(here::here(here::here('utils/theme.R')))
source(here::here(here::here('utils/functions.R')))

set_bls_theme()

source(here::here('analysis/impact.R'))
source(here::here('analysis/enforcement.R'))

### Violations vs number of establishments
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

# Least investigated
q1_q2 %>%
  left_join(investigations) %>%
  filter(complete.cases(qtrly_estabs_Q1), complete.cases(num)) %>%
  mutate(inv_per_est = (num / qtrly_estabs_Q1 * 1000)) %>%
  filter(inv_per_est != 0) %>%
  arrange(desc(inv_per_est)) %>%
  slice_tail(n = 20) %>%
  mutate(industry_title = fct_reorder(industry_title, inv_per_est)) %>%
  select(industry_title, num, qtrly_estabs_Q1, inv_per_est) %>% # View()
  ggplot(aes(x = industry_title, y = inv_per_est)) +
  geom_col() +
  coord_flip() +
  fix_bars() +
  labs(
    title = glue::glue('The least investigated industries, {min_year}-{max_year}'),
    subtitle = 'Only displaying industries with more than 100 establishments',
    x = element_blank(),
    y = 'Investigations per 1,000 establishments'
  )

# Scatterplot view
q1_q2 %>%
  mutate(suspect = ifelse(industry_code %in% suspect$industry_code, "Suspect", 'Not suspect')) %>%
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
  scale_fill_manual(values = bls_blue) +
  scale_x_continuous(labels = scales::comma_format(), limits = c(0,6000)) +
  labs(
    title = 'Frequency of WHD investigation, scaled to industry size',
    subtitle = glue::glue('Between {min_year}-{max_year}, in Houston'),
    x = 'Number of establishments',
    y = 'Number of investigations',
    fill = NULL
  ) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal')

## Highlight suspect industries
g <- q1_q2 %>%
  mutate(suspect = ifelse(industry_code %in% suspect$industry_code, "Suspect", 'Not suspect')) %>%
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
  scale_fill_manual(values = c(bls_blue, '#FFA400')) +
  scale_x_continuous(labels = scales::comma_format(), limits = c(0,6000)) +
  labs(
    title = 'Frequency of WHD investigation, scaled to industry size',
    subtitle = glue::glue('Between {min_year}-{max_year}, in Houston'),
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
  left_join(investigations) %>%
  filter(complete.cases(qtrly_estabs_Q1),
         complete.cases(num)) %>%
  arrange(suspect) %>%
  left_join(investigations) %>%
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
  scale_fill_manual(values = c(bls_blue, '#FFA400')) +
  labs(x = element_blank(),
       y = element_blank(),
       title = 'WHD attention to suspect industries vs. non-suspect industries',
       subtitle = 'Investigations per 1,000 establishments')

q1_q2 %>%
  mutate(suspect = ifelse(
    industry_code %in% suspect$industry_code,
    "Suspect",
    'Not suspect'
  )) %>%
  left_join(investigations) %>%
  filter(complete.cases(qtrly_estabs_Q1),
         complete.cases(num)) %>%
  arrange(suspect) %>%
  left_join(investigations) %>%
  mutate(inv_per_est = (num / qtrly_estabs_Q1 * 1000)) %>%
  filter(inv_per_est != 1000) %>%
  ggplot(aes(y = suspect, x = inv_per_est, color = suspect)) +
  geom_jitter(shape=16, position=position_jitter(0.3), alpha = .8, show.legend = FALSE) +
  scale_color_manual(values = c(bls_blue, '#FFA400')) +
  labs(x = 'Investigations per 1,000 establishments',
       y = element_blank(),
       title = 'WHD attention to suspect industries vs. non-suspect industries',
       subtitle = 'Each point is an industry')

