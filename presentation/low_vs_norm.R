library(tidyverse)
library(plotly)

source(here::here(here::here('utils/theme.R')))
source(here::here(here::here('utils/functions.R')))

set_bls_theme()

source(here::here('analysis/impact.R'))
source(here::here('analysis/enforcement.R'))

outliers_removed <-
  wage_and_emp %>%
  filter(
    avg_emplvl_Q1 != 0,
    avg_emplvl_Q2 != 0,
    avg_wkly_wage_Q1 != 0,
    avg_wkly_wage_Q2 != 0,
    emp_diff != Inf,
    wage_diff != Inf,
    !is.na(emp_diff),
    !is.na(wage_diff),
    wage_diff != 1,
    wage_diff != -1,
    emp_diff != 1,
    emp_diff != -1,
    avg_emplvl_Q1 > 100
  )

quantile(outliers_removed$avg_wkly_wage_Q1, probs = seq(0, 1, .1))
quantile(outliers_removed$avg_wkly_wage_Q1)

outliers_removed %>%
  summarise(mean(emp_diff), mean(wage_diff))

## Here, we can define different thresholds for 'low-wage'
below_avg <- outliers_removed %>% filter(avg_wkly_wage_Q1 <= 600)
above_avg <- outliers_removed %>% filter(avg_wkly_wage_Q1 > 600)

avg_emp_shift <- (sum(outliers_removed$avg_emplvl_Q2) - sum(outliers_removed$avg_emplvl_Q1)) / sum(outliers_removed$avg_emplvl_Q2)
avg_wage_shift <- (sum(outliers_removed$avg_wkly_wage_Q2) - sum(outliers_removed$avg_wkly_wage_Q1))/ sum(outliers_removed$avg_wkly_wage_Q2)

below_emp_shift <-  (sum(below_avg$avg_emplvl_Q2) - sum(below_avg$avg_emplvl_Q1)) / sum(below_avg$avg_emplvl_Q2)
below_wage_shift <- (sum(below_avg$avg_wkly_wage_Q2) - sum(below_avg$avg_wkly_wage_Q1))/ sum(below_avg$avg_wkly_wage_Q2)

above_emp_shift <- (sum(above_avg$avg_emplvl_Q2) - sum(above_avg$avg_emplvl_Q1)) / sum(above_avg$avg_emplvl_Q2)
above_wage_shift <-(sum(above_avg$avg_wkly_wage_Q2) - sum(above_avg$avg_wkly_wage_Q1))/ sum(above_avg$avg_wkly_wage_Q2)

emps <- data.frame(avg_emp_shift, below_emp_shift) %>%
  pivot_longer(everything())

emps %>%
  ggplot(aes(x = name, y = value, fill = ifelse(name == 'avg_emp_shift', 'Yes', 'No'))) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = element_blank(),
       y = element_blank()) +
  drop_axis("x") +
  scale_x_discrete(breaks = c('below_emp_shift', 'avg_emp_shift', 'above_emp_shift'),
                   labels = c('Lower wage', 'Average', 'Above'))

wages <- data.frame(avg_wage_shift, below_wage_shift) %>%
  pivot_longer(everything())

wages %>%
  ggplot(aes(x = name, y = value, fill = ifelse(name == 'avg_wage_shift', 'Yes', 'No'))) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = element_blank(),
       y = element_blank()) +
  drop_axis("x") +
  scale_x_discrete(breaks = c('below_wage_shift', 'avg_wage_shift', 'above_wage_shift'),
                   labels = c('Lower wage', 'Average', 'Above'))

outliers_removed <- outliers_removed %>%
  mutate(wage_cat = ifelse(avg_wkly_wage_Q1 <= 1196.50, 'Q1 wages below mean', 'Q1 wages above mean'))

outliers_removed %>%
  group_by(wage_cat) %>%
  summarise(`Employment size` = mean(emp_diff), 'Wage' = mean(wage_diff)) %>%
  pivot_longer(cols = c(`Employment size`, Wage)) %>%
  ggplot(aes(x = wage_cat, y = value, group = name, fill = name)) +
  geom_col(position = 'dodge') +
  geom_hline(aes(yintercept = 0)) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = element_blank(),
       y = '% change',
       fill = element_blank(),
       title = 'Changes in employment and wages between Q1 and Q2 2020',
       subtitle = 'For workers in lower- and higher-wage industries',
       caption = '\nNote: Only includes industries that have greater than 100 employees in Houston') +
  theme(legend.position = 'top',
        legend.direction = 'horizontal')
