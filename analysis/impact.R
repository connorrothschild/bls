library(tidyverse)
library(plotly)

source(here::here(here::here('utils/theme.R')))
source(here::here(here::here('utils/functions.R')))

set_bls_theme(font = 'IBM Plex Sans')

source(here::here('prep/create_qcew.R'))

g <- wage_and_emp %>%
  mutate(pretty_emp_level = scales::comma(avg_emplvl_Q1, accuracy = 1),
         pretty_emp_diff = scales::percent(emp_diff, accuracy = 1),
         pretty_wage_diff = scales::percent(wage_diff, accuracy = 1)) %>%
  ggplot() +
  geom_point(aes(x = emp_diff, y = wage_diff,
                 size = avg_emplvl_Q1, color = emp_diff - wage_diff,
                 text = glue::glue('<b style="font-size: 1rem;">{industry_title}</b>
                                 <b>Q1 employment</b>: {pretty_emp_level}
                                 <b>Q1-Q2 change in employment</b>: {pretty_emp_diff}
                                 <b>Q1-Q2 change in wages</b>: {pretty_wage_diff}')),
             alpha = 0.5,
             show.legend = FALSE) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  drop_axis() +
  labs(x = '% change in employment, Q1 to Q2',
       y = '% change in wages, Q1 to Q2',
       # color = 'Difference in differences',
       title = 'Industries arranged by wage and employment shifts, Q1 to Q2',
       subtitle = 'Each point is an industry; point size represents employment size') +
  theme(plot.title.position = 'plot',
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.key.width = unit(1.5, 'cm'))

g

g +
  geom_rect(aes(xmin=-1, xmax=0, ymin=-1, ymax=1), fill = "whitesmoke", alpha=.01) +
  geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1), fill = "whitesmoke", alpha=.01)

ggplotly(g, tooltip = 'text')

suspect <- wage_and_emp %>%
  filter(wage_diff < 0, emp_diff > 0)

### BEGIN SUSPECT SCATTERPLOT
g <- suspect %>%
  left_join(investigations) %>%
  mutate(pretty_emp_level = scales::comma(avg_emplvl_Q1, accuracy = 1),
         pretty_emp_diff = scales::percent(emp_diff, accuracy = 1),
         pretty_wage_diff = scales::percent(wage_diff, accuracy = 1)) %>%
  ggplot(aes(x = emp_diff, y = wage_diff, size = avg_emplvl_Q1,
             text = glue::glue('<b style="font-size: 1rem;">{industry_title}</b>
                                 <b>Q1 employment</b>: {pretty_emp_level}
                                 <b>Q1-Q2 change in employment</b>: {pretty_emp_diff}
                                 <b>Q1-Q2 change in wages</b>: {pretty_wage_diff}
                                 <b>Number of investigations: {num}'))) +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format())

ggplotly(g, tooltip = 'text')

## RAW COUNTS
# suspect %>%
#   ggplot(aes(x = avg_emplvl_Q2-avg_emplvl_Q1, y = avg_wkly_wage_Q2-avg_wkly_wage_Q1)) +
#   geom_point()

### BEGIN SUSPECT PLOTS
library(ggalt)

blue <- "#0171CE"
red <- "black"

dumbbell_data <- suspect %>%
  arrange(avg_wkly_wage_Q1) %>%
  slice(1:20) %>%
  mutate(industry_title = forcats::fct_reorder(industry_title, avg_wkly_wage_Q1))

dumbbell_data %>%
  ggplot() +
  geom_segment(
    aes(
      y = industry_title,
      yend = industry_title,
      x = min(avg_wkly_wage_Q1),
      xend = max(avg_wkly_wage_Q2) * 1.35
    ),
    color = "#b2b2b2",
    size = 0.15
  ) +
  geom_dumbbell(
    aes(y = industry_title, x = avg_wkly_wage_Q1, xend = avg_wkly_wage_Q2),
    size = 1.5,
    color = "#b2b2b2",
    size_x = 3,
    size_xend = 3,
    colour_x = red,
    colour_xend = blue
  ) +
  geom_text(data=(dumbbell_data %>% arrange(desc(avg_wkly_wage_Q1)) %>% head(1)),
            aes(x=avg_wkly_wage_Q1, y=industry_title, label="Q1"),
            color=red, size=3, vjust=-1.4, fontface="bold", family="IBM Plex Sans") +
  geom_text(data=(dumbbell_data %>% arrange(desc(avg_wkly_wage_Q1)) %>% head(1)),
            aes(x=avg_wkly_wage_Q2, y=industry_title, label="Q2"),
            color=blue, size=3, vjust=-1.4, fontface="bold", family="IBM Plex Sans")  +
  geom_rect(aes(xmin = max(avg_wkly_wage_Q1) * 1.15, xmax=max(avg_wkly_wage_Q1) * 1.35, ymin=-Inf, ymax=Inf), fill="whitesmoke") +
  geom_text(aes(label=scales::dollar(avg_wkly_wage_Q2-avg_wkly_wage_Q1), y=industry_title, x=max(avg_wkly_wage_Q1) * 1.25), size=3, family="IBM Plex Sans") +
  geom_text(data=(dumbbell_data %>% arrange(desc(avg_wkly_wage_Q1)) %>% head(1)),
            aes(x=max(dumbbell_data$avg_wkly_wage_Q1) * 1.25, y=industry_title, label="Difference"),
            color="black", size=3.1, vjust=-2, fontface="bold", family="IBM Plex Sans") +
  expand_limits(y = c(0, nrow(dumbbell_data) + 2)) +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme(axis.text.y = element_text(size = 8)) +
  drop_axis("y") +
  labs(x = '\nWeekly wage',
       y = element_blank(),
       title = 'Difference between weekly wages in Q1 and Q2 2020',
       subtitle = "Among suspect industries",
       caption = "\nNote: Suspect industries are classified as industries which saw a decline in wages and an increase in employment numbers between Q1 and Q2 2020.")

#### Changes in employment
#### Can't do dumbbell plot here because employment sizes are SO huge that differences are concealed (the x axis spans 0 to 100000)
emp_data <- suspect %>%
  # filter(avg_emplvl_Q1 > 100) %>%
  arrange(emp_diff) %>%
  distinct(industry_title, .keep_all = T) %>%
  slice(1:20) %>%
  mutate(industry_title = forcats::fct_reorder(industry_title, -emp_diff))

emp_data %>%
  ggplot(aes(x = emp_diff, y = industry_title)) +
  geom_col() +
  scale_x_continuous(expand = expansion(0, 0), labels = scales::percent_format(accuracy = 1), breaks = c(0, .05, .1)) +
  labs(y = element_blank(),
       x = '% Increase in employment',
       title = 'Difference between employment levels in Q1 and Q2 2020',
       subtitle = "Among suspect industries",
       caption = "\nNote: Suspect industries are classified as industries which saw a decline in wages and an increase in employment numbers between Q1 and Q2 2020.")

#### BEGIN AGGREGATE PLOTS
#### GREATEST LOSS IN WAGES
dumbbell_data <- wage_and_emp %>%
  filter(avg_emplvl_Q1 > 100) %>%
  mutate(raw_diff = avg_wkly_wage_Q2 - avg_wkly_wage_Q1) %>%
  arrange(raw_diff) %>%
  slice(1:20) %>%
  arrange(avg_wkly_wage_Q1) %>%
  mutate(industry_title = forcats::fct_reorder(industry_title, avg_wkly_wage_Q1))

dumbbell_data %>%
  ggplot() +
  geom_segment(
    aes(
      y = industry_title,
      yend = industry_title,
      x = min(avg_wkly_wage_Q1),
      xend = max(avg_wkly_wage_Q2) * 1.35
    ),
    color = "#b2b2b2",
    size = 0.15
  ) +
  geom_dumbbell(
    aes(y = industry_title, x = avg_wkly_wage_Q1, xend = avg_wkly_wage_Q2),
    size = 1.5,
    color = "#b2b2b2",
    size_x = 3,
    size_xend = 3,
    colour_x = red,
    colour_xend = blue
  ) +
  geom_text(data=(dumbbell_data %>% arrange(desc(avg_wkly_wage_Q1)) %>% head(1)),
            aes(x=avg_wkly_wage_Q1, y=industry_title, label="Q1"),
            color=red, size=3, vjust=-1.4, fontface="bold", family="IBM Plex Sans") +
  geom_text(data=(dumbbell_data %>% arrange(desc(avg_wkly_wage_Q1)) %>% head(1)),
            aes(x=avg_wkly_wage_Q2, y=industry_title, label="Q2"),
            color=blue, size=3, vjust=-1.4, fontface="bold", family="IBM Plex Sans")  +
  geom_rect(aes(xmin = max(avg_wkly_wage_Q1) * 1.15, xmax=max(avg_wkly_wage_Q1) * 1.35, ymin=-Inf, ymax=Inf), fill="whitesmoke") +
  geom_text(aes(label=scales::dollar(avg_wkly_wage_Q2-avg_wkly_wage_Q1), y=industry_title, x=max(avg_wkly_wage_Q1) * 1.25), size=3, family="IBM Plex Sans") +
  geom_text(data=(dumbbell_data %>% arrange(desc(avg_wkly_wage_Q1)) %>% head(1)),
            aes(x=max(dumbbell_data$avg_wkly_wage_Q1) * 1.25, y=industry_title, label="Difference"),
            color="black", size=3.1, vjust=-2, fontface="bold", family="IBM Plex Sans") +
  expand_limits(y = c(0, nrow(dumbbell_data) + 2)) +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme(axis.text.y = element_text(size = 8)) +
  drop_axis("y") +
  labs(x = '\nWeekly wage',
       y = element_blank(),
       title = 'Difference between weekly wages in Q1 and Q2 2020',
       subtitle = "Top 20 industries with the greatest (absolute) losses",
       caption = "\nNote: Only includes industries that have greater than 100 employees in Houston.")

#### GREATEST LOSS IN EMPLOYMENT
#### Can't do dumbbell plot here because employment sizes are SO huge that differences are concealed (the x axis spans 0 to 100000)
emp_loss_data <- wage_and_emp %>%
  filter(avg_emplvl_Q1 > 100,
         emp_diff != -1) %>%
  arrange(emp_diff) %>%
  distinct(industry_title, .keep_all = T) %>%
  slice(1:20) %>%
  mutate(industry_title = forcats::fct_reorder(industry_title, -emp_diff))

emp_loss_data %>%
  ggplot(aes(x = emp_diff, y = industry_title)) +
  geom_col() +
  scale_x_reverse(expand = expansion(0, 0.001), labels = scales::percent_format(accuracy = 1), breaks = c(0, -.25, -.50)) +
  # drop_axis('x') +
  labs(y = element_blank(),
       x = '\n% Decrease in employment',
       title = 'Difference between employment levels in Q1 and Q2 2020',
       subtitle = 'Top 20 industries with greatest (percentage) losses',
       caption = "\nNote: Only includes industries that have greater than 100 employees in Houston.")

