library(tidyverse)
library(cr)

set_cr_theme(font = 'IBM Plex Sans')

q1 <- readr::read_csv("./data/harris_q1_qcew.csv") %>%
  filter(own_code == 5, ## Private only
         nchar(industry_code) == 4) ## 4 digit industry
q2 <- readr::read_csv("./data/harris_q2_qcew.csv") %>%
  filter(own_code == 5,
         nchar(industry_code) == 4)

q1_q2 <- left_join(q1, q2, by = 'industry_code', suffix = c('_Q1', '_Q2'))
lookup <- readr::read_csv("./data/industry-titles-csv.csv")

joined <- left_join(q1_q2, lookup, by = "industry_code") %>%
  relocate(industry_title, .after = "industry_code") %>%
  mutate(industry_title = str_replace_all(industry_title, "NAICS ", ""),
         industry_title = str_replace_all(industry_title, '[[:digit:]]+', ""),
         industry_title = trimws(industry_title))

wage_and_emp <- joined %>%
  mutate(avg_emplvl_Q1 = month1_emplvl_Q1 + month2_emplvl_Q1 + month3_emplvl_Q1 / 3,
         avg_emplvl_Q2 = month1_emplvl_Q2 + month2_emplvl_Q2 + month3_emplvl_Q2 / 3) %>%
  mutate(emp_diff = (avg_emplvl_Q2 - avg_emplvl_Q1) / avg_emplvl_Q1,
         wage_diff = (avg_wkly_wage_Q2 - avg_wkly_wage_Q1) / avg_wkly_wage_Q1) %>%
  select(industry_code, industry_title, emp_diff, wage_diff,
         avg_emplvl_Q1, avg_emplvl_Q2,
         avg_wkly_wage_Q1, avg_wkly_wage_Q2)

g <- wage_and_emp %>%
  ggplot() +
  geom_point(aes(x = emp_diff, y = wage_diff, color = emp_diff - wage_diff, text = industry_title), show.legend = FALSE) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  # scale_color_viridis_c(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  annotate(geom = 'label', x = -.5, -.5, label = 'Wages & employment decreased') +
  annotate(geom = 'label', x = .5, .5, label = 'Wages & employment increased') +
  annotate(geom = 'label', x = -.5, .5, label = 'Wages up, employment down') +
  annotate(geom = 'label', x = .5, -.5, label = 'Wages down, employment up') +
  labs(x = '% change in employment, Q1 to Q2',
       y = '% change in wages, Q1 to Q2',
       # color = 'Difference in differences',
       title = 'Industries arranged by wage and employment shifts, Q1 to Q2'
       ) +
  theme(plot.title.position = 'panel',
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.key.width = unit(1.5, 'cm'))

g

plotly::ggplotly(g, tooltip = 'text')

suspect <- wage_and_emp %>%
  filter(emp_diff > 0, wage_diff < 0)

### BEGIN DUMBBELL PLOT
library(ggalt)

blue <- "#0171CE"
red <- "#DE4433"

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
  labs(x = '\nWeekly wage',
       y = element_blank(),
       title = 'Difference between weekly wages in Q1 and Q2 2020',
       subtitle = "Among industries defined as suspect*",
       caption = "*Define suspect here.")

