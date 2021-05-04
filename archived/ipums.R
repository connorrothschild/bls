library(ipumsr)
library(tidyverse)
library(ggplot2)

ddi <- read_ipums_ddi(here::here("data/cps_00002.xml"))
data <- read_ipums_micro(ddi)

# Houston: 3362 https://cps.ipums.org/cps-action/variables/METAREA#codes_section

htx <- data %>% filter(METAREA == 3362)

htx

selected_ind <- 180

htx %>%
  filter(IND != 0) %>%
  filter(IND == selected_ind) %>%
  group_by(IND, YEAR, MONTH) %>%
  summarise(hours = mean(AHRSWORKT, na.rm = T)) %>%
  ggplot(aes(x = MONTH, y = hours, group = as.character(YEAR), color = as.character(YEAR))) +
  geom_line()

# Which individual has the greatest numbers of observations?
top <- htx %>%
  filter(UHRSWORKT != 999) %>%
  group_by(CPSIDP) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice(3) %>%
  pull(CPSIDP)

### The first six digits of CPSIDP index the four-digit year and two-digit month that the household was first in the CPS.
### CPSIDP allows users to link a respondent appearing with a designated household roster line number (LINENO) across samples,
### based on the 4-8-4 rotation pattern, by assigning a unique CPSIDP value to this line number.
### CPSIDP will only ever appear for a maximum of 8 times,
### which is the number of times a household may be observed in the CPS survey (as indexed by MISH).
# htx %>% filter(CPSIDP == top) %>% View()

htx %>%
  filter(CPSIDP == top) %>%
  ggplot(aes(x = MONTH, y = UHRSWORKT, group = as.character(YEAR), color = as.character(YEAR))) +
  geom_line()
