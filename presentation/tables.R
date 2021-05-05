source(here::here('prep/create_qcew.R'))
source(here::here('prep/create_enforcement.R'))

library(reactable)

reactable_theme <- reactableTheme(
  borderColor = "#dfe2e5",
  stripedColor = "#f6f8fa",
  highlightColor = "#f0f5f9",
  cellPadding = "8px 12px",
  # headerStyle = list(textTransform = 'uppercase', letterSpacing = '1px', fontSize = '11px'),
  style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")
)

wage_and_emp %>%
  left_join(investigations) %>%
  arrange(desc(avg_emplvl_Q1)) %>%
  reactable(
    striped = TRUE,
    highlight = TRUE,
    bordered = TRUE,
    theme = reactable_theme,
    defaultColDef = colDef(format = colFormat(separators = TRUE, digits = 0)),
    columns = list(
    industry_code = colDef(show = FALSE, name = 'Industry Code', maxWidth = 100),
    industry_title = colDef(name = 'Industry', minWidth = 200),
    emp_diff = colDef(name = 'Employment change, Q1-Q2',
                      format = colFormat(percent = T, digits  = 1)),
    wage_diff = colDef(name = 'Weekly wage change, Q1-Q2',
                       format = colFormat(percent = T, digits = 1)),
    avg_emplvl_Q1 = colDef(name = 'Q1 Employment'),
    avg_emplvl_Q2 = colDef(name = 'Q2 Employment'),
    avg_wkly_wage_Q1 = colDef(name = 'Q1 Weekly Wage',
                              format = colFormat(prefix = "$", separators = TRUE)),
    avg_wkly_wage_Q2 = colDef(name = 'Q2 Weekly Wage',
                              format =  colFormat(prefix = "$", separators = TRUE)),
    num = colDef(name = 'Investigations, 2015-2020')
  ))

tbl <- suspect %>%
  left_join(investigations) %>%
  arrange(desc(avg_emplvl_Q2)) %>%
  # filter(avg_emplvl_Q1 > 300) %>%
  reactable(
    striped = TRUE,
    highlight = TRUE,
    bordered = TRUE,
    pagination = FALSE,
    theme = reactable_theme,
    defaultColDef = colDef(format = colFormat(separators = TRUE, digits = 0)),
    columns = list(
      industry_code = colDef(show = FALSE, name = 'Industry Code', maxWidth = 100),
      industry_title = colDef(name = 'Industry', minWidth = 200),
      emp_diff = colDef(name = 'Employment change, Q1-Q2',
                        format = colFormat(percent = T, digits  = 1)),
      wage_diff = colDef(name = 'Weekly wage change, Q1-Q2',
                         format = colFormat(percent = T, digits = 1)),
      avg_emplvl_Q1 = colDef(show = FALSE, name = 'Q1 Employment'),
      avg_emplvl_Q2 = colDef(name = 'Q2 Employment'),
      avg_wkly_wage_Q1 = colDef(show = FALSE, name = 'Q1 Weekly Wage',
                                format = colFormat(prefix = "$", separators = TRUE)),
      avg_wkly_wage_Q2 = colDef(name = 'Q2 Weekly Wage',
                                format =  colFormat(prefix = "$", separators = TRUE)),
      num = colDef(name = 'Investigations, 2015-2020')
    ))

tbl

htmlwidgets::saveWidget(widget = tbl, file = 'suspect.html')

q1_q2 %>%
  mutate(suspect = ifelse(industry_code %in% suspect$industry_code, "Suspect", 'Not suspect')) %>%
  left_join(investigations) %>%
  filter(complete.cases(qtrly_estabs_Q1),
         complete.cases(num)) %>%
  group_by(suspect) %>%
  summarise(mean_inv = mean(num),
            median_inv = median(num),
            mean_est = mean(qtrly_estabs_Q1),
            median_est = median(qtrly_estabs_Q1)) %>%
  reactable(theme = reactable_theme,
    defaultColDef = colDef(format = colFormat(separators = TRUE, digits = 0)),
    striped = TRUE,
    columns = list(
      suspect = colDef(name = '', minWidth = 120),
      mean_inv = colDef(name = 'Mean Investigations', minWidth = 150),
      median_inv = colDef(name = 'Median Investigations', minWidth = 150),
      mean_est = colDef(name = 'Mean Establishments', minWidth = 150),
      median_est = colDef(name = 'Median Establishments', minWidth = 150)
    ))


# SUMMARY STATISTICS TABLES
library(gtsummary)

replace_inf <- function(x) { if(is.infinite(x)) { return(0) } }

wage_and_emp %>%
  select(emp_diff:avg_wkly_wage_Q2) %>%
  mutate_if(is.numeric, list(~na_if(., Inf))) %>%
  mutate_if(is.numeric, list(~replace_na(., 0))) %>%
  gtsummary::tbl_summary(type = all_continuous() ~ "continuous2",
                         statistic = all_continuous() ~ c("{mean}",
                                                          "{median} ({p25} - {p75})",
                                                          "{min} - {max}"),
                         label = list(emp_diff ~ "Employment difference",
                                      wage_diff ~ "Wage difference",
                                      avg_emplvl_Q1 ~ "Q1 employment level",
                                      avg_emplvl_Q2 ~ "Q2 employment level",
                                      avg_wkly_wage_Q1 ~ "Q1 average weekly wage",
                                      avg_wkly_wage_Q2 ~ "Q2 average weekly wage")) %>%
  bold_labels() %>%
  as_gt(include = -cols_align)

investigations %>%
  select(num) %>%
  gtsummary::tbl_summary(type = all_continuous() ~ "continuous2",
                         statistic = all_continuous() ~ c("{mean}",
                                                          "{median} ({p25} - {p75})",
                                                          "{min} - {max}"),
                         label = list(num ~ "Number of investigations")) %>%
  bold_labels() %>%
  as_gt(include = -cols_align)

houston_enforcement %>%
  filter(lubridate::year(findings_end_date) %in% min_year:max_year) %>%
  select(case_violtn_cnt, bw_atp_amt, ee_atp_cnt) %>%
  gtsummary::tbl_summary(type = all_continuous() ~ "continuous2",
                         statistic = all_continuous() ~ c("{mean}",
                                                          "{median} ({p25} - {p75})",
                                                          "{min} - {max}"),
                         label = list(case_violtn_cnt ~ 'Total Case Violations',
                                      bw_atp_amt ~ "Total Backwages Agreed To Pay",
                                      ee_atp_cnt ~ 'Total Employees Agreed to Pay')) %>%
  bold_labels() %>%
  as_gt(include = -cols_align)
