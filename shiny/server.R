library(shiny)
library(shinydashboard)
library(tidyverse)

source(here::here(here::here('utils/theme.R')))
source(here::here(here::here('utils/functions.R')))

set_bls_theme()

q1 <- readr::read_csv(here::here("data/harris_q1_qcew.csv")) %>%
    filter(own_code == 5, ## Private only
           nchar(industry_code) == 4) ## 4 digit industry
q2 <- readr::read_csv(here::here("data/harris_q2_qcew.csv")) %>%
    filter(own_code == 5,
           nchar(industry_code) == 4)

q1_q2 <-
    left_join(q1, q2, by = 'industry_code', suffix = c('_Q1', '_Q2'))
lookup <-
    readr::read_csv(here::here("data/industry-titles.csv")) %>%
    filter(nchar(industry_code) == 4) %>%
    mutate(industry_title = str_replace(industry_title, 'NAICS ', ''))

joined <- left_join(q1_q2, lookup, by = "industry_code") %>%
    relocate(industry_title, .after = "industry_code") %>%
    mutate(
        industry_title = str_replace_all(industry_title, "NAICS", ""),
        industry_title = str_replace_all(industry_title, '[[:digit:]]+', ""),
        industry_title = trimws(industry_title)
    )

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    title_str <- reactive({
        ind <- lookup %>%
            filter(industry_code == input$code) %>%
            select(industry_title) %>%
            mutate(
                industry_title = str_replace_all(industry_title, '[0-9]+', ''),
                industry_title = trimws(industry_title),
                industry_title = str_to_lower(industry_title),
            ) %>%
            pull()

        return(ind)
    })

    ### EMPLOYMENT
    emp_df <- reactive({
        joined %>%
            filter(industry_code == input$code) %>%
            pivot_longer(
                cols = c(
                    'month1_emplvl_Q1',
                    'month2_emplvl_Q1',
                    'month3_emplvl_Q1',
                    'month1_emplvl_Q2',
                    'month2_emplvl_Q2',
                    'month3_emplvl_Q2'
                ),
                names_to = 'month',
                values_to = 'employment'
            ) %>%
            select(industry_code, industry_title, month, employment) %>%
            mutate(
                better_month = case_when(
                    month == 'month1_emplvl_Q1' ~ 1,
                    month == 'month2_emplvl_Q1' ~ 2,
                    month == 'month3_emplvl_Q1' ~ 3,
                    month == 'month1_emplvl_Q2' ~ 4,
                    month == 'month2_emplvl_Q2' ~ 5,
                    month == 'month3_emplvl_Q2' ~ 6
                )
            )
    })

    output$emp_plot <- renderPlot({
        ggplot(emp_df(),
               aes(
                   x = better_month,
                   y = employment,
                   group = industry_code
               )) +
            geom_point() +
            geom_line() +
            scale_x_discrete(labels = month.abb, limits = month.abb[1:6]) +
            scale_y_continuous(limits = c(0, NA),
                               labels = scales::comma_format()) +
            labs(
                title = paste0('Monthly employment in ', title_str(), ' in Houston'),
                x = element_blank(),
                y = 'Number of Employees'
            ) +
            theme(plot.title = element_text(size = 22))

    })

    emp_pct_change_val <-
        reactive({
            q1 <- emp_df() %>%
                filter(better_month == 1) %>%
                pull(employment)

            q2 <- emp_df() %>%
                filter(better_month == 6) %>%
                pull(employment)

            # percent change
            pct <- scales::percent((q2 - q1) / q1)

            return(pct)
        })

    output$emp_pct_change <- renderValueBox({
        valueBox(
            value = emp_pct_change_val(),
            subtitle = 'Percent Change in Employment (Q1-Q6)',
            icon = icon("briefcase", lib = "font-awesome"),
            color = "light-blue"
        )
    })

    ### WAGES
    wage_df <- reactive({
        joined %>%
            filter(industry_code == input$code) %>%
            pivot_longer(
                cols = c('avg_wkly_wage_Q1', 'avg_wkly_wage_Q2'),
                names_to = 'quarter',
                values_to = 'wages'
            ) %>%
            select(industry_code, industry_title, quarter, wages)
    })

    output$wage_plot <- renderPlot({
        ggplot(wage_df(),
               aes(
                   x = quarter,
                   y = wages,
                   group = industry_title
               )) +
            geom_col() +
            # geom_point() +
            # geom_line() +
            scale_x_discrete(labels = c('Q1', 'Q2')) +
            scale_y_continuous(
                labels = scales::dollar_format(),
                limits = c(0, NA),
                expand = c(0, .001)
            ) +
            labs(
                title = paste0(
                    'Shifts in average weekly wages in ',
                    title_str(),
                    ' during COVID-19'
                ),
                x = element_blank(),
                y = 'Average Weekly Wage'
            ) +
            theme(plot.title = element_text(size = 22))

    })

    wage_pct_change_val <-
        reactive({
            q1 <- wage_df() %>%
                filter(quarter == 'avg_wkly_wage_Q1') %>%
                pull(wages)

            q2 <- wage_df() %>%
                filter(quarter == 'avg_wkly_wage_Q2') %>%
                pull(wages)

            # percent change
            pct <- scales::percent((q2 - q1) / q1)

            return(pct)
        })

    output$wage_pct_change <- renderValueBox({
        valueBox(
            value = wage_pct_change_val(),
            subtitle = 'Percent Change in Wages',
            icon = icon("dollar-sign", lib = "font-awesome"),
            color = "light-blue"
        )
    })

    #### NUMBER OF EMPLOYEES
    emp_val <-
        reactive({
            val <- emp_df() %>%
                filter(better_month == 6) %>%
                pull(employment)

            return(scales::comma(val))
        })

    output$emp_count <- renderValueBox({
        valueBox(
            value = emp_val(),
            subtitle = 'Number of workers, June 2020',
            icon = icon("hashtag", lib = "font-awesome"),
            color = "light-blue"
        )
    })
})
