library(shiny)
library(shinydashboard)
library(tidyverse)

lookup <-
    readr::read_csv(here::here("data/industry-titles.csv")) %>%
    filter(nchar(industry_code) == 4) %>%
    pull(industry_code)

names(lookup) <-
    readr::read_csv(here::here("data/industry-titles.csv")) %>%
    filter(nchar(industry_code) == 4) %>%
    mutate(industry_title = str_replace(industry_title, 'NAICS ', '')) %>%
    pull(industry_title)

# Define UI for application that draws a histogram
dashboardPage(
    dashboardHeader(title = "COVID-19 Industry Exploration Tool", titleWidth = 400),

    # Sidebar with a slider input for number of bins
    dashboardSidebar(
        width = 400,
        selectizeInput("code",
                       "Industry",
                       choices = lookup),
        valueBoxOutput('emp_count', width = 12),
        valueBoxOutput('emp_pct_change', width = 12),
        valueBoxOutput('wage_pct_change', width = 12)
    ),

    # Show a plot of the generated distribution
    dashboardBody(fluidRow(
        plotOutput("emp_plot"),
        plotOutput("wage_plot")
    ))
)
