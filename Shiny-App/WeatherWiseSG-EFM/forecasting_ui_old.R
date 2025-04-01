# forecasting_ui.R

library(shiny)
library(bs4Dash)

forecasting_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 3,
      bs4Card(
        title = "Univariate",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        selectInput(ns("variable"), "Variable:", choices = NULL),
        selectInput(ns("station"), "Station:", choices = NULL),
        numericInput(ns("horizon"), "Forecast Horizon:", value = 12, min = 1),
        selectInput(ns("model"), "Model:", choices = c("ARIMA", "ETS", "Prophet")),
        radioButtons(ns("time_view"), "View Data By:", choices = c("Day", "Week", "Month"), inline = TRUE),
        radioButtons(ns("time_period"), "Time Period:", choices = c("Custom", "All (2018-2024)"), inline = TRUE),
        dateInput(ns("start_date"), "Start Date:"),
        dateInput(ns("end_date"), "End Date:")
      )
    ),
    
    column(
      width = 9,
      bs4Card(
        title = "Time-Series Forecasting",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        closable = FALSE,
        collapsible = TRUE,
        tabPanel(
          title = "Decomposition",
          fluidRow(
            # First Row: Area 1 and Area 2
            column(8,
                   bs4Card(
                     title = "(1) Main Time Series Plot",
                     width = 10,
                     status = "info",
                     plotOutput(ns("time_series_plot"))
                   )
            ),
            column(4,
                   bs4Card(
                     title = "(2) Dickey-Fuller Test",
                     width = 2,
                     status = "info",
                     verbatimTextOutput(ns("adf_test_output"))
                   )
            )
          ),
          fluidRow(
            # Second Row: Area 3 and Area 4
            column(6,
                   bs4Card(
                     title = "(3) ACF & PACF Plots",
                     width = 5,
                     status = "info",
                     plotOutput(ns("acf_plot")),
                     plotOutput(ns("pacf_plot"))
                   )
            ),
            column(6,
                   bs4Card(
                     title = "(4) STL Decomposition",
                     width = 7,
                     status = "info",
                     plotOutput(ns("stl_plot"))
                   )
            )
          )
        )
      )
    )
  )
}
