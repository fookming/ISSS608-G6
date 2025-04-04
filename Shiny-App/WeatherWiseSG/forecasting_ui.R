# forecasting_ui.R
library(shiny)
library(bs4Dash)
library(shinyjs)  # clearly add this line explicitly
library(forecast)
library(memoise)
library(cachem)
library(tseries)
library(plotly)
library(prophet)
library(ggplot2)
library(lubridate)




forecasting_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Left-side Univariate Panel
    column(
      width = 3,
      bs4Card(
        title = "UNIVARIATE",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        selectInput(ns("variable"), "Variable:", choices = NULL),
        selectInput(ns("station"), "Station:", choices = NULL),
        numericInput(ns("forecast_horizon"), "Forecast Horizon:", value = 12, min = 1),
        selectInput(ns("model"), "Model:", choices = c("ARIMA", "ETS", "Prophet"),selected = "Prophet"),
        radioButtons(ns("time_view"), "View Data By:", choices = c("Day", "Week", "Month"), inline = TRUE, selected="Month"),
        radioButtons(ns("time_period"), "Time Period:", choices = c("Custom", "All (2018-2024)"), inline = TRUE),
        dateInput(ns("start_date"), "Start Date:", value = "2018-01-01"),
        dateInput(ns("end_date"), "End Date:", value = "2024-12-31")
      )
    ),
    
    # Right-side Main Content Panel with tabs
    column(
      width = 9,
      bs4Card(
        title = "TIME-SERIES FORECASTING",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        closable = FALSE,
        collapsible = FALSE,
        maximizable = FALSE,
        tabsetPanel(
          id = ns("main_forecast_tabs"),
          tabPanel(
            title = "DECOMPOSITION",
            fluidRow(
              # Row 1: Area 1 and Area 2
              column(
                width = 8,
                bs4Card(
                  title = "(1) Main Time Series Plot",
                  width = 12,
                  status = "info",
                  plotOutput(ns("time_series_plot"))
                )
              ),
              column(
                width = 4,
                bs4Card(
                  title = "(2) Dickey-Fuller Test",
                  width = 12,
                  status = "info",
                  verbatimTextOutput(ns("adf_test_result"))
                )
              )
            ),
            fluidRow(
              # Row 2: Area 3 and Area 4
              column(
                width = 5,  # ~40%
                bs4Card(
                  title = "(3) ACF & PACF Plots",
                  width = 12,
                  status = "info",
                  plotOutput(ns("acf_plot")),
                  plotOutput(ns("pacf_plot"))
                )
              ),
              column(
                width = 7,  # ~60%
                bs4Card(
                  title = "(4) STL Decomposition",
                  width = 12,
                  status = "info",
                  plotOutput(ns("stl_plot"))
                ),
                
                # ARIMA orders in separate neat box
                bs4Card(
                  title = "Suggested ARIMA Orders",
                  width = 12,
                  solidHeader = TRUE,
                  status = "success",
                  
                  actionButton(ns("calc_arima"), "Calculate ARIMA Order"),
                  br(), br(),
                  verbatimTextOutput(ns("arima_order"))
                )
                
              )
            )
          ),
          tabPanel(
            title = "FORECASTING",
            fluidRow(
              column(
                width = 7,
                bs4Card(
                  title = "Static Forecast",
                  width = 12,
                  height = "250px",  # approx. 20%
                  status = "primary",
                  plotOutput(ns("static_forecast_plot"), height = "200px")
                )
              ), # column
              column(width = 5,
                     bs4Card(
                       title = "Evaluation Metrics (ARIMA/SARIMA, Prophet, ETS)",
                       width = 12,
                       height = "250px",  # approx. 20%
                       status = "info",
                       actionButton(ns("evaluate_forecast"), "Evaluate Forecast Models"),
                       tableOutput(ns("forecast_eval_table"))
                     )
              )
              
            ), # fluidRow

            # Row 2 (80% height): Interactive Forecast
            fluidRow(
              column(width = 12,
                     bs4Card(
                       title = "Interactive Forecast",
                       width = 12,
                       height = "600px",  # approx. 80%
                       status = "success",
                       plotlyOutput(ns("interactive_forecast_plot"), height = "550px")
                     )
              )
            )
            
          
          ) # tabPanel
          
          
        )
      )
    )
  )
}
