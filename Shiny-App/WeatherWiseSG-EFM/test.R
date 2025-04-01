



tabPanel(
  title = "FORECASTING",
  fluidRow(
    # Row 1 (20% height): area 1 and area 2
    column(width = 8,
           bs4Card(
             title = "Static Forecast",
             width = 12,
             height = "250px",  # approx. 20%
             status = "primary",
             plotOutput(ns("static_forecast_plot"), height = "200px")
           )
    ),
    column(width = 4,
           bs4Card(
             title = "Evaluation Metrics (ARIMA/SARIMA, Prophet, ETS)",
             width = 12,
             height = "250px",  # approx. 20%
             status = "info",
             tableOutput(ns("forecast_eval_table"))
           )
    )
  ),
  
  # Row 2 (80% height): Interactive Forecast
  fluidRow(
    column(width = 12,
           bs4Card(
             title = "Interactive Forecast with Tooltips",
             width = 12,
             height = "600px",  # approx. 80%
             status = "success",
             plotlyOutput(ns("interactive_forecast_plot"), height = "550px")
           )
    )
  )
)