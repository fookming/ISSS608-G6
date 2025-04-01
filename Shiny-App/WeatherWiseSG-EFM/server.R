#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# -----------------------------------------------------------------
# Load Packages
# -----------------------------------------------------------------
pacman::p_load(lubridate, ggthemes, tidyverse,
               ggridges, corrplot, GGally, ggstatsplot,
               DT, viridis, forcats, nortest, patchwork,
               tsibble, feasts, ggpubr, shiny, bs4Dash)


# EFM
source("forecasting_server.R")

# -----------------------------------------------------------------
# Importing data
# -----------------------------------------------------------------
climate <- read_csv("data/climate_final_2018_2024.csv")

# -----------------------------------------------------------------
# Section 1: EDA & CDA - Preprocessing
# -----------------------------------------------------------------
climate_eda <- climate %>%
  mutate(
    Date = make_date(Year, Month, Day),
    Station = as.factor(Station),
    Month = factor(Month, levels = 1:12, labels = month.abb)
  )

# -----------------------------------------------------------------
# Section 2: Time Series Forecasting - Preprocessing
# -----------------------------------------------------------------
climate_time <- climate # (whatever unique name you want)

# -----------------------------------------------------------------
# Section 3: Geo-Spatial - Preprocessing
# -----------------------------------------------------------------
 climate_geo <- climate # (whatever unique name you want)

# -----------------------------------------------------------------
# Shiny Server Function
# -----------------------------------------------------------------
function(input, output, session) {

  # EDA Tab Outputs
  # -------------------------------------------------------------
  # 1 Overview
  # 2 Seasonality Analysis
  
  # 3.1 Station Comparison - Station-wise Distribution
  # Update parameter choices dynamically from dataset
  observe({
    param_choices <- names(climate_eda)[sapply(climate_eda, is.numeric)]
    param_choices <- param_choices[param_choices %in% c(
      "Daily Rainfall Total (mm)",
      "Mean Temperature (Celsius)",
      "Maximum Temperature (Celsius)",
      "Minimum Temperature (Celsius)",
      "Mean Wind Speed (km/h)"
    )]
    
    updateSelectInput(session, "eda_station_param_1",
                      choices = param_choices,
                      selected = "Mean Temperature (Celsius)")
  })
  
  # Update year options dynamically
  observe({
    year_choices <- sort(unique(climate_eda$Year))
    year_opts <- c("All Years" = "all", as.character(year_choices))
    
    updateSelectInput(session, "eda_station_time_1",
                       choices = year_opts,
                       selected = "all")
  })
  
  # Filter data based on parameter and year
  filtered_station_data <- reactive({
    req(input$eda_station_param_1)
    
    df <- climate_eda
    
    if (input$eda_station_time_1 != "all") {
      df <- df %>% filter(Year == as.numeric(input$eda_station_time_1))
    }
    
    return(df)
  })
  
  # Render Station-wise Distribution Chart
  output$station_dist_plot <- renderPlot({
    req(input$eda_station_param_1)
    
    ggplot(filtered_station_data(), aes(x = .data[[input$eda_station_param_1]], y = Station)) +
      geom_boxplot(fill = "steelblue") +
      labs(
        title = paste(input$eda_station_param_1, "Distribution by Station"),
        x = input$eda_station_param_1,
        y = "Station"
      ) +
      theme_minimal()
  })
  
  # 3.2 Station Comparison - Month-by-Station Variation
  

  # CDA Tab Outputs
  # -------------------------------------------------------------
  # 1.1 Compare across stations - Normality
  # output$cda_test_results <- renderPrint({ ... })
  # 1.2 Compare across stations - Statistical Test
  # output$cda_group_plot <- renderPlot({ ... })
  
  # 2.1 Compare across time - Normality
  # 2.2 Compare across time - Statistical Test


  # Time Series Forecasting Tab Outputs
  # -------------------------------------------------------------
  # output$ts_forecast_plot <- renderPlot({ ... })
  # output$ts_trend_plot <- renderPlot({ ... })
  
  # EFM
  
  forecasting_server("forecasting")
  
  
  

  # Geo-Spatial Tab Outputs
  # -------------------------------------------------------------
  # output$geo_map <- renderLeaflet({ ... })
  # output$extreme_event_plot <- renderPlot({ ... })
  
}
