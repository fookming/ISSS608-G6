
# forecasting_server.R

library(shiny)
library(tidyverse)  # data manipulation, reading CSV, and plotting
library(lubridate)
library(bs4Dash)
library(shinyjs)
library(forecast)
library(memoise)
library(cachem)
library(tseries)
library(plotly)
library(prophet)
library(ggplot2)




forecasting_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns   # <-- clearly define this explicitly here
    
    # Load data
    climate_data <- read_csv("data/climate_final_2018_2024.csv") %>%
      mutate(Date = ymd(paste(Year, Month, Day, sep = "-")))
    
    # Define limited-size cache explicitly
    my_cache <- cache_mem(max_size = 128 * 1024^2)  # 128 MB explicitly
    
    # Observer to handle enabling/disabling date inputs clearly
    observeEvent(input$time_period, {
      
      if (input$time_period == "All (2018-2024)") {
        shinyjs::disable("start_date")
        shinyjs::disable("end_date")
      } else if (input$time_period == "Custom") {
        shinyjs::enable("start_date")
        shinyjs::disable("end_date")
      }
      
    }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
  
    observeEvent(c(input$variable, input$station, input$time_view, input$time_period), {
      
      output$arima_order <- renderPrint({
        cat("") # clear ARIMA text explicitly
      })
      
    })
    
    
    
    
    # Calculate Arima Order
    observeEvent(input$calc_arima, {
      
      # Immediately change button text to show calculation is in progress
      updateActionButton(session, "calc_arima", label = "Calculating ARIMA (please wait...)")
      
      # Force immediate UI update clearly
      shinyjs::disable("calc_arima") # disable button temporarily
      
      # Show modal dialog clearly before calculation starts
      # showModal(modalDialog("Calculating ARIMA, please wait...", footer = NULL))
      
      # Explicitly force immediate UI update before heavy calculation
      shinyjs::runjs("Shiny.setInputValue('forceUIUpdate', Math.random());")
      
      # Short pause to ensure the browser updates the button explicitly
      Sys.sleep(0.2)  # short pause of 200 ms
      
      # Perform synchronous ARIMA calculation explicitly
      fit <- auto.arima(filtered_ts(), seasonal = TRUE)
      orders <- arimaorder(fit)
      
      # After calculation clearly hide modal dialog
      # removeModal()
      
      # Update ARIMA output explicitly after calculation
      output$arima_order <- renderPrint({
        cat("Suggested ARIMA Model:\n")
        cat(
          paste0("ARIMA(", orders[1], ",", orders[2], ",", orders[3], ")",
                 "(", orders[4], ",", orders[5], ",", orders[6], ")[", orders[7], "]\n\n")
        )
        
        cat("Non-seasonal parameters:\n")
        cat("  p (AR):", orders[1], "\n")
        cat("  d (Diff):", orders[2], "\n")
        cat("  q (MA):", orders[3], "\n\n")
        
        cat("Seasonal parameters:\n")
        cat("  P (Seasonal AR):", orders[4], "\n")
        cat("  D (Seasonal Diff):", orders[5], "\n")
        cat("  Q (Seasonal MA):", orders[6], "\n")
        cat("  Seasonal frequency:", orders[7], "\n")
      })
      
      # After calculation, revert button explicitly
      updateActionButton(session, "calc_arima", label = "Calculate ARIMA Order")
      shinyjs::enable("calc_arima") # re-enable button
      
    })
    
  
    
    # Extract choices for UI dropdowns
    variable_choices <- names(climate_data)[6:10]
    station_choices <- unique(climate_data$Station)
    
    # Dynamically populate UI dropdowns
    updateSelectInput(session, "variable", choices = variable_choices)
    updateSelectInput(session, "station", choices = station_choices)
    
  
    
    aggregate_data <- memoise(function(variable, station, time_view, time_period, start_date) {
      data <- climate_data %>%
        filter(Station == station) %>%
        select(Date, Value = all_of(variable))
      
      if (time_period == "Custom") {
        data <- data %>% filter(Date >= start_date)
      }
      
      aggregated <- data %>%
        mutate(Period = floor_date(Date, unit = switch(time_view,
                                                       "Week" = "week",
                                                       "Month" = "month",
                                                       "Day" = "day")))
      
      # Set aggregation function based on variable
      aggregation_fun <- if (variable == "Daily Rainfall Total (mm)") sum else mean
      
      aggregated %>%
        group_by(Period) %>%
        summarise(Value = aggregation_fun(Value, na.rm = TRUE), .groups = "drop") %>%
        rename(Date = Period)
      
    }, cache = my_cache)
    

    
    # Reactive filtering 
    filtered_aggregated_data <- reactive({
      req(input$variable, input$station, input$time_view, input$time_period)
      
      aggregate_data(
        input$variable, 
        input$station, 
        input$time_view, 
        input$time_period, 
        input$start_date
      )
    })
    
    # Time-series plot
    output$time_series_plot <- renderPlot({
      req(filtered_aggregated_data())
      
      filtered_aggregated_data() %>%
        ggplot(aes(x = Date, y = Value)) +
        geom_line(color = "steelblue") +
        labs(
          title = paste(input$time_view, "View of", input$variable, "at", input$station),
          x = "Date",
          y = input$variable
        ) +
        theme_minimal()
    })
    
    
    # Prepare data for time-series
    filtered_ts <- reactive({
      data <- filtered_aggregated_data()
      req(nrow(data) > 0)  # ensure data is available
      
      freq <- switch(input$time_view,
                     "Day" = 365,
                     "Week" = 52,
                     "Month" = 12)
      
      ts(data$Value, 
         frequency = freq, 
         start = c(year(min(data$Date)), yday(min(data$Date))))
    })
    
    
    # Then your plotting and STL decomposition logic follows here...
    output$stl_plot <- renderPlot({
      req(filtered_ts())
      
      stl_result <- stl(filtered_ts(), s.window = "periodic")
      
      autoplot(stl_result) +
        labs(title = paste("STL Decomposition of", input$variable, "at", input$station)) +
        theme_minimal()
    })
    
    
    # ACF plot
    output$acf_plot <- renderPlot({
      req(filtered_ts())
      
      forecast::ggAcf(filtered_ts()) +
        labs(title = paste("ACF Plot of", input$variable, "at", input$station)) +
        theme_minimal()
    })
    
    # PACF plot
    output$pacf_plot <- renderPlot({
      req(filtered_ts())
      
      forecast::ggPacf(filtered_ts()) +
        labs(title = paste("PACF Plot of", input$variable, "at", input$station)) +
        theme_minimal()
    })
  
    
  
    output$adf_test_result <- renderPrint({
      
      ts_data <- filtered_ts()
      
      # Explicit check if data is valid
      if (is.null(ts_data) || length(ts_data) < 3) {
        cat("Insufficient data points for ADF test.\n")
        return()
      }
      
      # Perform ADF test explicitly
      adf_result <- adf.test(ts_data, alternative = "stationary")
      
      # Clearly print the results and interpretation
      cat("Augmented Dickey-Fuller Test for Stationarity:\n\n")
      
      cat("Test statistic:", round(adf_result$statistic, 4), "\n")
      cat("P-value:", round(adf_result$p.value, 4), "\n")
      cat("Lag order:", adf_result$parameter, "\n")
      cat("Alternative hypothesis: Stationary\n\n")
      
      if (adf_result$p.value < 0.05) {
        cat("Conclusion: Series is STATIONARY (p < 0.05), reject the null hypothesis of non-stationarity.\n")
      } else {
        cat("Conclusion: Series is NON-STATIONARY (p ≥ 0.05), fail to reject the null hypothesis of non-stationarity.\n")
      }
    })
    
    
    # TABPANEL - FORECASTING
    
    # Static Forecast Plot
    output$static_forecast_plot <- renderPlot({
      req(input$model, input$forecast_horizon, input$time_view, filtered_ts())
      
      # Explicitly get filtered time-series
      ts_data <- filtered_ts()
      
      # Adjust forecast horizon explicitly based on view selected
      forecast_horizon <- as.numeric(input$forecast_horizon)
      
      # Perform explicit forecasting based on model selected
      fc <- switch(input$model,
                   "ARIMA" = forecast(auto.arima(ts_data), h = forecast_horizon),
                   "ETS" = forecast(ets(ts_data), h = forecast_horizon),
                   "Prophet" = {
                     # Convert ts_data explicitly into a dataframe with dates
                     df_prophet <- tibble(
                       ds = seq.Date(
                         from = min(filtered_aggregated_data()$Date),
                         by = switch(input$time_view,
                                     "Day" = "day", "Week" = "week", "Month" = "month"),
                         length.out = length(ts_data)
                       ),
                       y = as.numeric(ts_data)
                     )
                     
                     # Explicitly fit the Prophet model
                     model <- prophet::prophet(df_prophet)
                     
                     # Explicitly create future dates for forecasting
                     future <- prophet::make_future_dataframe(
                       model,
                       periods = forecast_horizon,
                       freq = switch(input$time_view,
                                     "Day" = "day", "Week" = "week", "Month" = "month")
                     )
                     
                     # Explicitly compute the forecast
                     forecast <- predict(model, future)
                     
                     # Return both model and forecast explicitly as a list
                     list(model = model, forecast = forecast)
                   },
                   stop("Unsupported model explicitly chosen."))
      
      # Explicit Plotting based on model
      if(input$model %in% c("ARIMA", "ETS")){
        autoplot(fc) +
          ggtitle(paste(input$model, "Forecast (Horizon:", forecast_horizon, input$time_view, ")")) +
          theme_minimal()
        
      } else if(input$model == "Prophet"){
        plot(fc$model, fc$forecast) +
          ggtitle(paste("Prophet Forecast (Horizon:", forecast_horizon, input$time_view, ")")) +
          theme_minimal()
      }
    })
    
    
  
    
    
    # Interactive forecast plot
    output$interactive_forecast_plot <- renderPlotly({
      req(input$model, input$forecast_horizon, input$time_view, filtered_aggregated_data())
      
      # Explicit Aggregation: Sum rainfall explicitly per period
      aggregated_data <- filtered_aggregated_data() %>%
        mutate(period = floor_date(Date, unit = switch(input$time_view,
                                                       "Day" = "day",
                                                       "Week" = "week",
                                                       "Month" = "month"))) %>%
        group_by(period) %>%
        summarise(value = sum(Value, na.rm = TRUE)) %>%  # ensure explicitly summing rainfall
        arrange(period)
      
      ts_freq <- switch(input$time_view, "Day" = 365, "Week" = 52, "Month" = 12)
      ts_data <- ts(aggregated_data$value, frequency = ts_freq)
      
      forecast_horizon <- as.numeric(input$forecast_horizon)
      freq <- switch(input$time_view, "Day" = "day", "Week" = "week", "Month" = "month")
      
      # explicitly correct forecast start date
      last_date <- max(aggregated_data$period)
      forecast_dates <- switch(input$time_view,
                               "Day" = seq.Date(last_date + days(1), by = "day", length.out = forecast_horizon),
                               "Week" = seq.Date(last_date + weeks(1), by = "week", length.out = forecast_horizon),
                               "Month" = seq.Date(last_date %m+% months(1), by = "month", length.out = forecast_horizon))
      
      if (input$model == "ARIMA") {
        fc <- forecast(auto.arima(ts_data), h = forecast_horizon)
      } else if (input$model == "ETS") {
        fc <- forecast(ets(ts_data), h = forecast_horizon)
      } else if (input$model == "Prophet") {
        # Correctly prepared aggregated historical data
        df_prophet <- aggregated_data %>% rename(ds = period, y = value)
        
        # Prophet model fitting explicitly
        model <- prophet(df_prophet)
        
        # Correct future dataframe explicitly
        future <- make_future_dataframe(model, periods = forecast_horizon, freq = freq)
        fc <- predict(model, future)
        
        plot_data <- fc %>%
          select(ds, yhat, yhat_lower, yhat_upper) %>%
          left_join(df_prophet, by = "ds") %>%
          mutate(
            type = ifelse(ds > max(df_prophet$ds), "Forecast", "Historical"),
            value = ifelse(type == "Forecast", yhat, y),
            lower = ifelse(type == "Forecast", yhat_lower, NA),
            upper = ifelse(type == "Forecast", yhat_upper, NA),
            text = paste0(
              switch(input$time_view,
                     "Month" = paste0("Month: ", format(ds, "%Y-%m")),
                     "Week" = paste0("Week: ", format(ds, "%Y-%U")),
                     "Day" = paste0("Date: ", format(ds, "%Y-%m-%d"))
              ),
              "<br>Rainfall: ", round(value, 2), " mm")
          ) %>% rename(date = ds)
        
      } else {
        stop("Unsupported model explicitly chosen.")
      }
      
      # ARIMA and ETS handling (correct explicitly)
      if (input$model %in% c("ARIMA", "ETS")) {
        plot_data <- tibble(
          date = c(aggregated_data$period, forecast_dates),
          value = c(as.numeric(ts_data), as.numeric(fc$mean)),
          type = c(rep("Historical", length(ts_data)), rep("Forecast", forecast_horizon)),
          lower = c(rep(NA, length(ts_data)), fc$lower[, 2]),
          upper = c(rep(NA, length(ts_data)), fc$upper[, 2])
        ) %>% mutate(
          text = paste0(
            switch(input$time_view,
                   "Month" = paste0("Month: ", format(date, "%Y-%m")),
                   "Week" = paste0("Week: ", format(date, "%Y-%U")),
                   "Day" = paste0("Date: ", format(date, "%Y-%m-%d"))
            ),
            "<br>Rainfall: ", round(value, 2), "  ")
        )
      }
      
      connector_df <- plot_data %>% filter(row_number() %in% c(length(ts_data), length(ts_data) + 1))
      
      y_axis_title <- paste(
        switch(input$time_view,
               "Day" = if (input$variable == "Daily Rainfall Total (mm)") "Daily Total - " else "Daily Mean - ",
               "Week" = if (input$variable == "Daily Rainfall Total (mm)") "Weekly Total - " else "Weekly Mean - ",
               "Month" = if (input$variable == "Daily Rainfall Total (mm)") "Monthly Total - " else "Monthly Mean - "),
        input$variable
      )
      
      plot_ly() %>%
        add_ribbons(data = plot_data,
                    x = ~date, ymin = ~lower, ymax = ~upper,
                    fillcolor = 'rgba(0,0,255,0.2)',
                    line = list(color = 'transparent'),
                    name = "Confidence Interval",
                    hoverinfo = "none") %>%
        add_trace(data = plot_data %>% filter(type == "Historical"),
                  x = ~date, y = ~value, type = 'scatter', mode = 'lines',
                  line = list(color = "black"), name = "Historical",
                  text = ~text, hoverinfo = "text") %>%
        add_trace(data = plot_data %>% filter(type == "Forecast"),
                  x = ~date, y = ~value, type = 'scatter', mode = 'lines',
                  line = list(color = "blue"), name = "Forecast",
                  text = ~text, hoverinfo = "text") %>%
        add_trace(data = connector_df,
                  x = ~date, y = ~value, type = 'scatter', mode = 'lines+markers',
                  line = list(color = "gray40", dash = "dot"),
                  marker = list(color = "gray40", size = 6),
                  name = "Connector", text = ~text, hoverinfo = "text") %>%
        layout(title = paste("Interactive", input$model, "Forecast for", y_axis_title, "at", input$station),
               xaxis = list(title = "Date"),
               yaxis = list(title = paste(y_axis_title, "  ")))
    })
    
    
    
    
    
    
    
    
    
    
  
    
    
    
    # Forecast Evaluation Table
    output$forecast_eval_table <- renderTable({
      req(filtered_aggregated_data(), input$forecast_horizon)
      
      data <- filtered_aggregated_data()
      
      # Use the last `k` points as test set (min 6)
      k <- max(6, round(nrow(data) * 0.2))
      train <- head(data, -k)
      test <- tail(data, k)
      
      ts_train <- ts(train$Value, frequency = switch(input$time_view,
                                                     "Day" = 365, "Week" = 52, "Month" = 12))
      
      # Ensure matching start point for test
      ts_test <- test$Value
      
      # ARIMA
      fit_arima <- forecast(auto.arima(ts_train), h = k)
      arima_metrics <- accuracy(fit_arima, ts_test)
      
      # ETS
      fit_ets <- forecast(ets(ts_train), h = k)
      ets_metrics <- accuracy(fit_ets, ts_test)
      
      # Prophet
      df_prophet <- train %>%
        rename(ds = Date, y = Value)
      model_prophet <- prophet(df_prophet, verbose = FALSE)
      future <- make_future_dataframe(model_prophet, periods = k, freq = switch(input$time_view,
                                                                                "Day" = "day",
                                                                                "Week" = "week",
                                                                                "Month" = "month"))
      forecast_prophet <- predict(model_prophet, future)
      yhat <- tail(forecast_prophet$yhat, k)
      prophet_metrics <- c(
        MAE = mean(abs(ts_test - yhat)),
        RMSE = sqrt(mean((ts_test - yhat)^2)),
        MAPE = mean(abs((ts_test - yhat) / ts_test)) * 100
      )
      
      # Summary Table
      eval_table <- tibble::tibble(
        Model = c("ARIMA", "ETS", "Prophet"),
        MAE = round(c(arima_metrics["Test set", "MAE"], ets_metrics["Test set", "MAE"], prophet_metrics["MAE"]), 2),
        RMSE = round(c(arima_metrics["Test set", "RMSE"], ets_metrics["Test set", "RMSE"], prophet_metrics["RMSE"]), 2),
        MAPE = round(c(arima_metrics["Test set", "MAPE"], ets_metrics["Test set", "MAPE"], prophet_metrics["MAPE"]), 2)
      )
      
      # Recommend model with lowest RMSE
      best_model <- eval_table$Model[which.min(eval_table$RMSE)]
      eval_table <- add_column(eval_table, Recommendation = ifelse(eval_table$Model == best_model, "✅ Recommended", ""))
      
      eval_table
    })
    
      
        

  
    
  })

}

