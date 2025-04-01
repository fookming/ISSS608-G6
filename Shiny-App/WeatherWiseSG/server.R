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
               tsibble, feasts, ggpubr, shiny, bs4Dash,
               plotly, shinyWidgets)

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
  # Update parameter choices dynamically from dataset
  observe({
    # Define mapping: Display Name = Column Name
    param_choices <- c(
      "Daily Rainfall" = "Daily Rainfall Total (mm)",
      "Mean Temperature" = "Mean Temperature (Celsius)",
      "Maximum Temperature" = "Maximum Temperature (Celsius)",
      "Minimum Temperature" = "Minimum Temperature (Celsius)",
      "Mean Wind Speed" = "Mean Wind Speed (km/h)"
    )
    
    # Filter only columns that actually exist in the dataset
    valid_params <- param_choices[param_choices %in% names(climate_eda)]
    
    updateSelectInput(session, "eda_overview_parameter",
                      choices = valid_params,
                      selected = "Mean Temperature (Celsius)")
  })
  
  output$eda_overview_title <- renderUI({
    req(input$eda_overview_parameter)
  
    HTML(paste0("<b> Singapore Overview – ", input$eda_overview_parameter, "</b>"))
  })
  
  # Summary Boxes - 
  # --- Hottest Day ---
  output$overview_box_1 <- renderUI({
    req(input$eda_overview_parameter)
    
    param <- input$eda_overview_parameter
    if (param == "Mean Wind Speed (km/h)") {
      windiest_day <- climate_eda %>%
        slice_max(`Mean Wind Speed (km/h)`, n = 1)
      
      wind_val <- round(windiest_day$`Mean Wind Speed (km/h)`[1], 2)
      wind_date <- format(windiest_day$Date[1], "%d %b %Y")
      
      div(
        style = "background-color: #ffffff; border: 1px solid #ddd; border-left: 8px solid #5bc0de; padding: 15px 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); margin-bottom: 10px; border: 1px solid #dee2e6;",
        div(
          style = "display: flex; align-items: center; gap: 15px;",
          icon("wind", class = "fa-2x", style = "color: #5bc0de;"),
          div(
            div(style = "font-size: 22px; font-weight: bold; color: #222;", paste0(wind_val, " km/h")),
            div(style = "font-size: 14px; color: #666;", paste("Windiest Day:", wind_date))
          )
        )
      )
    } else if (param == "Daily Rainfall Total (mm)") {
      # Rainfall logic – Wettest Day
      wettest_day <- climate_eda %>% slice_max(`Daily Rainfall Total (mm)`, n = 1)
      val <- round(wettest_day$`Daily Rainfall Total (mm)`[1], 1)
      date <- format(wettest_day$Date[1], "%d %b %Y")
      
      div(
        style = "background-color: #ffffff; border: 1px solid #ddd; border-left: 8px solid #5bc0de; padding: 15px 20px; 
               border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); border: 1px solid #eee;",
        div(
          style = "display: flex; align-items: center; gap: 15px;",
          icon("cloud-rain", class = "fa-2x", style = "color: #5bc0de;"),
          div(
            div(style = "font-size: 22px; font-weight: bold; color: #222;", paste0(val, " mm")),
            div(style = "font-size: 14px; color: #666;", paste("Wettest Day:", date))
          )
        )
      )
    } else {
      # Temperature logic (same as your earlier hottest day logic)
      hottest_day <- climate_eda %>%
        slice_max(`Maximum Temperature (Celsius)`, n = 1)
      
      temp <- round(hottest_day$`Maximum Temperature (Celsius)`[1], 2)
      date <- format(hottest_day$Date[1], "%d %b %Y")
      
      div(
        style = "background-color: #ffffff; border: 1px solid #ddd; border-left: 8px solid #d9534f; padding: 15px 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
        div(
          style = "display: flex; align-items: center; gap: 15px;",
          icon("temperature-high", class = "fa-2x", style = "color: #d9534f;"),
          div(
            div(style = "font-size: 22px; font-weight: bold; color: #222;", paste0(temp, "°C")),
            div(style = "font-size: 14px; color: #666;", paste("Hottest Day:", date))
          )
        )
      )
    }
  })
  
  # --- Coldest Day ---
  output$overview_box_2 <- renderUI({
    req(input$eda_overview_parameter)
    
    param <- input$eda_overview_parameter
    if (param == "Mean Wind Speed (km/h)") {
      windiest_month <- climate_eda %>%
        group_by(Year, Month) %>%
        summarise(Monthly_Wind = mean(`Mean Wind Speed (km/h)`, na.rm = TRUE), .groups = "drop") %>%
        slice_max(Monthly_Wind, n = 1)
      
      wind_val <- round(windiest_month$Monthly_Wind[1], 2)
      month_str <- paste(month.abb[windiest_month$Month[1]], windiest_month$Year[1])
      
      div(
        style = "background-color: #ffffff; border: 1px solid #ddd; border-left: 10px solid #5bc0de; padding: 15px 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); margin-bottom: 10px; border: 1px solid #dee2e6;",
        div(
          style = "display: flex; align-items: center; gap: 15px;",
          icon("calendar-alt", class = "fa-2x", style = "color: #5bc0de;"),
          div(
            div(style = "font-size: 22px; font-weight: bold; color: #222;", paste0(wind_val, " km/h")),
            div(style = "font-size: 14px; color: #666;", paste("Windiest Month:", month_str))
          )
        )
      )
    
    } else if (param == "Daily Rainfall Total (mm)") {
      wettest_month <- climate_eda %>%
        group_by(Year, Month) %>%
        summarise(Monthly_Rain = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE), .groups = "drop") %>%
        slice_max(Monthly_Rain, n = 1)
      
      total_rain <- round(wettest_month$Monthly_Rain[1], 1)
      month_text <- paste0(month.name[wettest_month$Month[1]], " ", wettest_month$Year[1])
      
      div(
        style = "background-color: #ffffff; border: 1px solid #ddd; border-left: 8px solid #0275d8; padding: 15px 20px; 
               border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); border: 1px solid #eee;",
        div(
          style = "display: flex; align-items: center; gap: 15px;",
          icon("calendar-day", class = "fa-2x", style = "color: #0275d8;"),
          div(
            div(style = "font-size: 22px; font-weight: bold; color: #222;", paste0(total_rain, " mm")),
            div(style = "font-size: 14px; color: #666;", paste("Wettest Month:", month_text))
          )
        )
      )
    } else {
      coldest_day <- climate_eda %>%
        slice_min(`Minimum Temperature (Celsius)`, n = 1)
      
      temp <- round(coldest_day$`Minimum Temperature (Celsius)`[1], 2)
      date <- format(coldest_day$Date[1], "%d %b %Y")
      
      div(
        style = "background-color: #ffffff; border: 1px solid #ddd; border-left: 8px solid #0275d8; padding: 15px 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
        div(
          style = "display: flex; align-items: center; gap: 15px;",
          icon("temperature-low", class = "fa-2x", style = "color: #0275d8;"),
          div(
            div(style = "font-size: 22px; font-weight: bold; color: #222;", paste0(temp, "°C")),
            div(style = "font-size: 14px; color: #666;", paste("Coolest Day:", date))
          )
        )
      )
    }
  })
  
  
  # --- Hottest Month ---
  output$overview_box_3 <- renderUI({
    req(input$eda_overview_parameter)
    
    param <- input$eda_overview_parameter
    if (param == "Mean Wind Speed (km/h)") {
      return(NULL)
    } else if (param == "Daily Rainfall Total (mm)") {
      return(NULL)
    } else {
      hottest_month <- climate_eda %>%
        group_by(Year, Month) %>%
        summarise(Avg_Temp = mean(`Mean Temperature (Celsius)`, na.rm = TRUE), .groups = "drop") %>%
        slice_max(Avg_Temp, n = 1)
      
      temp <- round(hottest_month$Avg_Temp[1], 2)
      label <- paste(month.name[hottest_month$Month[1]], hottest_month$Year[1])
      
      div(
        style = "background-color: #ffffff; border: 1px solid #ddd; border-left: 8px solid #f0ad4e; padding: 15px 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
        div(
          style = "display: flex; align-items: center; gap: 15px;",
          icon("sun", class = "fa-2x", style = "color: #f0ad4e;"),
          div(
            div(style = "font-size: 22px; font-weight: bold; color: #222;", paste0(temp, "°C")),
            div(style = "font-size: 13.5px; color: #666;", paste("Hottest Month:", label))
          )
        )
      )
    }
  })
  
  # --- Coldest Month ---
  output$overview_box_4 <- renderUI({
    req(input$eda_overview_parameter)
    
    param <- input$eda_overview_parameter
    if (param == "Mean Wind Speed (km/h)") {
      return(NULL)
    } else if (param == "Daily Rainfall Total (mm)") {
      rainiest_month <- climate_eda %>%
        filter(`Daily Rainfall Total (mm)` > 0) %>%
        group_by(Station, Year, Month) %>%
        summarise(Rainy_Days = n(), .groups = "drop") %>%
        slice_max(Rainy_Days, n = 1)
      
      days <- rainiest_month$Rainy_Days[1]
      label <- paste0(month.name[rainiest_month$Month[1]], " ", rainiest_month$Year[1])
      station <- rainiest_month$Station[1]
      
      div(
        style = "background-color: #ffffff; border: 1px solid #ddd; border-left: 8px solid #5cb85c; padding: 15px 20px; 
               border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); border: 1px solid #eee;",
        div(
          style = "display: flex; align-items: center; gap: 15px;",
          icon("umbrella", class = "fa-2x", style = "color: #5cb85c;"),
          div(
            div(style = "font-size: 22px; font-weight: bold; color: #222;", paste0(days, " days")),
            div(style = "font-size: 13px; color: #666;", paste("Rainiest Month:", label)),
            div(style = "font-size: 13px; color: #666;", paste("Station:", station))
          )
        )
      )
    } else {
      coldest_month <- climate_eda %>%
        group_by(Year, Month) %>%
        summarise(Avg_Temp = mean(`Mean Temperature (Celsius)`, na.rm = TRUE), .groups = "drop") %>%
        slice_min(Avg_Temp, n = 1)
      
      temp <- round(coldest_month$Avg_Temp[1], 2)
      label <- paste(month.name[coldest_month$Month[1]], coldest_month$Year[1])
      
      div(
        style = "background-color: #ffffff; border: 1px solid #ddd; border-left: 8px solid #17a2b8; padding: 15px 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
        div(
          style = "display: flex; align-items: center; gap: 15px;",
          icon("snowflake", class = "fa-2x", style = "color: #17a2b8;"),
          div(
            div(style = "font-size: 22px; font-weight: bold; color: #222;", paste0(temp, "°C")),
            div(style = "font-size: 14px; color: #666;", paste("Coolest Month:", label))
          )
        )
      )
    }
  })
  
  
  # Plot 1 - Yearly Trend
  output$overview_yearly_plot <- renderPlotly({
    req(input$eda_overview_parameter)
    
    param <- input$eda_overview_parameter
    
    # Determine aggregation logic
    is_rainfall <- grepl("Daily Rainfall", param, ignore.case = TRUE)
    agg_func <- if (is_rainfall) sum else mean
    y_label <- if (is_rainfall) paste("Total - ", param) else paste("Avg - ", param)
    plot_title <- if (is_rainfall) {
      paste("Yearly Total Trend")
    } else {
      paste("Yearly Average Trend")
    }
    
    yearly_data <- climate_eda %>%
      group_by(Year) %>%
      summarise(Avg_Value = agg_func(.data[[param]], na.rm = TRUE))
    
    p <- ggplot(yearly_data, aes(x = Year, y = Avg_Value)) +
      geom_line(color = "darkorange", linewidth = 1) +
      geom_point(aes(text= round(Avg_Value, 2)),
                 size = 2, color = "darkorange") +
      labs(
        title = plot_title,
        x = "Year",
        y = param
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Plot 2 - Year Comparison Chart: Bars (2018–2023), Line (2024) 
  output$overview_yearcompare_plot <- renderPlotly({
    req(input$eda_overview_parameter)
    
    param <- input$eda_overview_parameter
    
    # Check if it's a rainfall parameter (use sum instead of mean)
    is_rainfall <- grepl("Daily Rainfall", param, ignore.case = TRUE)
    agg_func <- if (is_rainfall) sum else mean
    y_label <- if (is_rainfall) paste("Total -", param) else paste("Avg -", param)
    
    # Prepare data
    weather_summary <- climate_eda %>%
      group_by(Year, Month) %>%
      summarise(Value = agg_func(.data[[param]], na.rm = TRUE), .groups = "drop")
    
    # Separate datasets
    bars_data <- weather_summary %>% filter(Year %in% 2018:2023)
    line_data <- weather_summary %>% filter(Year == 2024)
    
    # Aggregate for bars (mean across 2018–2023)
    bars_avg <- bars_data %>%
      group_by(Month) %>%
      summarise(Avg_Value = mean(Value, na.rm = TRUE), .groups = "drop")
    
    # Create plotly object
    fig <- plot_ly() %>%
      # Bars: 2018–2023 average
      add_trace(
        data = bars_avg,
        x = ~Month,
        y = ~Avg_Value,
        type = "bar",
        name = paste("2018-2023", if (is_rainfall) "Total" else "Avg"),
        marker = list(color = "salmon"),
        hovertemplate = paste0(
          "<b>Value:</b> %{y:.2f}<extra></extra>"
        )
      ) %>%
      
      # Line: 2024 values
      add_trace(
        data = line_data,
        x = ~Month,
        y = ~Value,
        type = "scatter",
        mode = "lines+markers",
        name = paste("2024", if (is_rainfall) "Total" else "Avg"),
        line = list(color = "black", width = 2),
        marker = list(color = "black", size = 6),
        hovertemplate = paste0(
          "<b>Value (2024):</b> %{y:.2f}<extra></extra>"
        )
      ) %>%
      
      layout(
        title = list(
          text = paste("Monthly Comparison: 2024 vs 2018-2023"),
          font = list(size = 15)
        ),
        legend = list(
          orientation = "h",        # Horizontal legend
          x = 0.2,                    # Left-aligned
          y = 1.09,                 # Move it above the chart (adjust if needed)
          font = list(size = 9),   # Smaller font size
          xanchor = "left"
        ),
        barmode = "group",
        xaxis = list(title = "Month"),
        yaxis = list(title = param)      
        )
    fig
  })
  
  # Plot 3 - Top/Bottom 5 Stations Plot
  output$overview_5_stations_plot <- renderPlotly({
    req(input$eda_overview_parameter)
    
    param <- input$eda_overview_parameter
    
    # Determine aggregation and type
    is_rainfall <- grepl("Daily Rainfall", param, ignore.case = TRUE)
    is_min_temp <- grepl("Minimum Temperature", param, ignore.case = TRUE)
    
    agg_func <- if (is_rainfall) sum else mean
    y_label <- if (is_rainfall) paste("Total -", param) else paste("Avg -", param)
    
    # Adjust title logic
    plot_title <- if (is_min_temp) {
      paste("Bottom 5 Stations by Average Value")
    } else if (is_rainfall) {
      paste("Top 5 Stations by Total Value")
    } else {
      paste("Top 5 Stations by Average Value")
    }
    
    # Summarise data
    station_summary <- climate_eda %>%
      group_by(Station) %>%
      summarise(Value = agg_func(.data[[param]], na.rm = TRUE), .groups = "drop")
    
    # Slice top or bottom 5
    station_summary <- if (is_min_temp) {
      station_summary %>% slice_min(Value, n = 5)
    } else {
      station_summary %>% slice_max(Value, n = 5)
    }
    
    station_summary <- station_summary %>% arrange(Value)
    
    # Plotly chart
    plot_ly(
      data = station_summary,
      x = ~Value,
      y = ~reorder(Station, Value),
      type = "bar",
      orientation = "h",
      text = ~paste0(round(Value, 2)),
      textposition = "auto",
      marker = list(
        color = ~Value,
        colorscale = list(c(0, 1), c("#52bf90", "#317256")),  # light to dark green
        showscale = FALSE
      ),
      hovertemplate = paste(
        paste0("%{x:.2f}"),
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = list(text = plot_title, font = list(size = 16)),
        xaxis = list(title = param),
        yaxis = list(title = "Station"),
        margin = list(l = 60, r = 20, b = 40, t = 60)
      )
  })
  
  
  
  # 2 Seasonality Analysis
  # Update parameter choices dynamically from dataset
  observe({
    # Define mapping: Display Name = Column Name
    param_choices <- c(
      "Daily Rainfall" = "Daily Rainfall Total (mm)",
      "Mean Temperature" = "Mean Temperature (Celsius)",
      "Maximum Temperature" = "Maximum Temperature (Celsius)",
      "Minimum Temperature" = "Minimum Temperature (Celsius)",
      "Mean Wind Speed" = "Mean Wind Speed (km/h)"
    )
    
    # Filter only columns that actually exist in the dataset
    valid_params <- param_choices[param_choices %in% names(climate_eda)]
    
    updateSelectInput(session, "eda_seasonality_parameter",
                      choices = valid_params,
                      selected = "Mean Temperature (Celsius)")
  })
  
  # Update station options dynamically
  observe({
    station_choices <- sort(unique(climate_eda$Station))
    station_opts <- c("Singapore (All Stations)" = "Singapore", as.character(station_choices))
    
    updateSelectInput(session, "eda_seasonality_station",
                      choices = station_opts,
                      selected = "Singapore")
  })
  
  # Filter data based on parameter and station
  filtered_seasonality_data <- reactive({
    req(input$eda_seasonality_parameter)
    
    df <- climate_eda
    
    if (input$eda_seasonality_station != "Singapore") {
      df <- df %>% filter(Station == input$eda_seasonality_station)
    }
    
    return(df)
  })
  
  # ---- Plotly Seasonal Trend Line Chart ----
  output$seasonal_trend_plot <- renderPlotly({
    req(input$eda_seasonality_parameter)
    
    df <- filtered_seasonality_data()
    param <- input$eda_seasonality_parameter
    
    # Determine aggregation logic
    is_rainfall <- grepl("Daily Rainfall", param, ignore.case = TRUE)
    agg_func <- if (is_rainfall) sum else mean
    y_label <- if (is_rainfall) paste("Total - ", param) else paste("Avg - ", param)
    plot_title <- if (is_rainfall) {
      paste("Total Monthly Trend -", param)
    } else {
      paste("Average Monthly Trend -", param)
    }
    
    # Prepare data
    df_summary <- df %>%
      mutate(
        Year = lubridate::year(Date),
        Month = lubridate::month(Date, label = TRUE, abbr = TRUE)
      ) %>%
      group_by(Year, Month) %>%
      summarise(Value = agg_func(.data[[param]], na.rm = TRUE), .groups = "drop") %>%
      arrange(Year, Month)
    
    # Plotly line chart
    plot_ly(data = df_summary,
            x = ~Month,
            y = ~Value,
            color = ~as.factor(Year),
            colors = "Set2",
            type = "scatter",
            mode = "lines",
            line = list(width = 1.8),
            hovertemplate = paste(
              "<b>Month:</b> %{x}<br>",
              paste0("<b>", y_label, ":</b> %{y:.2f}"),
              "<extra></extra>"
            )) %>%
      layout(
        title = list(
          text = plot_title,
          font = list(size = 16),
          pad = list(b = 5)
        ),
        annotations = list(
          list(
            text = paste0("<b>Station:</b> ", input$eda_seasonality_station),
            xref = "paper", yref = "paper",
            x = -0.05, y = 1.05,  # Position slightly above the plot
            showarrow = FALSE,
            font = list(size = 12, color = "gray")
          )
        ),
        xaxis = list(title = "Month"),
        yaxis = list(title = y_label),
        legend = list(title = list(text = "Year")),
        margin = list(l = 60, r = 20, b = 40, t = 40)
      )
  })
  
  # Seasonality Cycle Plot
  output$seasonal_cycle_plot <- renderPlot({
    req(input$eda_seasonality_parameter)
    
    df <- filtered_seasonality_data()
    param <- input$eda_seasonality_parameter
    
    # Determine aggregation logic
    is_rainfall <- grepl("Daily Rainfall", param, ignore.case = TRUE)
    agg_func <- if (is_rainfall) sum else mean
    y_label <- if (is_rainfall) paste("Total -", param) else paste("Avg -", param)
    plot_title <- if (is_rainfall) {
      paste("Monthly Seasonality Cycle – Total", param)
    } else {
      paste("Monthly Seasonality Cycle – Avg", param)
    }
    
    # Prepare tsibble
    ts_df <- df %>%
      mutate(YearMonth = yearmonth(Date)) %>%
      group_by(YearMonth) %>%
      summarise(Value = agg_func(.data[[param]], na.rm = TRUE), .groups = "drop") %>%
      as_tsibble(index = YearMonth)
    
    # Generate cycle plot
    gg_subseries(ts_df, Value) +
      labs(
        title = plot_title,
        subtitle = paste("Station:", input$eda_seasonality_station),
        x = "Year",
        y = y_label
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),   # Rotate x labels
        panel.grid.minor.x = element_blank()
      ) 
  })
  
  # 3.1 Station Comparison - Station-wise Distribution
  # Update parameter choices dynamically from dataset
  observe({
    # Define mapping: Display Name = Column Name
    param_choices <- c(
      "Daily Rainfall" = "Daily Rainfall Total (mm)",
      "Mean Temperature" = "Mean Temperature (Celsius)",
      "Maximum Temperature" = "Maximum Temperature (Celsius)",
      "Minimum Temperature" = "Minimum Temperature (Celsius)",
      "Mean Wind Speed" = "Mean Wind Speed (km/h)"
    )
    
    # Filter only columns that actually exist in the dataset
    valid_params <- param_choices[param_choices %in% names(climate_eda)]
    
    updateSelectInput(session, "eda_station_param_1",
                      choices = valid_params,
                      selected = "Mean Temperature (Celsius)")
  })
  
  # Update year options dynamically
  observe({
    year_choices <- sort(unique(climate_eda$Year))
    year_opts <- c("All Years" = "All Years", as.character(year_choices))
    
    updateSelectInput(session, "eda_station_time_1",
                       choices = year_opts,
                       selected = "All Years")
  })
  
  # Filter data based on parameter and year
  filtered_station_data_1 <- reactive({
    req(input$eda_station_param_1)
    
    df <- climate_eda
    
    if (input$eda_station_time_1 != "All Years") {
      df <- df %>% filter(Year == as.numeric(input$eda_station_time_1))
    }
    
    return(df)
  })
  
  # Render Station-wise Distribution Chart
  output$station_dist_plot <- renderPlotly({
    req(input$eda_station_param_1)
    
    param <- input$eda_station_param_1
    data <- filtered_station_data_1()
    
    plot_ly(
      data = data,
      x = ~.data[[param]],
      y = ~Station,
      type = "box",
      orientation = "h",
      boxpoints = "outliers",
      marker = list(color = "#1f77b4", opacity = 0.6),
      line = list(color = "black", width = 1.4),
      fillcolor = "#1f77b4",
      hoverinfo = "x+y"
    ) %>%
      layout(
        title = paste("Distribution of", param, "by Station for", input$eda_station_time_1),
        xaxis = list(title = param),
        yaxis = list(title = "Station"),
        font = list(size = 12),
        margin = list(b = 30),
        hoverlabel = list(bgcolor = "#dfe7f3", font = list(size = 13))
      )
  })
  
  
  # 3.2 Station Comparison - Month-by-Station Variation
  # Update parameter choices dynamically from dataset
  observe({
    # Define mapping: Display Name = Column Name
    param_choices <- c(
      "Daily Rainfall" = "Daily Rainfall Total (mm)",
      "Mean Temperature" = "Mean Temperature (Celsius)",
      "Maximum Temperature" = "Maximum Temperature (Celsius)",
      "Minimum Temperature" = "Minimum Temperature (Celsius)",
      "Mean Wind Speed" = "Mean Wind Speed (km/h)"
    )
    
    # Filter only columns that actually exist in the dataset
    valid_params <- param_choices[param_choices %in% names(climate_eda)]
    
    updateSelectInput(session, "eda_station_param_2",
                      choices = valid_params,
                      selected = "Mean Temperature (Celsius)")
  })
  
  # Update year options dynamically
  observe({
    year_choices <- sort(unique(climate_eda$Year))
    year_opts <- c("All Years" = "All Years", as.character(year_choices))
    
    updateSelectInput(session, "eda_station_time_2",
                      choices = year_opts,
                      selected = "All Years")
  })
  
  # Filter data based on parameter and year
  filtered_station_data_2 <- reactive({
    req(input$eda_station_param_2)
    
    df <- climate_eda
    
    if (input$eda_station_time_2 != "All Years") {
      df <- df %>% filter(Year == as.numeric(input$eda_station_time_2))
    }
    
    return(df)
  })
  
  # Render Station-by-Month Variation Chart
  output$month_station_heatmap <- renderPlotly({
    req(input$eda_station_param_2)
    
    # Filtered data
    df <- filtered_station_data_2()
    param <- input$eda_station_param_2

    # Summarise the selected parameter
    summary_df <- df %>%
      group_by(Station, Month) %>%
      summarise(
        Avg_Value = mean(.data[[param]], na.rm = TRUE),
        .groups = "drop"
      )
    
    # Plotly heatmap
    plot_ly(
      data = summary_df,
      x = ~Month,
      y = ~Station,
      z = ~Avg_Value,
      type = "heatmap",
      colorscale = "Viridis",
      reversescale = TRUE,
      hovertemplate = paste(
        "Station: %{y}<br>",
        "Month: %{x}<br>",
        paste0("Avg of ", param, ": %{z:.2f}"),
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = paste("Monthly Average of", input$eda_station_param_2, "per Station for", input$eda_station_time_2),
        xaxis = list(title = "Month"),
        yaxis = list(title = "Station"),
        margin = list(b = 30)
      )
  })
  

  # CDA Tab Outputs
  # -------------------------------------------------------------
  # 1.1 Compare across stations - Normality
  # Update parameter choices dynamically from dataset
  observe({
    # Define mapping: Display Name = Column Name
    param_choices <- c(
      "Daily Rainfall" = "Daily Rainfall Total (mm)",
      "Mean Temperature" = "Mean Temperature (Celsius)",
      "Maximum Temperature" = "Maximum Temperature (Celsius)",
      "Minimum Temperature" = "Minimum Temperature (Celsius)",
      "Mean Wind Speed" = "Mean Wind Speed (km/h)"
    )
    
    # Filter only columns that actually exist in the dataset
    valid_params <- param_choices[param_choices %in% names(climate_eda)]
    
    updateSelectInput(session, "cda_station_param",
                      choices = valid_params,
                      selected = "Mean Temperature (Celsius)")
  })
  
  # ---- Dynamic Time Picker ----
  output$cda_station_time_picker <- renderUI({
    req(input$cda_station_time_type)
    selected_type <- input$cda_station_time_type
    
    min_date <- min(climate_eda$Date, na.rm = TRUE)
    max_date <- max(climate_eda$Date, na.rm = TRUE)
    
    if (selected_type == "Overall") {
      return(NULL)
    }
    
    if (selected_type == "Year") {
      # Year-only picker using selectInput
      year_choices <- sort(unique(lubridate::year(climate_eda$Date)))
      selectInput("cda_station_year", "Select Year", choices = year_choices, selected = year_choices[1])
      
    } else if (selected_type == "Month") {
      # Month-Year picker using airMonthpickerInput
      airMonthpickerInput(
        inputId = "cda_station_month",
        label = "Select month and year",
        value = min_date,
        minDate = min_date,
        maxDate = max_date
      )
    }
  })
  
  # ---- Dynamic Station List ----
  observe({
    station_choices <- sort(unique(climate_eda$Station))
    
    updateSelectizeInput(session, "cda_station_list",
                             choices = station_choices,
                             selected = NULL,
                             server = TRUE)
  })
  
  filtered_climate_data <- reactive({
    req(input$cda_station_list)
    
    df <- climate_eda %>% 
      filter(Station %in% input$cda_station_list)
    
    # Filter by time type
    if (input$cda_station_time_type == "Year") {
      req(input$cda_station_year)
      df <- df %>%
        filter(lubridate::year(Date) == input$cda_station_year)
      
    } else if (input$cda_station_time_type == "Month") {
      req(input$cda_station_month)
      selected_date <- input$cda_station_month
      df <- df %>%
        filter(lubridate::year(Date) == lubridate::year(selected_date),
               lubridate::month(Date) == lubridate::month(selected_date))
    }
    
    return(df)
  })
  
  observeEvent(input$cda_station_normality_btn, {
    df <- filtered_climate_data()
    req(nrow(df) > 0)
    
    param <- input$cda_station_param
    req(param %in% names(df))
    
    station_list <- unique(df$Station)
    
    # QQ Plots
    output$cda_station_normality_plot <- renderPlotly({
      plots <- lapply(station_list, function(station) {
        station_data <- df %>% filter(Station == station)
        
        values <- station_data[[param]]
        values <- unlist(values)
        values <- as.numeric(values)
        values <- na.omit(values)
        
        ggqq <- ggqqplot(values, title = paste("QQ Plot -", station))
        ggplotly(ggqq) %>% layout(title = list(text = paste0("<b>", station, "</b>")))
      })
      
      num_stations <- length(plots)
      nrows <- 2
      ncols <- ceiling(num_stations / nrows)
      
      subplot(plots,
              nrows = nrows,
              shareX = FALSE,
              shareY = FALSE,
              titleY = TRUE,
              margin = 0.03)
    })
    
    # Create test result dataframe
    test_result_df <- do.call(rbind, lapply(station_list, function(station) {
      station_data <- df %>% filter(Station == station)
      
      values <- station_data[[param]]
      values <- unlist(values)
      values <- as.numeric(values)
      values <- na.omit(values)
      
      ad <- tryCatch(nortest::ad.test(values), error = function(e) return(NULL))
      
      if (is.null(ad)) {
        data.frame(Station = station, A = NA, P_Value = NA)
      } else {
        data.frame(
          Station = station,
          A = round(ad$statistic, 4),
          P_Value = round(ad$p.value, 4)
        )
      }
    }))
    
    # Output the table
    output$cda_ad_table <- renderDataTable({
      test_result_df
    }, options = list(scrollX = TRUE, pageLength = 3))
  })
  
  
  
  
  # 1.2 Compare across stations - Statistical Test
  # output$cda_group_plot <- renderPlot({ ... })
  
  # 2.1 Compare across time - Normality
  # 2.2 Compare across time - Statistical Test


  # Time Series Forecasting Tab Outputs
  # -------------------------------------------------------------
  # output$ts_forecast_plot <- renderPlot({ ... })
  # output$ts_trend_plot <- renderPlot({ ... })
  

  # Geo-Spatial Tab Outputs
  # -------------------------------------------------------------
  # output$geo_map <- renderLeaflet({ ... })
  # output$extreme_event_plot <- renderPlot({ ... })
  
}
