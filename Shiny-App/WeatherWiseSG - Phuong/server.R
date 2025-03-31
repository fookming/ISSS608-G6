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

pacman::p_load(tidyverse, tmap, sf, sfdep, corrplot, terra,
               gstat, automap,  SpatialML, GWmodel, Metrics, ggrepel, leaflet)

# -----------------------------------------------------------------
# Importing data
# -----------------------------------------------------------------
climate <- read_csv("data/climate_final_2018_2024.csv")
climate$Date <- as.Date(with(climate, paste(Year, Month, Day, sep = "-")), format = "%Y-%m-%d")

climate_geo = climate
# Rename columns for easier references: Replace spaces with underscores and remove " ()" part
colnames(climate_geo) <- gsub(" \\(.*?\\)", "", colnames(climate_geo))
colnames(climate_geo) <- gsub(" ", "_", colnames(climate_geo))

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
# Read in Station details
station = read_csv('data/Station_Records.csv') |> 
  filter(Station_Type == "Full AWS Station")

# Read in Singapore Master Plan Subzone Boundary
mpsz = st_read(dsn = "data", layer = "MP14_SUBZONE_WEB_PL") |>
  st_transform(crs = 3414)

# Join Station with Climate dataframe
climate_sf <- climate |>
  left_join(station) |>
  st_as_sf(coords = c("Long","Lat"),
           crs= 4326) |>
  st_transform(crs = 3414)
# For Interpolation
# Create grid data
grid <- terra::rast(mpsz, 
                    nrows = 150, 
                    ncols = 250)
# Create xy list
xy <- terra::xyFromCell(grid, 
                        1:ncell(grid))
# Create simulation locations
coop <- st_as_sf(as.data.frame(xy), 
                 coords = c("x", "y"),
                 crs = st_crs(mpsz))
coop <- st_filter(coop, mpsz)

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
  

  # Geo-Spatial Tab Outputs
  # -------------------------------------------------------------
  # output$geo_map <- renderLeaflet({ ... })
  # output$extreme_event_plot <- renderPlot({ ... })
  
  # observeEvent(input$geo_extreme_month_input, {
  #   print(paste0(input$geo_extreme_month_input))
  # })
  # observeEvent(input$geo_extreme_year_input, {
  #   print(paste0(input$geo_extreme_year_input))
  # })
  # observeEvent(input$geo_extreme_weather_var, {
  #   print(paste0(input$geo_extreme_weather_var))
  # })
  
  # 1. Extreme Weather Events Map
  # Reactive expression to filter data based on selected weather variable and time selection
  filtered_data <- reactive({
    
    # Ensure climate is a valid dataframe
    climate_df <- as.data.frame(climate)
    
    # Ensure column exists in climate_df
    selected_var <- input$geo_extreme_weather_var
    
    if (!selected_var %in% colnames(climate_df)) {
      return(data.frame(Message = "Selected variable not found in dataset"))
    }
    
    # Filter based on time selection
    if (input$geo_extreme_time_interval == "Month") {
      req(input$geo_extreme_month_input)
      selected_date <- as.Date(paste0(input$geo_extreme_month_input, "-01"))
      climate_df <- climate_df %>% 
        filter(Year == year(selected_date) & Month == month(selected_date))
    } else if (input$geo_extreme_time_interval == "Year") {
      req(input$geo_extreme_year_input)
      selected_year <- as.integer(format(as.Date(input$geo_extreme_year_input, format="%Y"), "%Y"))
      climate_df <- climate_df %>% filter(Year == selected_year)
    }
    
    # Calculate extreme value based on selected weather variable
    extreme_value_df <- climate_df %>%
      group_by(Station) %>%
      summarize(
        Extreme_Value = max(!!sym(selected_var), na.rm = TRUE),
        Date_of_Extreme = Date[which.max(!!sym(selected_var))][1],
      ) |>
      ungroup()
    
    # Join with station data to bring in longitude and latitude
    climate_df <- extreme_value_df %>% 
      left_join(station, by = "Station")
    
    # Convert to spatial data frame
    climate_sf <- st_as_sf(climate_df, coords = c("Long", "Lat"), crs = 4326)
    
    # Select relevant columns
    climate_sf %>%
      mutate(
        Map_Label = paste(Station, Extreme_Value, Date_of_Extreme, sep = '\n'))
  })
  #tmap_mode('view')
  # Render the tmap
  output$geo_extreme_map <- renderLeaflet({
    climate_sf <- filtered_data()
    tmap_leaflet(
    tm_shape(climate_sf) +
      tm_bubbles(fill = "Extreme_Value", size = 1.2, 
                 fill.scale = tm_scale_continuous(values = "brewer.blues"),
                 fill.legend = tm_legend(title = "Extreme Values"),
                 hover = TRUE) +
      tm_text(text = "Map_Label", size = 0.8) +
      tm_layout(legend.outside = TRUE) +
      tm_title(paste("Extreme Values of ", input$geo_extreme_weather_var," for ", 
                     ifelse(input$geo_extreme_time_interval == 'Year', 
                            format(as.Date(input$geo_extreme_year_input), "%Y"), 
                            format(as.Date(paste0(input$geo_extreme_month_input, "-01")), 
                                   "%b-%Y")))),
    show = TRUE)
  })
  
  # 2.1 IDW Interpolation
  
  # Reactive expression to filter and prepare data for interpolation
  interpolated_data <- reactive({
    
    # Ensure climate is a valid dataframe
    climate_df <- as.data.frame(climate)
    
    # Ensure column exists in climate_df
    selected_var <- input$geo_inter_weather_var
    
    if (!selected_var %in% colnames(climate_df)) {
      return(data.frame(Message = "Selected variable not found in dataset"))
    }
    
    # Filter based on time selection
    if (input$geo_inter_time_interval == "Month") {
      selected_date <- as.Date(paste0(input$geo_inter_month_input, "-01"))
      climate_df <- climate_df %>% 
        filter(Year == year(selected_date) & Month == month(selected_date))
    } else if (input$geo_inter_time_interval == "Year") {
      selected_year <- as.integer(format(as.Date(input$geo_inter_year_input, format="%Y"), "%Y"))
      climate_df <- climate_df %>% 
        filter(Year == selected_year)
    }
    
    # Select relevant columns and ensure data types are correct
    climate_df <- climate_df %>%
      group_by(Station) |>
      summarise(total_val = if(grepl("Rainfall", selected_var)) {
        sum(.data[[selected_var]], na.rm = TRUE) # Sum or average based on the variable
      } else {
        mean(.data[[selected_var]], na.rm = TRUE)
      }) |>
      ungroup()
    
    # Join with station data to bring in longitude and latitude
    climate_df <- climate_df %>% 
      left_join(station, by = "Station")
    
    # Convert to spatial data frame
    climate_sf <- climate_df |>
      st_as_sf(coords = c("Long", "Lat"), crs = 4326) |>
      st_transform(crs = 3414)
    
    # Return the filtered spatial dataframe for interpolation
    climate_sf
  })
  # Observe "update_map" button click and render the map only after the button click
  observeEvent(input$geo_idw_update_map, {
  # Reactive expression to perform IDW interpolation
  idw_interpolated_map <- reactive({
    climate_sf <- interpolated_data()
    
    # Create gstat object for IDW interpolation
    res <- gstat(formula = total_val ~ 1 , 
                       locations = climate_sf,
                       nmax = input$geo_inter_nmax,
                       set = list(idp = input$geo_inter_idp))
    
    # Perform the IDW interpolation (setting a spatial grid)
    resp <- predict(res, coop)
    resp$x <- st_coordinates(resp)[,1]
    resp$y <- st_coordinates(resp)[,2]
    resp$pred <- resp$var1.pred
    
    pred <- rasterize(resp, grid, 
                             field = "pred", 
                             fun = "mean")
    
    # Return the interpolated raster
    pred
  })
  
  # # Observe "update_map" button click and render the map only after the button click
  # observeEvent(input$geo_idw_update_map, {
    # Ensure we have data to plot
    idw_raster <- idw_interpolated_map()

    # Render the tmap with interpolated data
    output$geo_inter_idw_map <- renderTmap({
      tm_shape(idw_raster) +
        tm_raster(col.legend = tm_legend(
          title = ifelse(input$geo_inter_weather_var == "Daily Rainfall Total (mm)",
                 paste("Total Rainfall (mm)"),
                 paste("Average ", input$geo_inter_weather_var)),
          title.size = 0.8,
          position = c('right','bottom'),
          width = 10,
          height = 15
        ),
                  col_alpha = 0.8, 
                  col.scale = tm_scale_continuous(
                    values = "brewer.greens")) +
        tm_title(paste("Interpolated Map of ", input$geo_inter_weather_var, 
                       " for ", 
                       ifelse(input$geo_inter_time_interval == 'Year', 
                              format(as.Date(input$geo_inter_year_input), "%Y"), 
                              format(as.Date(paste0(input$geo_inter_month_input, "-01")), 
                                     "%b-%Y"))))

    })
  })
  
  # 2.2 Kriging Interpolation
  # Observe "update_map" button click and render the map only after the button click
  observeEvent(input$geo_kriging_update_map, {
  # Reactive expression to perform manual Kriging interpolation
  manual_kriging_interpolated_map <- reactive({
    climate_sf <- interpolated_data()
    
    v <- variogram(total_val ~ 1, 
                   data = climate_sf)
    
    # Fit variogram
    fv <- fit.variogram(object = v,
                        model = vgm(
                          psill = input$geo_inter_psill, 
                          model = input$geo_inter_model,
                          range = input$geo_inter_range, 
                          nugget = input$geo_inter_nugget))
    
    # Create gstat object for Kriging interpolation
    res <- gstat(formula = total_val ~ 1 , 
                 locations = climate_sf,
                 model = fv)
    
    # Perform the Kriging interpolation (setting a spatial grid)
    resp <- predict(res, coop)
    resp$x <- st_coordinates(resp)[,1]
    resp$y <- st_coordinates(resp)[,2]
    resp$pred <- resp$var1.pred
    resp$pred <- resp$pred
    
    pred <- rasterize(resp, grid, 
                      field = "pred")
    
    # Return the interpolated raster
    pred
  })
  # Reactive expression to perform automated Kriging interpolation
  auto_kriging_interpolated_map <- reactive({
    climate_sf <- interpolated_data()
    
    v <- autofitVariogram(total_val ~ 1, 
                   climate_sf)
    
    # Create gstat object for Kriging interpolation
    res <- gstat(formula = total_val ~ 1 , 
                 data = climate_sf,
                 model = v$var_model)
    
    # Perform the Kriging interpolation (setting a spatial grid)
    resp <- predict(res, coop)
    resp$x <- st_coordinates(resp)[,1]
    resp$y <- st_coordinates(resp)[,2]
    resp$pred <- resp$var1.pred
    resp$pred <- resp$pred
    
    pred <- rasterize(resp, grid, 
                      field = "pred")
    
    # Return the interpolated raster
    pred
  })
  
  # # Observe "update_map" button click and render the map only after the button click
  # observeEvent(input$geo_kriging_update_map, {
    
    # Assign manual kriging raster
    manual_kriging_raster <- manual_kriging_interpolated_map()
    # Render the tmap with interpolated data
    output$geo_kriging_manual_map <- renderTmap({
      tm_shape(manual_kriging_raster) +
        tm_raster(col.legend = tm_legend(
          title = ifelse(input$geo_inter_weather_var == "Daily Rainfall Total (mm)",
                         paste("Total Rainfall (mm)"),
                         paste("Average ", input$geo_inter_weather_var)),
          title.size = 0.8,
          position = tm_pos_out(cell.h = "center", cell.v = "bottom"),
          width = 8,
          height = 10
        ),
        col_alpha = 0.8, 
        col.scale = tm_scale_continuous(
          values = "brewer.greens")) +
        tm_title(paste("Interpolated Map of ", input$geo_inter_weather_var, 
                       " for ", 
                       ifelse(input$geo_inter_time_interval == 'Year', 
                              format(as.Date(input$geo_inter_year_input), "%Y"), 
                              format(as.Date(paste0(input$geo_inter_month_input, "-01")), 
                                     "%b-%Y"))))
      
    })
    # Assign auto kriging raster
    auto_kriging_raster <- auto_kriging_interpolated_map()
    # Render the tmap with interpolated data
    output$geo_kriging_auto_map <- renderTmap({
      tm_shape(auto_kriging_raster) +
        tm_raster(col.legend = tm_legend(
          title = ifelse(input$geo_inter_weather_var == "Daily Rainfall Total (mm)",
                         paste("Total Rainfall (mm)"),
                         paste("Average ", input$geo_inter_weather_var)),
          title.size = 0.8,
          position = tm_pos_out(cell.h = "center", cell.v = "bottom"),
          width = 8,
          height = 10
        ),
        col_alpha = 0.8, 
        col.scale = tm_scale_continuous(
          values = "brewer.greens")) +
        tm_title(paste("Interpolated Map of ", input$geo_inter_weather_var, 
                       " for ", 
                       ifelse(input$geo_inter_time_interval == 'Year', 
                              format(as.Date(input$geo_inter_year_input), "%Y"), 
                              format(as.Date(paste0(input$geo_inter_month_input, "-01")), 
                                     "%b-%Y"))))
      
    })
  })
  
  # # GW Predictive Modelling
  # 
  # # Reactive function to filter training and test data
  # filtered_data <- reactive({
  #    # Convert climate data to dataframe
  #   climate_df <- as.data.frame(climate)
  #   
  #   # Filter training data (Oct 2023 - Sep 2024)
  #   train_df <- climate_df %>%
  #     filter(Date >= as.Date("2024-09-01") & Date <= as.Date("2024-11-30"))
  #   
  #   # Filter test data (Oct 2024 - Dec 2024)
  #   test_df <- climate_df %>%
  #     filter(Date >= as.Date("2024-12-01") & Date <= as.Date("2024-12-31"))
  #   
  #   # Define dependent and independent variables
  #   dependent_var <- input$geo_fc_weather_var
  #   independent_vars <- switch(dependent_var,
  #                              "Daily Rainfall Total (mm)" = c("Minimum Temperature (Celsius)", "Mean Wind Speed (km/h)"),
  #                              "Mean Temperature (Celsius)" = c("Daily Rainfall Total (mm)", "Mean Wind Speed (km/h)"),
  #                              "Mean Wind Speed (km/h)" = c("Daily Rainfall Total (mm)", "Minimum Temperature (Celsius)"))
  #   
  #   # Convert to spatial data
  #   train_sf <- train_df |>
  #     left_join(station, by = "Station") |>
  #     st_as_sf(coords = c("Long", "Lat"), crs = 4326) |>
  #     st_transform(crs = 3414)
  #   test_sf <- test_df |>
  #     left_join(station, by = "Station") |>
  #     st_as_sf(coords = c("Long", "Lat"), crs = 4326) |>
  #     st_transform(crs = 3414)
  #   
  #   list(train_sf = train_sf, test_sf = test_sf, dependent_var = dependent_var, independent_vars = independent_vars)
  # })
  # 
  # # ReactiveVal to store model results (only updates when button is clicked)
  # model_results <- reactiveVal(NULL)
  # 
  # # Observe button click and trigger model training
  # observeEvent(input$geo_fc_update_chart, {
  #   data <- filtered_data()
  #  
  #   train_sf <- data$train_sf
  #   test_sf <- data$test_sf
  #   dependent_var <- data$dependent_var
  #   independent_vars <- data$independent_vars
  #   
  #   # Define formula for GW-RF model
  #   formula_rf <- as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + ")))
  #   
  #   # Select bandwidth based on kernel type
  #   bandwidth <- if (input$geo_fc_kernel == "adaptive") input$geo_fc_adapt_bw else input$geo_fc_fixed_bw
  #   
  #   # Train GW-RF model
  #   gwrf_model <- grf(
  #     formula = formula_rf,
  #     data = train_sf,
  #     kernel = input$geo_fc_kernel,
  #     bw = bandwidth
  #   )
  #   
  #   # Predict on test data
  #   test_sf$Predicted <- predict(gwrf_model, newdata = test_sf)
  #   
  #   # Store model results
  #   model_results(list(model = gwrf_model, test_sf = test_sf, dependent_var = dependent_var))
  # })
  # 
  # # Render scatter plot
  # output$geo_fc_chart <- renderPlot({
  #   results <- model_results()
  #   req(results)
  #   
  #   test_sf <- results$test_sf
  #   dependent_var <- results$dependent_var
  #   
  #   ggplot(test_sf, aes_string(x = dependent_var, y = "Predicted")) +
  #     geom_point(color = "blue", alpha = 0.6) +
  #     geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  #     theme_minimal() +
  #     labs(title = paste("Actual vs Predicted", dependent_var),
  #          x = "Actual Values",
  #          y = "Predicted Values") +
  #     theme(plot.title = element_text(hjust = 0.5))
  # })
  # 
  # # Render MAE and RMSE only after button click
  # output$geo_fc_mae <- renderValueBox({
  #   results <- model_results()
  #   req(results)
  #   
  #   test_sf <- results$test_sf
  #   dependent_var <- results$dependent_var
  #   
  #   mae_value <- mae(test_sf[[dependent_var]], test_sf$Predicted)
  #   
  #   valueBox(round(mae_value, 2), "MAE", icon = icon("chart-line"), color = "blue")
  # })
  # 
  # output$geo_fc_rmse <- renderValueBox({
  #   results <- model_results()
  #   req(results)
  #   
  #   test_sf <- results$test_sf
  #   dependent_var <- results$dependent_var
  #   
  #   rmse_value <- rmse(test_sf[[dependent_var]], test_sf$Predicted)
  #   
  #   valueBox(round(rmse_value, 2), "RMSE", icon = icon("chart-bar"), color = "green")
  # })
}

