# ---------------------------------------------------------
# Load Packages
# ---------------------------------------------------------
library(methods) # Required for S4 methods (like coerce)
library(sp) # Required for STFDF, Raster* classes
library(raster) # Used internally by gstat; load before gstat
library(gstat) # Load after sp and raster to avoid coerce error

# Core Shiny App Packages
library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(plotly)

# EFM
library(shinyjs) # clearly add this line explicitly
library(forecast)
library(memoise)
library(cachem)
library(tseries)
library(prophet)
library(ggplot2)
library(lubridate)

# Phuong
library(tidyverse)
library(tmap)
library(sf)
library(sfdep)
library(corrplot)
library(terra)
library(automap)
library(SpatialML)
library(GWmodel)
library(Metrics)
library(ggrepel)
library(leaflet)

# ---------------------------------------------------------
# Dashboard Page Layout
# ---------------------------------------------------------

source("forecasting_ui.R")

dashboardPage(
  title = "WeatherWise Singapore",
  header = dashboardHeader(
    title = dashboardBrand(
      title = "WeatherWise SG",
      color = "info",
      href = "#"
    ),
    skin = "light",
    fixed = TRUE,
    controlbarIcon = NULL,
    fullscreenIcon = NULL,
    rightUi = NULL
  ),


  # Sidebar Menu (Navigation)
  sidebar = dashboardSidebar(
    skin = "light",
    status = "info",
    title = NULL,
    bs4SidebarMenu(
      bs4SidebarMenuItem("Landing Page", tabName = "landing", icon = icon("home")),
      bs4SidebarMenuItem("Exploratory Data Analysis",
        icon = icon("chart-area"),
        bs4SidebarMenuSubItem("Overview", tabName = "eda_overview"),
        bs4SidebarMenuSubItem("Seasonality Analysis", tabName = "eda_seasonality"),
        bs4SidebarMenuSubItem("Station Comparison", tabName = "eda_station")
      ),
      bs4SidebarMenuItem("Confirmatory Data Analysis",
        icon = icon("balance-scale"),
        bs4SidebarMenuSubItem("Compare across stations", tabName = "cda_station"),
        bs4SidebarMenuSubItem("Compare across time", tabName = "cda_time")
      ),
      bs4SidebarMenuItem("Time Series Forecasting", tabName = "forecasting", icon = icon("chart-line")),
      bs4SidebarMenuItem("Geo-Spatial Analysis",
        icon = icon("globe-asia"),
        bs4SidebarMenuSubItem("Extreme Weather Events", tabName = "geo_extreme"),
        bs4SidebarMenuSubItem("Spatial Interpolation", tabName = "geo_interpolation")
      )
    )
  ),

  # Main Body (Tab Content Area)
  body = dashboardBody(
    useShinyjs(), # <-- clearly put this at the top of dashboardBody

    # CSS overlay style
    tags$head(
      tags$style(HTML("
        /* Change the header bar and title background to match 'info' status */
        .main-header.navbar,
        .main-header .brand-link {
          background-color: #17a2b8 !important;  /* Bootstrap 'info' color */
        }

        /* Sidebar font sizes */
        .nav-sidebar .nav-link,
        .nav-sidebar .nav-link span {
          font-size: 14px !important;
        }

        .nav-sidebar .nav-treeview .nav-link {
          font-size: 14px !important;
        }

        /* Reduce font size of labels for inputs */
        .shiny-input-container > label {
          font-size: 0.88em !important;
        }

        /* Reduce font size inside selectize/dropdown inputs */
        .selectize-input,
        .selectize-dropdown-content {
          font-size: 0.90em !important;
        }

        /* Optional: reduce font size for all input fields generally */
        .form-control {
          font-size: 0.85em !important;
        }

        #loading-overlay {
          position: fixed;
          top: 0; left: 0;
          width: 100%; height: 100%;
          background-color: rgba(255, 255, 255, 0.7);
          z-index: 9999;
          display: none;
          text-align: center;
          padding-top: 200px;
          font-size: 22px;
          color: #blue;
        }
      "))
    ),

    # The overlay div
    div(id = "loading-overlay", "Loading ... Please wait !"),

    # 3. Add the script to toggle the overlay
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleLoading', function(isLoading) {
        var el = document.getElementById('loading-overlay');
        if (isLoading) {
          el.style.display = 'block';
        } else {
          el.style.display = 'none';
        }
      });
    ")),

    # Tabs
    bs4TabItems(
      bs4TabItem(
        tabName = "landing",
        fluidRow(
          # Left Side
          column(
            width = 8,
            bs4Card(
              title = "Welcome !",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE,
              p("Singapore’s tropical climate presents complex challenges for urban planning, infrastructure, and risk management due to high temperatures, intense humidity, and frequent rainfall. This dashboard offers a comprehensive, standalone weather analytics solution tailored to Singapore’s unique climate. It explores historical weather trends, integrates advanced statistical methods, spatial visualisation, and predictive modelling to empower users with actionable insights for strategic and climate-resilient planning across industries."),
              p(HTML("The historical data was obtained from the <a href = 'https://www.weather.gov.sg/climate-historical-daily/'>Meteorological Service Singapore</a> site."))
            ),
            bs4Card(
              title = "Dashboard Modules Overview",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = FALSE,
              icon = icon("layer-group"),
              p(
                strong("1. Exploratory Data Analysis:"),
                "Explore Singapore’s weather patterns and analyze temperature, rainfall, and wind patterns across time and stations through interactive charts and visual summaries."
              ),
              p(
                strong("2. Confirmatory Data Analysis:"),
                "Statistical testing to validate patterns observed in the EDA section. Users can test for significant differences between groups (e.g., stations, time periods) and explore normality and distributional assumptions."
              ),
              p(
                strong("3. Time Series Forecasting:"),
                "Identify long-term and seasonal patterns using rolling averages and seasonal plots. Includes decomposition techniques to separate trend, seasonality, and residuals. Users can statistically analyze weather fluctuations and forecast future trends using models like ARIMA, Prophet, or Exponential Smoothing to assess future climate risks and variations."
              ),
              p(
                strong("4. Geo-Spatial Analysis:"),
                "Provides interactive map to explore extreme events by Station and Map Visualizations of interpolated weather measurements for estimation of weather conditions at locations where direct measurements are unavailable. Plot and compare interpolated map using different methods: Inverse Distance Weighted and Ordinary Kriging. "
              )
            )
          ),

          # Right Side
          column(
            width = 4,

            # First row: Two summary boxes side by side
            fluidRow(
              bs4ValueBox(
                value = HTML("<span style='font-size: 25px; font-weight: bold;'>15</span>"),
                subtitle = "Weather Stations",
                icon = icon("map-marker-alt", class = "fa-1x"),
                color = "olive",
                width = 6
              ),
              bs4ValueBox(
                value = HTML("<span style='font-size: 23px; font-weight: bold;'>2018–2024</span>"),
                subtitle = "Data Coverage",
                icon = icon("calendar-alt", class = "fa-1x"),
                color = "olive",
                width = 6
              )
            ),

            # Second row: One full-width summary box
            fluidRow(
              bs4ValueBox(
                value = HTML("<span style='font-size: 23px; font-weight: bold;'>5</span>"),
                subtitle = HTML("Weather Parameters: <span style='font-size: 14px;'>Rainfall Total, Temperature (Mean/Max/Min), Mean Wind Speed</span>"),
                icon = icon("database", class = "fa-1x"),
                color = "olive",
                width = 12
              )
            ),

            # Third row: First image full-width
            fluidRow(
              column(
                width = 12,
                bs4Card(
                  title = "Time Series Analysis",
                  width = 12,
                  solidHeader = TRUE,
                  img(src = "trend_plot.png", width = "100%")
                )
              )
            ),

            # Fourth row: Second image full-width
            fluidRow(
              column(
                width = 12,
                bs4Card(
                  title = "Geospatial Interpolation",
                  width = 12,
                  solidHeader = TRUE,
                  img(src = "geo_map.png", width = "100%")
                )
              )
            )
          )
        )
      ),
      bs4TabItem(
        tabName = "eda_overview",
        fluidRow(

          # -- Left Column: Filters --
          column(
            width = 2,

            # Info Box (Optional)
            box(
              title = tags$span(icon("info-circle"), "About"),
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              status = "info",
              solidHeader = TRUE,
              p("Presents high-level summaries such as yearly/monthly/station trends and key statistics (e.g., hottest day, rainiest month)")
            ),

            # Filter Box
            box(
              title = "Filters",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              selectInput(
                inputId = "eda_overview_parameter",
                label = "Select Parameter:",
                choices = c("Loading..." = "loading"),
                width = "100%"
              )
            )
          ),

          # -- Right Column: Charts and Value Boxes --
          column(
            width = 10,
            box(
              title = uiOutput("eda_overview_title"),
              width = 12,
              solidHeader = TRUE,
              status = "primary",

              # -- Top Row: Line Chart + Value Boxes --
              fluidRow(
                column(
                  width = 6,
                  plotlyOutput("overview_yearcompare_plot", height = "280px")
                ),
                column(
                  width = 6,
                  fluidRow(
                    column(width = 6, style = "padding-right: 3px; padding-bottom: 8px;", uiOutput("overview_box_1")),
                    column(width = 6, style = "padding-left: 3px; padding-bottom: 8px;", uiOutput("overview_box_2"))
                  ),
                  fluidRow(
                    column(width = 6, style = "padding-right: 3px;", uiOutput("overview_box_3")),
                    column(width = 6, style = "padding-left: 3px;", uiOutput("overview_box_4"))
                  )
                )
              ),


              # -- Bottom Row: 2 Charts Side-by-Side --
              fluidRow(
                column(
                  width = 6,
                  plotlyOutput("overview_yearly_plot", height = "230px")
                ),
                column(
                  width = 6,
                  plotlyOutput("overview_5_stations_plot", height = "230px")
                )
              )
            )
          )
        )
      ),
      bs4TabItem(
        tabName = "eda_seasonality",
        fluidRow(

          # ---- Row 1: About Box ----
          column(
            width = 12,
            box(
              title = tags$span(icon("info-circle"), "About"),
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              status = "info",
              solidHeader = TRUE,
              p("Highlights seasonal trends and repeating patterns across months and years for various parameters and stations.")
            )
          ),

          # ---- Row 2: Charts Box with Dropdowns Inside ----
          column(
            width = 12,
            box(
              title = "Seasonality Charts",
              width = 12,
              solidHeader = TRUE,
              status = "primary",

              # -- Filters in Single Line Centered --
              div(
                style = "display: flex; justify-content: center; gap: 40px; padding: 1px 0;",

                # Parameter Dropdown + Label
                div(
                  style = "display: flex; align-items: center; gap: 5px;",
                  tags$label("Select Parameter:", style = "margin-bottom: 0;"),
                  selectInput("eda_seasonality_parameter",
                    label = NULL,
                    choices = c("Loading..." = "loading"), width = "200px"
                  )
                ),

                # Station Dropdown + Label
                div(
                  style = "display: flex; align-items: center; gap: 5px;",
                  tags$label("Select Station:", style = "margin-center: 0;"),
                  selectInput("eda_seasonality_station",
                    label = NULL,
                    choices = c("Loading..." = "loading"), width = "200px"
                  )
                )
              ),

              # -- Chart Section Below --
              fluidRow(
                column(width = 6, plotlyOutput("seasonal_trend_plot")),
                column(width = 6, plotOutput("seasonal_cycle_plot"))
              )
            )
          )
        )
      ),
      bs4TabItem(
        tabName = "eda_station",
        tabsetPanel(
          id = "eda_station_tabs", # input$station_tabs will hold the selected tab
          type = "tabs", # (optional: use "pills" if you prefer that look)

          # ---- Tab 1 ----
          tabPanel(
            "Station-wise Distribution",
            br(),
            fluidRow(
              column(
                width = 2,

                #  Info Box (collapsible)
                box(
                  title = tags$span(icon("info-circle"), "About"),
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  status = "info",
                  solidHeader = TRUE,
                  p("This view shows how weather parameters vary across stations. Use the filters below to explore distributions.")
                ),

                #  Parameter Control Panel
                box(
                  title = "Filters",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  selectInput("eda_station_param_1", "Select Parameter", choices = c("Loading..." = "loading")), # set dynamically in server

                  selectInput("eda_station_time_1", "Select Time Interval", choices = c("Loading..." = "loading")) # set dynamically in server
                )
              ),
              column(
                width = 10,
                fluidRow(
                  box(
                    title = "Chart", width = 12, solidHeader = TRUE,
                    status = "primary", collapsible = FALSE,
                    plotlyOutput("station_dist_plot")
                  )
                )
              )
            )
          ),

          # ---- Tab 2 ----
          tabPanel(
            "Month-by-Station Variation",
            br(),
            fluidRow(
              column(
                width = 2,

                #  Info Box (collapsible)
                box(
                  title = tags$span(icon("info-circle"), "About"),
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  status = "info",
                  solidHeader = TRUE,
                  p("This view shows heatmap for station-by-month. Use the filters below to explore for other years.")
                ),

                #  Parameter Control Panel
                box(
                  title = "Filters",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  selectInput("eda_station_param_2", "Select Parameter", choices = c("Loading..." = "loading")), # set dynamically in server

                  selectInput("eda_station_time_2", "Select Time Interval", choices = c("Loading..." = "loading")) # set dynamically in server
                )
              ),
              column(
                width = 10,
                fluidRow(
                  box(
                    title = "Chart", width = 12, solidHeader = TRUE,
                    status = "primary", collapsible = FALSE,
                    plotlyOutput("month_station_heatmap")
                  )
                )
              )
            )
          )
        )
      ),
      bs4TabItem(
        tabName = "cda_station",
        fluidRow(

          # --- Column 1: About + General Filters ---
          column(
            width = 2,

            # About Box
            box(
              title = tags$span(icon("info-circle"), "About"),
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              solidHeader = TRUE,
              status = "info",
              p("This page allows you to check for normality and run statistical tests between stations for a selected metric and period.")
            ),

            # Filter Box
            box(
              title = "Filters",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              selectInput("cda_station_param", "Select Parameter", choices = c("Loading..." = "loading")),
              selectInput("cda_station_time_type", "Select Time Interval",
                choices = c("Year", "Month"), selected = "Year"
              ),
              uiOutput("cda_station_time_picker"),
              selectizeInput("cda_station_list", "Select Station(s)",
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Select...", plugins = list("remove_button"))
              )
            )
          ),

          # --- Column 2: Statistical Test Options ---
          column(
            width = 2,
            box(
              title = "Statistical Test Parameters",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              selectInput("cda_station_test_type", "Test Type",
                choices = c("Parametric", "Non-Parametric", "Robust", "Bayes-Factor"),
                selected = "Non-Parametric"
              ),
              selectInput("cda_station_conf_level", "Confidence Level",
                choices = c("90%", "95%", "99%"),
                selected = "95%"
              ),
              actionButton("cda_station_test_btn", "Run Test", class = "btn-outline-primary", width = "100%")
            )
          ),

          # --- Column 3: Outputs (Test Results) ---
          column(
            width = 8,

            # Normality Table (collapsible)
            box(
              title = "Anderson-Darling Normality Test Result",
              width = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              status = "primary",
              dataTableOutput("cda_ad_table")
            ),

            # Final Test Output Plot
            box(
              title = "Statistical Test Results",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              plotOutput("cda_station_test_plot", height = "400px")
            )
          )
        )
      ),
      bs4TabItem(
        tabName = "cda_time",
        fluidRow(

          # --- Column 1: About + General Filters ---
          column(
            width = 2,

            # About Box
            box(
              title = tags$span(icon("info-circle"), "About"),
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              solidHeader = TRUE,
              status = "info",
              p("This page allows you to check for normality and run statistical tests for a metric across time (e.g., months, years) within a station")
            ),

            # Filter Box
            box(
              title = "Filters",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              selectInput("cda_time_param", "Select Parameter", choices = c("Loading..." = "loading")),
              selectizeInput("cda_time_station_list", "Select Station",
                choices = NULL,
                multiple = FALSE,
                options = list(placeholder = "Select...", plugins = list("remove_button"))
              ),
              # Time Comparison Type
              selectInput("cda_time_compare_type", "Compare Across",
                choices = c("By Year", "By Month (All Years)", "By Month (Selected Year)"),
                selected = "By Year"
              ),

              # Dynamic Time Picker (depends on option above)
              uiOutput("cda_time_picker")
            )
          ),

          # --- Column 2: Statistical Test Options ---
          column(
            width = 2,
            box(
              title = "Statistical Test Parameters",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              selectInput("cda_time_test_type", "Test Type",
                choices = c("Parametric", "Non-Parametric", "Robust", "Bayes-Factor"),
                selected = "Non-Parametric"
              ),
              selectInput("cda_time_conf_level", "Confidence Level",
                choices = c("90%", "95%", "99%"),
                selected = "95%"
              ),
              actionButton("cda_time_test_btn", "Run Test", class = "btn-outline-primary", width = "100%")
            )
          ),

          # --- Column 3: Outputs (Test Results) ---
          column(
            width = 8,

            # Normality Table (collapsible)
            box(
              title = "Anderson-Darling Normality Test Result",
              width = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              status = "primary",
              dataTableOutput("cda_time_ad_table")
            ),

            # Final Test Output Plot
            box(
              title = "Statistical Test Results",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              plotOutput("cda_time_test_plot", height = "400px")
            )
          )
        )
      ),
      bs4TabItem(tabName = "forecasting", forecasting_ui("forecasting"), icon = icon("chart-line")),
      bs4TabItem(
        tabName = "geo_extreme",
        fluidRow(
          # Left panel - dropdown filter
          column(
            width = 3,

            #  Info Box (collapsible)
            box(
              title = tags$span(icon("info-circle"), "About"),
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              status = "info",
              solidHeader = TRUE,
              p("This interactive map shows extreme weather events according to selection.")
            ),

            #  Parameter Control Panel
            box(
              title = "Filters",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = FALSE,
              selectInput(
                inputId = "geo_extreme_weather_var",
                label = "Select Parameter",
                choices = c(
                  "Daily Rainfall" = "Daily Rainfall Total (mm)",
                  "Mean Temperature" = "Mean Temperature (Celsius)",
                  "Maximum Temperature" = "Maximum Temperature (Celsius)",
                  "Minimum Temperature" = "Minimum Temperature (Celsius)",
                  "Mean Wind Speed" = "Mean Wind Speed (km/h)"
                ),
                selected = "Mean Temperature (Celsius)"
              ),
              selectInput(
                inputId = "geo_extreme_time_interval",
                label = "Select Time Interval",
                choices = c("Month", "Year"),
                selected = "Year"
              ),
              # Show Month-Year Picker when "Month" is selected
              conditionalPanel(
                condition = "input.geo_extreme_time_interval == 'Month'",
                airDatepickerInput("geo_extreme_month_input", "Select Year & Month",
                  view = "months",
                  minView = "months",
                  minDate = "2018-01-01", maxDate = "2024-12-31",
                  dateFormat = "yyyy-MM",
                  autoClose = TRUE,
                  value = "2024-12"
                )
              ),
              # Show Year Picker when "Year" is selected
              conditionalPanel(
                condition = "input.geo_extreme_time_interval == 'Year'",
                airDatepickerInput("geo_extreme_year_input", "Select Year",
                  view = "years",
                  minView = "years",
                  minDate = "2018-01-01", maxDate = "2024-12-31",
                  dateFormat = "yyyy",
                  autoClose = TRUE,
                  value = "2024"
                )
              )
            )
          ),

          # Main panel: charts, value boxes, etc.
          column(
            width = 9,
            fluidRow(
              box(
                title = "Interactive Map", width = 12, solidHeader = TRUE,
                status = "primary", collapsible = FALSE,
                leafletOutput("geo_extreme_map")
              )
            )
            # Add more charts or value boxes here later
          )
        )
      ),
      bs4TabItem(
        tabName = "geo_interpolation",
        fluidRow(
          # Left panel - dropdown filter
          column(
            width = 2,

            #  Info Box (collapsible)
            box(
              title = tags$span(icon("info-circle"), "About"),
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              status = "info",
              solidHeader = TRUE,
              p("This map visualize interpolated values of selected weather measurements on Singapore map")
            ),

            #  Parameter Control Panel
            box(
              title = "Filters",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = FALSE,
              selectInput(
                inputId = "geo_inter_weather_var",
                label = "Select Parameter",
                choices = c(
                  "Daily Rainfall" = "Daily Rainfall Total (mm)",
                  "Mean Temperature" = "Mean Temperature (Celsius)",
                  "Maximum Temperature" = "Maximum Temperature (Celsius)",
                  "Minimum Temperature" = "Minimum Temperature (Celsius)",
                  "Mean Wind Speed" = "Mean Wind Speed (km/h)"
                ),
                selected = "Mean Temperature (Celsius)"
              ),
              selectInput(
                inputId = "geo_inter_time_interval",
                label = "Select Time Interval",
                choices = c("Month", "Year"),
                selected = "Year"
              ),
              # Show Month-Year Picker when "Month" is selected
              conditionalPanel(
                condition = "input.geo_inter_time_interval == 'Month'",
                airDatepickerInput("geo_inter_month_input", "Select Year & Month",
                  view = "months",
                  minView = "months",
                  minDate = "2018-01-01", maxDate = "2024-12-31",
                  dateFormat = "yyyy-MM",
                  autoClose = TRUE,
                  value = "2024-12"
                )
              ),
              # Show Year Picker when "Year" is selected
              conditionalPanel(
                condition = "input.geo_inter_time_interval == 'Year'",
                airDatepickerInput("geo_inter_year_input", "Select Year",
                  view = "years",
                  minView = "years",
                  minDate = "2018-01-01", maxDate = "2024-12-31",
                  dateFormat = "yyyy",
                  autoClose = TRUE,
                  value = "2024"
                )
              )
            )
          ),

          # Main panel: charts, value boxes, etc.
          column(
            width = 10,
            tabsetPanel(
              id = "geo_inter_tabs",
              type = "tabs",
              tabPanel(
                "Inverse Distance Weighted Interpolation", br(),
                fluidRow(
                  column(
                    width = 2,
                    box(
                      title = "Interpolation Parameters",
                      width = 12,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsible = FALSE,
                      sliderInput(
                        inputId = "geo_inter_nmax",
                        label = "Number of neighbors",
                        min = 1,
                        max = 10,
                        value = 5,
                        step = 1
                      ),
                      sliderInput(
                        inputId = "geo_inter_idp",
                        label = "Inverse distance power",
                        min = 0,
                        max = 2.5,
                        value = 1,
                        step = 0.1
                      ),
                      actionButton(
                        inputId = "geo_idw_update_map",
                        label = "Update Map",
                        class = "btn-primary"
                      )
                    )
                  ),
                  column(
                    width = 10,
                    box(
                      title = "Static Map", width = 12,
                      solidHeader = TRUE, status = "primary",
                      collapsible = FALSE,
                      tmapOutput("geo_inter_idw_map")
                    )
                  )
                )
              ),
              tabPanel(
                "Ordinary Kriging Interpolation", br(),
                fluidRow(
                  column(
                    width = 2,
                    box(
                      title = "Interpolation Parameters",
                      width = 12,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsible = FALSE,
                      selectInput(
                        inputId = "geo_inter_model",
                        label = "model",
                        choices = c(
                          "Exp", "Sph", "Gau", "Mat", "Nug", "Exc",
                          "Ste", "Cir", "Lin", "Bes", "Pen", "Per",
                          "Wav", "Hol", "Log", "Pow", "Spl"
                        ),
                        selected = "Sph"
                      ),
                      sliderInput(
                        inputId = "geo_inter_psill",
                        label = "psill",
                        min = 0.5,
                        max = 10,
                        value = 0.5,
                        step = 0.5
                      ),
                      sliderInput(
                        inputId = "geo_inter_range",
                        label = "range",
                        min = 1000,
                        max = 10000,
                        value = 5000,
                        step = 1000
                      ),
                      sliderInput(
                        inputId = "geo_inter_nugget",
                        label = "nugget",
                        min = 0.1,
                        max = 10,
                        value = 0.1,
                        step = 0.1
                      ),
                      actionButton(
                        inputId = "geo_kriging_update_map",
                        label = "Update Map",
                        class = "btn-primary"
                      )
                    )
                  ),
                  column(
                    width = 5,
                    box(
                      title = "Manually Fitted Variogram", width = 12,
                      solidHeader = TRUE, status = "primary",
                      collapsible = FALSE,
                      tmapOutput("geo_kriging_manual_map")
                    )
                  ),
                  column(
                    width = 5,
                    box(
                      title = "Automatically Fitted Variogram",
                      width = 12,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsible = FALSE,
                      tmapOutput("geo_kriging_auto_map")
                    )
                  )
                )
              )
            )
          )
        )
      )

      # bs4TabItem(tabName = "geo_modelling",
      #            fluidRow(
      #              # Left panel - dropdown filter
      #              column(
      #                width = 2,
      #
      #                #  Info Box (collapsible)
      #                box(
      #                  title = tags$span(icon("info-circle"), "About"),
      #                  width = 12,
      #                  collapsible = TRUE,
      #                  collapsed = TRUE,
      #                  status = "info",
      #                  solidHeader = TRUE,
      #                  p("This view shows performance comparison between Non-spatial and Geographically Weighted Random Forest.")
      #                ),
      #
      #                #  Parameter Control Panel
      #                box(
      #                  title = "Filters",
      #                  width = 12,
      #                  solidHeader = TRUE,
      #                  status = "primary",
      #                  collapsible = FALSE,
      #                  selectInput(
      #                    inputId = "geo_fc_weather_var",
      #                    label = "Select Parameter",
      #                    choices = c("Daily Rainfall" = "Daily Rainfall Total (mm)",
      #                                "Mean Temperature" = "Mean Temperature (Celsius)",
      #                                "Maximum Temperature" = "Maximum Temperature (Celsius)",
      #                                "Minimum Temperature" = "Minimum Temperature (Celsius)",
      #                                "Mean Wind Speed" = "Mean Wind Speed (km/h)"),
      #                    selected = "Mean Temperature (Celsius)"
      #                  ),
      #
      #                  selectInput(
      #                    inputId = "geo_fc_kernel",
      #                    label = "Select Kernel",
      #                    choices = c("adaptive", "fixed"),
      #                    selected = "adaptive"
      #                  ),
      #                  # Show Adaptive bw range when adaptive kernel is selected
      #                  conditionalPanel(
      #                    condition = "input.geo_fc_kernel == 'adaptive'",
      #                    sliderInput(inputId = "geo_fc_adapt_bw",
      #                                label = "bandwidth (# neighbors)",
      #                                min = 300,
      #                                max = 2000,
      #                                value = 350,
      #                                step = 50)
      #                  ),
      #                  # Show Fixed bw range when fixed kernel is selected
      #                  conditionalPanel(
      #                    condition = "input.geo_fc_kernel == 'fixed'",
      #                    sliderInput(inputId = "geo_fc_fixed_bw",
      #                                label = "bandwidth (meters)",
      #                                min = 1000,
      #                                max = 20000,
      #                                value = 1000,
      #                                step = 1000)
      #                  ),
      #                  actionButton(inputId = "geo_fc_update_chart",
      #                               label = "Update Charts",
      #                               class = "btn-primary")
      #                )
      #              ),
      #
      #              # Main panel: charts, value boxes, etc.
      #              column(
      #                width = 10,
      #                fluidRow(
      #                  box(title = "Scatter Plots of Actual vs Predicted Values",
      #                      width = 12,
      #                      solidHeader = TRUE,
      #                      status = "primary",
      #                      collapsible = FALSE,
      #                      plotOutput("geo_fc_chart")
      #                  )
      #                )
      #                # Add more charts or value boxes here later
      #              )
      #            )
      #   )
    )
  ),
  controlbar = dashboardControlbar(), # Optional
)
