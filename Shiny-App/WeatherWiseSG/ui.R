
# ---------------------------------------------------------
# Load Packages
# ---------------------------------------------------------
pacman::p_load(
  shiny, bs4Dash, plotly, shinyWidgets)

# ---------------------------------------------------------
# Dashboard Page Layout
# ---------------------------------------------------------
dashboardPage(
  title = "WeatherWise Singapore",

  # Header (Top Bar with Title)
  header = dashboardHeader(
    title = dashboardBrand(
      title = "WeatherWise SG",
      color = "primary",
      href = "#"
    )
  ),

  # Sidebar Menu (Navigation)
  sidebar = dashboardSidebar(
    skin = "light",
    status = "primary",
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
        bs4SidebarMenuSubItem("Spatial Interpolation", tabName = "geo_interpolation"),
        bs4SidebarMenuSubItem("Geographically Weighted Predictive Modelling", tabName = "geo_modelling")
      )
    )
  ),

  # Main Body (Tab Content Area)
  body = dashboardBody(

    # CSS overlay style
    tags$head(
      tags$style(HTML("
      /* Font size tweaks (optional) */
      .nav-sidebar .nav-link,
      .nav-sidebar .nav-link span {
        font-size: 14px !important;
      }

      .nav-sidebar .nav-treeview .nav-link {
        font-size: 13px !important;
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
        h2("A Visual Exploration Tool for Singapore's Weather"),
        p("Understanding Singapore's changing weather..."), # You can use HTML for formatting
        h4("Overview of modules in app")
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
              p("This section gives a high-level overview of Singapore's weather across years.")
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
                  plotlyOutput("overview_yearly_plot", height = "220px")
                ),
                column(
                  width = 6,
                  plotlyOutput("overview_5_stations_plot", height = "220px")
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
              p("This view shows seasonal trends and cycles in weather patterns.")
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
              p("This page allows you to check for normality and run statistical tests across selected weather stations and time periods.")
            ),

            # Filter Box
            box(
              title = "Filters",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              selectInput("cda_station_param", "Select Parameter", choices = c("Loading..." = "loading")),
              selectInput("cda_station_time_type", "Select Time Interval",
                choices = c("Overall", "Year", "Month"), selected = "Overall"
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
              br(),
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
              p("This page allows you to check for normality and run statistical tests across selected weather stations and time periods.")
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
              br(),
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
      bs4TabItem(tabName = "forecasting"),
      bs4TabItem(tabName = "geo_extreme"),
      bs4TabItem(tabName = "geo_interpolation"),
      bs4TabItem(tabName = "geo_modelling")
    )
  ),
  controlbar = dashboardControlbar(), # Optional
)
