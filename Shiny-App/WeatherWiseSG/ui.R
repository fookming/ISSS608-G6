#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# ---------------------------------------------------------
# Load Packages
# ---------------------------------------------------------
pacman::p_load(shiny, bs4Dash, plotly, shinyWidgets)

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
      
      bs4SidebarMenuItem("Exploratory Data Analysis", icon = icon("chart-area"),
                         bs4SidebarMenuSubItem("Overview", tabName = "eda_overview"),
                         bs4SidebarMenuSubItem("Seasonality Analysis", tabName = "eda_seasonality"),
                         bs4SidebarMenuSubItem("Station Comparison", tabName = "eda_station")
                        ),
      
      bs4SidebarMenuItem("Confirmatory Data Analysis", icon = icon("balance-scale"),
                         bs4SidebarMenuSubItem("Compare across stations", tabName = "cda_station"),
                         bs4SidebarMenuSubItem("Compare across time", tabName = "cda_time")
                        ),
      
      bs4SidebarMenuItem("Time Series Forecasting", tabName = "forecasting", icon = icon("chart-line")),
      
      bs4SidebarMenuItem("Geo-Spatial Analysis", icon = icon("globe-asia"),
                         bs4SidebarMenuSubItem("Extreme Weather Events", tabName = "geo_extreme"),
                         bs4SidebarMenuSubItem("Spatial Interpolation", tabName = "geo_interpolation"),
                         bs4SidebarMenuSubItem("Geographically Weighted Predictive Modelling", tabName = "geo_modelling")
                        )
    )
  ),
  
  # Main Body (Tab Content Area)
  body = dashboardBody(
    
    # Tabs
    bs4TabItems(
      
      bs4TabItem(tabName = "landing",
                 h2("A Visual Exploration Tool for Singapore's Weather"),
                 p("Understanding Singapore's changing weather..."),  # You can use HTML for formatting
                 h4("Overview of modules in app")
        ),
     
      bs4TabItem(tabName = "eda_overview",
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
                         column(width = 6,
                                plotlyOutput("overview_yearcompare_plot", height = "280px")
                         ),
                         column(width = 6,
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
                         column(width = 6,
                                plotlyOutput("overview_yearly_plot", height = "220px")
                         ),
                         column(width = 6,
                                plotlyOutput("overview_5_stations_plot", height = "220px")
                         )
                       )
                     )
                   )
                 )
      ),
      
      
      bs4TabItem(tabName = "eda_seasonality",
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
                       div(style = "display: flex; justify-content: center; gap: 40px; padding: 1px 0;",
                           
                           # Parameter Dropdown + Label
                           div(style = "display: flex; align-items: center; gap: 5px;",
                               tags$label("Select Parameter:", style = "margin-bottom: 0;"),
                               selectInput("eda_seasonality_parameter", label = NULL,
                                           choices = c("Loading..." = "loading"), width = "200px")
                           ),
                           
                           # Station Dropdown + Label
                           div(style = "display: flex; align-items: center; gap: 5px;",
                               tags$label("Select Station:", style = "margin-center: 0;"),
                               selectInput("eda_seasonality_station", label = NULL,
                                           choices = c("Loading..." = "loading"), width = "200px")
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
    
      bs4TabItem(tabName = "eda_station",
                 tabsetPanel(
                   id = "eda_station_tabs",  # input$station_tabs will hold the selected tab
                   type = "tabs",        # (optional: use "pills" if you prefer that look)
                   
                   # ---- Tab 1 ----
                   tabPanel("Station-wise Distribution",
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
                                  
                                  selectInput("eda_station_param_1", "Select Parameter", choices = c("Loading..." = "loading")),  # set dynamically in server
                                  
                                  selectInput("eda_station_time_1", "Select Time Interval", choices = c("Loading..." = "loading"))  # set dynamically in server
                                  )
                              ),
                              column(
                                width = 10,
                                fluidRow(
                                  box(title = "Chart", width = 12, solidHeader = TRUE,
                                      status = "primary", collapsible = FALSE,
                                      plotlyOutput("station_dist_plot")
                                  )
                                )
                              )
                            )
                   ),
                   
                   # ---- Tab 2 ----
                   tabPanel("Month-by-Station Variation",
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
                                  
                                  selectInput("eda_station_param_2", "Select Parameter", choices = c("Loading..." = "loading")),  # set dynamically in server
                                  
                                  selectInput("eda_station_time_2", "Select Time Interval", choices = c("Loading..." = "loading"))  # set dynamically in server
                                )
                              ),
                              column(
                                width = 10,
                                fluidRow(
                                  box(title = "Chart", width = 12, solidHeader = TRUE,
                                      status = "primary", collapsible = FALSE,
                                      plotlyOutput("month_station_heatmap")
                                  )
                                )
                              )
                            )
                   )
              )
        ),
      
      bs4TabItem(tabName = "cda_station",
                 fluidRow(
                   
                   # -- Shared Filters (left panel)
                   column(width = 2,
                          box(
                            title = "Filters",
                            width = 12,
                            solidHeader = TRUE,
                            status = "primary",
                            
                            selectInput("cda_station_param", "Select Parameter", choices = c("Loading..." = "loading")),
                            
                            selectInput("cda_station_time_type", "Select Time Interval",
                                        choices = c("Overall", "Year", "Month"), selected = "Overall"),
                            
                            uiOutput("cda_station_time_picker"),
                            
                            selectizeInput("cda_station_list", "Select Station(s)",
                                           choices = NULL,
                                           multiple = TRUE,
                                           options = list(placeholder = 'Select...', plugins = list('remove_button')))
                            
                          )
                   ),
                   
                   # -- Tab Content (right panel)
                   column(width = 10,
                          tabsetPanel(
                            id = "cda_station_tabs",
                            type = "tabs",
                            
                            # --- TAB 1: Normality Test ---
                            tabPanel("Normality Test",
                                     br(),
                                     actionButton("cda_station_normality_btn", "Check Normality", class = "btn-success", width = "20%"),
                                     br(), br(),
                                     box(
                                       title = "Anderson-Darling Test Result",
                                       width = 12,
                                       solidHeader = TRUE,
                                       collapsed = TRUE,
                                       status = "primary",
                                       dataTableOutput("cda_ad_table")
                                     ),
                                     box(
                                       title = "QQ Plot",
                                       width = 12,
                                       solidHeader = TRUE,
                                       status = "primary",
                                       plotlyOutput("cda_station_normality_plot", height = "300px")
                                     )
                                     
                                     
                            ),
                            
                            # --- TAB 2: Statistical Test ---
                            tabPanel("Run Statistical Test",
                                     br(),
                                     fluidRow(
                                       column(width = 2,
                                              box(
                                                title = "Statistical Test Options",
                                                width = 12,
                                                solidHeader = TRUE,
                                                status = "primary",
                                                selectInput("cda_station_test_type", "Test Type",
                                                            choices = c("Parametric", "Non-Parametric", "Robust", "Bayes-Factor"),
                                                            selected = "Non-Parametric"),
                                                selectInput("cda_station_conf_level", "Confidence Level",
                                                            choices = c("90%", "95%", "99%"),
                                                            selected = "95%"),
                                                checkboxInput("cda_station_pairwise", "Show Pairwise Comparison", value = FALSE),
                                                conditionalPanel(
                                                  condition = "input.cda_station_pairwise == true",
                                                  selectInput("cda_station_pairwise_display", "Pairwise Display Type",
                                                              choices = c("Significant", "Non-Significant", "All"),
                                                              selected = "Significant")
                                                ),
                                                actionButton("cda_station_test_btn", "Run Test", class = "btn-success", width = "100%")
                                              )
                                       ),
                                       column(width = 10,
                                              box(
                                                title = "Statistical Test Results",
                                                width = 12,
                                                solidHeader = TRUE,
                                                status = "primary",
                                                plotOutput("cda_station_test_plot", height = "400px")
                                              )
                                       )
                                     )
                            )
                          )
                   )
                 )
      ),
      
      bs4TabItem(tabName = "cda_time"
        ),
      
      bs4TabItem(tabName = "forecasting"
        ),
      
      bs4TabItem(tabName = "geo_extreme"
        ),
      
      bs4TabItem(tabName = "geo_interpolation"
        ),
      
      bs4TabItem(tabName = "geo_modelling"
        )
    )
  ),
  
  controlbar = dashboardControlbar(),  # Optional
  footer = dashboardFooter()           # Optional
)

