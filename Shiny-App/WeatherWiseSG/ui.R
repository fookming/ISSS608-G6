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
library(bs4Dash)

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
                   # Filters columns
                   column(
                     width = 2,
                     
                     #  Info Box (collapsible)
                     box(
                       title = tags$span(icon("info-circle"), "Overall"),
                       width = 12,
                       collapsible = TRUE,
                       collapsed = TRUE,
                       status = "info",
                       solidHeader = TRUE,
                       p("This view shows overall weather metrics.")
                     ),
                     
                     #  Parameter Control Panel
                     box(
                       title = "Filters",
                       width = 12,
                       solidHeader = TRUE,
                       status = "primary",
                       
                       h5("Select Parameter:"),
                       selectInput(
                         inputId = "overview_parameter",
                         label = NULL,
                         choices = c("Daily Rainfall" = "Daily Rainfall Total (mm)",
                                     "Mean Temperature" = "Mean Temperature (Celsius)",
                                     "Maximum Temperature" = "Maximum Temperature (Celsius)",
                                     "Minimum Temperature" = "Minimum Temperature (Celsius)",
                                     "Mean Wind Speed" = "Mean Wind Speed (km/h)"),
                         selected = "Mean Temperature (Celsius)"
                       )
                     )
                   ),
                   
                   # Main Chart column: charts, value boxes, etc.
                   column(
                     width = 10,
                     fluidRow(
                       box(title = "Overview charts", width = 12, solidHeader = TRUE,
                           status = "primary", collapsible = FALSE,
                           plotOutput("overview_charts")
                       )
                     )
                     # Add more charts or value boxes here later
                   )
                 )
        ),
      
      bs4TabItem(tabName = "eda_seasonality"
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
                                  
                                  h5("Select Parameter:"),
                                  selectInput("eda_station_param_1", NULL, choices = c("Loading..." = "loading")),  # set dynamically in server
                                  
                                  h5("Select Time Interval:"),
                                  selectInput("eda_station_time_1", NULL, choices = c("Loading..." = "loading"))  # set dynamically in server
                                  )
                              ),
                              column(
                                width = 10,
                                fluidRow(
                                  box(title = "Chart", width = 12, solidHeader = TRUE,
                                      status = "primary", collapsible = FALSE,
                                      plotOutput("station_dist_plot")
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
                                  
                                  h5("Select Parameter:"),
                                  selectInput("eda_station_param_2", NULL,
                                              choices = c("Daily Rainfall" = "Daily Rainfall Total (mm)",
                                                          "Mean Temperature" = "Mean Temperature (Celsius)",
                                                          "Maximum Temperature" = "Maximum Temperature (Celsius)",
                                                          "Minimum Temperature" = "Minimum Temperature (Celsius)",
                                                          "Mean Wind Speed" = "Mean Wind Speed (km/h)"),
                                              selected = "Mean Temperature (Celsius)"
                                  ),
                                  h5("Select Time Interval:"),
                                  radioButtons("eda_station_time_2", NULL,
                                               choices = c("Overall", "By Year"),
                                               selected = "Overall")
                                )
                              ),
                              column(
                                width = 10,
                                fluidRow(
                                  box(title = "Chart", width = 12, solidHeader = TRUE,
                                      status = "primary", collapsible = FALSE,
                                      plotOutput("month_station_heatmap")
                                  )
                                )
                              )
                            )
                   )
              )
        ),
      
      bs4TabItem(tabName = "cda_station"
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

