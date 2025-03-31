#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# library(bs4Dash)

pacman::p_load(bs4Dash, shiny, shinyWidgets, tidyverse, tmap, sf, sfdep, corrplot, terra,
               gstat, automap,  SpatialML, GWmodel, Metrics, ggrepel, leaflet)

dashboardPage(
  title = "WeatherWise Singapore",
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = "WeatherWise SG",
      color = "primary",
      href = "#"
    )
  ),
  
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
                   # Left panel - dropdown filter
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
                   
                   # Main panel: charts, value boxes, etc.
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
      
      bs4TabItem(tabName = "geo_extreme",
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
                         choices = c("Daily Rainfall" = "Daily Rainfall Total (mm)",
                                     "Mean Temperature" = "Mean Temperature (Celsius)",
                                     "Maximum Temperature" = "Maximum Temperature (Celsius)",
                                     "Minimum Temperature" = "Minimum Temperature (Celsius)",
                                     "Mean Wind Speed" = "Mean Wind Speed (km/h)"),
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
                                            minView = 'months',
                                            minDate = "2018-01-01",maxDate = "2024-12-31",
                                            dateFormat = "yyyy-MM",
                                            autoClose = TRUE,
                                            value = "2024-12")
                       ),
                       # Show Year Picker when "Year" is selected
                       conditionalPanel(
                         condition = "input.geo_extreme_time_interval == 'Year'",
                         airDatepickerInput("geo_extreme_year_input", "Select Year", 
                                            view = "years",
                                            minView = 'years',
                                            minDate = "2018-01-01",maxDate = "2024-12-31",
                                            dateFormat = "yyyy",
                                            autoClose = TRUE,
                                            value = "2024")
                       )
                     )
                   ),
                   
                   # Main panel: charts, value boxes, etc.
                   column(
                     width = 10,
                     fluidRow(
                       box(title = "Interactive Map", width = 12, solidHeader = TRUE,
                           status = "primary", collapsible = FALSE,
                           leafletOutput("geo_extreme_map")
                       )
                     )
                     # Add more charts or value boxes here later
                   )
                 )
        ),
      
      bs4TabItem(tabName = "geo_interpolation",
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
                         choices = c("Daily Rainfall" = "Daily Rainfall Total (mm)",
                                     "Mean Temperature" = "Mean Temperature (Celsius)",
                                     "Maximum Temperature" = "Maximum Temperature (Celsius)",
                                     "Minimum Temperature" = "Minimum Temperature (Celsius)",
                                     "Mean Wind Speed" = "Mean Wind Speed (km/h)"),
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
                                            minView = 'months',
                                            minDate = "2018-01-01",maxDate = "2024-12-31",
                                            dateFormat = "yyyy-MM",
                                            autoClose = TRUE,
                                            value = "2024-12")
                       ),
                       # Show Year Picker when "Year" is selected
                       conditionalPanel(
                         condition = "input.geo_inter_time_interval == 'Year'",
                         airDatepickerInput("geo_inter_year_input", "Select Year", 
                                            view = "years",
                                            minView = 'years',
                                            minDate = "2018-01-01",maxDate = "2024-12-31",
                                            dateFormat = "yyyy",
                                            autoClose = TRUE,
                                            value = "2024")
                       )
                     )
                   ),
                   
                   # Main panel: charts, value boxes, etc.
                   column(
                     width = 10,
                     tabsetPanel(
                       id = 'geo_inter_tabs',
                       type = 'tabs',
                       tabPanel(
                         "Inverse Distance Weighted Interpolation",br(),
                         fluidRow(
                           column(
                             width = 2,
                             box(
                               title = "Interpolation Parameters",
                               width = 12, 
                               solidHeader = TRUE, 
                               status = "primary",
                               collapsible = FALSE,
                               sliderInput(inputId = "geo_inter_nmax",
                                           label = "Number of neighbors", 
                                           min = 1, 
                                           max = 10, 
                                           value = 5, 
                                           step = 1),
                               sliderInput(inputId = "geo_inter_idp",
                                           label = "Inverse distance power",
                                           min = 0, 
                                           max = 2.5, 
                                           value = 1, 
                                           step = 0.1),
                               actionButton(inputId = "geo_idw_update_map", 
                                            label = "Update Map",
                                            class = "btn-primary")
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
                                 choices = c("Exp", "Sph", "Gau", "Mat","Nug","Exc",
                                             "Ste","Cir","Lin","Bes","Pen","Per",
                                             "Wav","Hol","Log","Pow","Spl"),
                                 selected = "Sph"),
                               sliderInput(inputId = "geo_inter_psill",
                                           label = "psill", 
                                           min = 0.5, 
                                           max = 10, 
                                           value = 0.5, 
                                           step = 0.5),
                               sliderInput(inputId = "geo_inter_range",
                                           label = "range",
                                           min = 1000, 
                                           max = 10000, 
                                           value = 5000, 
                                           step = 1000),
                               sliderInput(inputId = "geo_inter_nugget",
                                           label = "nugget",
                                           min = 0.1, 
                                           max = 10, 
                                           value = 0.1, 
                                           step = 0.1),
                               actionButton(inputId = "geo_kriging_update_map", 
                                            label = "Update Map",
                                            class = "btn-primary")
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
                               status = 'primary',
                               collapsible = FALSE,
                               tmapOutput("geo_kriging_auto_map")
                             )
                           )
                         )
                       )
                     )
                       )
                     )
                     # Add more charts or value boxes here later
                   
                 
        ),
      
      bs4TabItem(tabName = "geo_modelling",
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
                       p("This view shows performance comparison between Non-spatial and Geographically Weighted Random Forest.")
                     ),
                     
                     #  Parameter Control Panel
                     box(
                       title = "Filters",
                       width = 12,
                       solidHeader = TRUE,
                       status = "primary",
                       collapsible = FALSE,
                       selectInput(
                         inputId = "geo_fc_weather_var",
                         label = "Select Parameter",
                         choices = c("Daily Rainfall" = "Daily Rainfall Total (mm)",
                                     "Mean Temperature" = "Mean Temperature (Celsius)",
                                     "Maximum Temperature" = "Maximum Temperature (Celsius)",
                                     "Minimum Temperature" = "Minimum Temperature (Celsius)",
                                     "Mean Wind Speed" = "Mean Wind Speed (km/h)"),
                         selected = "Mean Temperature (Celsius)"
                       ),
                       
                       selectInput(
                         inputId = "geo_fc_kernel",
                         label = "Select Kernel",
                         choices = c("adaptive", "fixed"),
                         selected = "adaptive"
                       ),
                       # Show Adaptive bw range when adaptive kernel is selected
                       conditionalPanel(
                         condition = "input.geo_fc_kernel == 'adaptive'",
                         sliderInput(inputId = "geo_fc_adapt_bw",
                                     label = "bandwidth (# neighbors)",
                                     min = 300, 
                                     max = 2000, 
                                     value = 350, 
                                     step = 50)
                       ),
                       # Show Fixed bw range when fixed kernel is selected
                       conditionalPanel(
                         condition = "input.geo_fc_kernel == 'fixed'",
                         sliderInput(inputId = "geo_fc_fixed_bw",
                                     label = "bandwidth (meters)",
                                     min = 1000, 
                                     max = 20000, 
                                     value = 1000, 
                                     step = 1000)
                       ),
                       actionButton(inputId = "geo_fc_update_chart", 
                                    label = "Update Charts",
                                    class = "btn-primary")
                     )
                   ),
                   
                   # Main panel: charts, value boxes, etc.
                   column(
                     width = 10,
                     fluidRow(
                       box(title = "Scatter Plots of Actual vs Predicted Values",
                           width = 12, 
                           solidHeader = TRUE,
                           status = "primary",
                           collapsible = FALSE,
                           plotOutput("geo_fc_chart")
                       )
                     )
                     # Add more charts or value boxes here later
                   )
                 )         
        )
    )
  ),
  
  controlbar = dashboardControlbar(),  # Optional
  footer = dashboardFooter()           # Optional
)

