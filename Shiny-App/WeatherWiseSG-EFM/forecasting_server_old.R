
# forecasting_server.R

library(shiny)
library(tidyverse)  # data manipulation, reading CSV, and plotting
library("lubridate")

forecasting_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Load your data
    climate_data <- read_csv("data/climate_final_2018_2024.csv")
    
    # Extract choices for UI dropdowns
    variable_choices <- names(climate_data)[6:10]
    station_choices <- unique(climate_data$Station)
    
    # Dynamically populate UI dropdowns
    updateSelectInput(session, "variable", choices = variable_choices)
    updateSelectInput(session, "station", choices = station_choices)
    
    
    # Reactive filtering
    filtered_data <- reactive({
      req(input$variable, input$station)
      climate_data %>%
        filter(Station == input$station) %>%
    #    mutate(Date = make_date(as.integer(Year), as.integer(Month), as.integer(Day))) %>% 
        mutate(Date = ymd(paste(Year, Month, Day, sep = "-"))) %>%
        select(Date, "Daily Rainfall Total (mm)", Station,
               Value = all_of(input$variable))
    })
    
    output$time_series_plot <- renderPlot({
      req(filtered_data())
      filtered_data() %>%
        ggplot(aes(x = Date, y = Value)) +
        geom_line(color = "steelblue") +
        labs(
          title = paste("Time Series Plot of", input$variable, "at", input$station),
          x = "Date",
          y = input$variable
        ) +
        theme_minimal()
    })
    
    
  })

}

