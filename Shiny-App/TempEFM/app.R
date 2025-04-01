library(shiny)
library(tidyverse)

ui <- fluidPage(
  verbatimTextOutput("test")
)

server <- function(input, output, session){
  climate_data <- read_csv("data/climate_final_2018_2024.csv")
  
  output$test <- renderPrint({
    head(climate_data, 5)
  })
  
  # Variables choices (columns 6 to 10)
  variable_choices <- names(climate_data)[6:10]
  
  # Station choices (unique values)
  station_choices <- unique(climate_data$Station)
  
  # Verify your results
  print(variable_choices)
  print(station_choices)
  
}

shinyApp(ui, server)
