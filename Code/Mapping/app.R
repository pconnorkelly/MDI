# Shiny App: Mapping Forced Migration
# Connor Kelly
# December 7, 2020

# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(rsconnect)

# Set directory and load data
setwd("C:/Users/Connor/Documents/GitHub/MDI")
priority_flow_long <- read_csv("Data/priority_flow_long.csv")


# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Flows from Country of Origin"),
                
                # Output
                plotlyOutput(outputId = "map"),
                
                # Inputs
                fluidRow(
                  # Select country to plot
                  selectInput(inputId = "country", label = strong("Country of origin"),
                              choices = unique(priority_flow_long$Origin),
                              selected = "Afghanistan"),
                  
                  # Select year of observation
                  numericInput(inputId = "date", strong("Year"), value=1962, min = 1962, max = 2019, step=1)
                ),
)


# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_country <- reactive({
    priority_flow_long %>%
      filter(Origin == input$country, year == input$date) %>%
      group_by(`Country of asylum`) %>%
      mutate(total = sum(refugees)) %>%
      arrange(desc(total))
  })
  
  # Create plot
  output$map <- renderPlotly({
    print(
      plot_ly(selected_country(), type='choropleth', locations=selected_country()$destiso, 
              z=selected_country()$refugees, colorscale="Blues_r")
    )
    
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

# To download and run app from GitHub through R:
# runGitHub("MDI", "pconnorkelly")