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
# setwd("C:/Users/Connor/Documents/GitHub/MDI")
priority_flow_long <- read_csv("Data/priority_flow_long.csv")

  
# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Flows from Country of Origin"),
                
                # Output
                plotlyOutput(outputId = "lineplot"),
                
                # Inputs
                fluidRow(
                  # Select country to plot
                  selectInput(inputId = "country", label = strong("Country of origin"),
                              choices = unique(priority_flow_long$Origin),
                              selected = "Afghanistan"),
                  
                  # Select number of top destinations to display
                  numericInput(inputId = "n", label = strong("Number of destinations"), value = 5, min = 1, max = 20, step = 1),
                  
                  # Select date range to be plotted
                  numericInput(inputId = "date", strong("Start year"), value = 1962, min = 1962, max = 2019, step=1)
                  ),
                )


# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_country <- reactive({
    priority_flow_long %>%
    filter(Origin == input$country, year >= input$date) %>%
    group_by(`Country of asylum`) %>%
    mutate(total = sum(refugees)) %>%
    arrange(desc(total))
    })
  
  # Identify top destinations
  top <- reactive({
    priority_flow_long %>%
    filter(Origin == input$country, year >= input$date) %>%
    group_by(`Country of asylum`) %>%
    summarize(total = sum(refugees)) %>%
    arrange(desc(total)) %>%
    head(`Country of asylum`, n=input$n) %>%
    pull(`Country of asylum`)
  })

  
  # Create plot
  output$lineplot <- renderPlotly({
  print(
    ggplotly(
      ggplot(subset(selected_country(), `Country of asylum` %in% top()), aes(x=year, y=refugees, color=`Country of asylum`)) +
        geom_line() + xlab("Year") + ylab("Refugees and Asylum Seekers") + scale_y_continuous(labels = scales::comma) +
        ggtitle("Destinations of Asylum Seekers and Refugees") + scale_color_discrete(limits=top())
    )
  )
  
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

# To download and run app from GitHub through R:
# runGitHub("MDI", "pconnorkelly")