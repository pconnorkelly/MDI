# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

# Set directory and load data
setwd("C:/Users/Connor/Documents/GitHub/MDI")
priority_flow <- read_csv("Data/priority_flow.csv")

years <- colnames(priority_flow)[4:61]
  
# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Flows from Country of Origin"),
                sidebarLayout(
                  sidebarPanel(
                    # Select country to plot
                    selectInput(inputId = "country", label = strong("Country of origin"),
                                choices = unique(priority_flow$Origin),
                                selected = "Afghanistan"),
                    
                    # Select number of top destinations to display
                    numericInput(inputId = "n", label = strong("Number of destinations"), value = 5, min = 1, max = 20, step = 1),
                    
                    # Select date range to be plotted
                    selectInput(inputId = "date", strong("Start year"), choices = years, selected = "1962")
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "300px"),
    
                  )
                )
)

# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_country <- reactive({
    priority_flow %>%
      filter(Origin == input$country) %>%
      pivot_longer(col = c(input$date:60),
                   names_to = "year",
                   values_to = "refugees") %>%
      group_by(`Country of asylum`) %>%
      summarize(total = sum(refugees)) %>%
      arrange(desc(total)) %>%
      head(`Country of asylum`, n=input$n)
    })
  
  
  # Create plot
  output$lineplot <- renderPlot({
  ggplot(selected_country(), aes(x=year, y=refugees, color=`Country of asylum`)) + geom_line()  
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)