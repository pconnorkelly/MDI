# Animations
# Connor Kelly
# December 15, 2020

# Load packages
library(tidyverse)
library(readr)
library(ggplot2)
library(plotly)
library(gganimate)

# Set directory and load data
setwd("C:/Users/Connor/Documents/GitHub/MDI")
priority_flow_long <- read_csv("Data/priority_flow_long.csv")

# Plot
afg <- priority_flow_long %>%
  filter(Origin == "Afghanistan") %>%
  group_by(year) %>%
  mutate(yeartotal = sum(refugees)) %>%
  mutate(percent = refugees/yar)

summary(afg$yeartotal)

library(plotly)
library(gapminder)


plot_ly(afg,
        type = 'choropleth',
        locations = afg$destiso,
        z = afg$refugees,
        frame = afg$year,
        zmin = 0,
        zmax = 	1400000,
        colorscale="Blues_r")





df <- gapminder 
fig <- df %>%
  plot_ly(
    x = ~gdpPercap, 
    y = ~lifeExp, 
    size = ~pop, 
    color = ~continent, 
    frame = ~year, 
    text = ~country, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  )
fig <- fig %>% layout(
  xaxis = list(
    type = "log"
  )
)

fig