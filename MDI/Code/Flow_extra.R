# Cleaning UNHCR Flow Data for Use with Shiny App
# Author: Connor Kelly
# Date: Dec 4, 2020
# Packages
library(readxl)
library(tidyverse)
library(maptools)
library(sf)
library(cartogram)
library(plotly)
library(countrycode)
library(tmap)
library(maps)
library(viridis)
library(rgdal)

############
# Setup and Initial Cleaning
############
setwd("C:/Users/Connor/Documents/GitHub/MDI")

priority_flow <- read_csv("Data/priority_flow.csv")
priority_flow <- priority_flow[, -c(1)]


priority_flow_long <- read_csv("Data/priority_flow_long.csv")
priority_flow_long <- priority_flow_long[, -c(1)]


# Add ISO3 codes
priority_flow_long$originiso <- countrycode(priority_flow_long$Origin, "country.name", "iso3c")
priority_flow_long$destiso <- countrycode(priority_flow_long$`Country of asylum`, "country.name", "iso3c")

priority_flow_long <- priority_flow_long[, c(1,5,2,6,3:4)]

write.csv(priority_flow_long, file="Data/priority_flow_long.csv")

afg <- priority_flow_long %>%
  filter(originiso == "AFG") %>%
  group_by(destiso) %>%
  summarize(refugees = sum(refugees))











total$originiso <- countrycode(total$Origin, "country.name", "iso3c")
total$originiso <- as.factor(total$originiso)

total$destiso <- countrycode(total$`Country of asylum`, "country.name", "iso3c")
total$destiso <- as.factor(total$destiso)
# Missing ISO for Kosovo (SC/RES/1244(1999)) and "Various"
# How to proceed with these instances?




# Parameter
startyear <- 2000

afg_cont <- cartogram_cont()











# What I want:
# Data on origin country

# Data on host country
# Proximity


# Country Borders Data from https://github.com/geodatasource/country-borders
borders <- read_csv("country-borders/GEODATASOURCE-COUNTRY-BORDERS.CSV")
borders <- borders[, c(2,4)]
borders$neighbor <- 1
borders <- borders %>%
  rename(Origin = country_name,
         `Country of asylum` = country_border_name)

# Different names
# Iran





total <- merge(priority_flow, borders, by=c("Origin", "Country of asylum"), all=TRUE)
total <- total[, c(1,2,61,3:60)]
total$neighbor[is.na(total$neighbor)] = 0



# Percentage of displaced going to neighboring country


