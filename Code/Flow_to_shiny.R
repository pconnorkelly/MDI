# Cleaning UNHCR Flow Data for Use with Shiny App
# Author: Connor Kelly
# Date: Dec 4, 2020

# Packages
library(readxl)
library(tidyverse)
library(countrycode)

############
# Setup and Initial Cleaning
############
setwd("C:/Users/Connor/Documents/GitHub/MDI")
flow <- read_excel("Data/new_ref_arrival_new_asy_app_1962_2019.xlsx", skip = 1)

flow[is.na(flow)] <- 0 # Replace missing values as 0
flow <- flow[, c(2, 1, 3:61)] # Reorder columns

# Combine refugees and asylum seekers
flow <- flow %>%
  group_by(Origin, `Country of asylum`) %>%
  summarize_if(is.numeric, sum)

# Save priority countries
priority_countries <- c("Afghanistan", "Bangladesh", "Burundi", "Central African Republic", "Chad", "Guatemala", "Iraq", "Libya", "Mali",
                        "Myanmar", "Nigeria", "Somalia", "South Sudan", "Sudan", "Syrian Arab Rep.", "Venezuela (Bolivarian Republic of)",
                        "Yemen")

# Restrict dataset to origins within priority countries
priority_flow <- flow %>% filter(Origin %in% priority_countries)

priority_flow_long <- priority_flow %>%
  pivot_longer(col = c(3:60),
               names_to = "year",
               values_to = "refugees")

priority_flow_long$`Country of asylum` <- as.factor(priority_flow_long$`Country of asylum`)


# Add ISO3 codes
priority_flow_long$originiso <- countrycode(priority_flow_long$Origin, "country.name", "iso3c")
priority_flow_long$destiso <- countrycode(priority_flow_long$`Country of asylum`, "country.name", "iso3c")

priority_flow_long <- priority_flow_long[, c(1,5,2,6,3:4)]

# Save long data
write.csv(priority_flow_long, file="Data/priority_flow_long.csv")
