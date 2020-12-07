# Cleaning UNHCR Flow Data for Use with Shiny App
# Author: Connor Kelly
# Date: Dec 4, 2020

# Packages
library(readxl)
library(tidyverse)

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

# Save cleaned data
write.csv(priority_flow, file="Data/priority_flow.csv")


afg <- priority_flow %>%
    filter(Origin == "Afghanistan") %>%
    pivot_longer(col = c("1980":60),
                 names_to = "year",
                 values_to = "refugees") %>%
    group_by(`Country of asylum`) %>%
    summarize(total = sum(refugees)) %>%
    arrange(desc(total)) %>%
    head(`Country of asylum`, n=5)


