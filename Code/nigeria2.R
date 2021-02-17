# Nigeria redux
# Author: Connor Kelly
# Date: Feb 17, 2021
# Going back to re-do IOM data aggregation to ensure origin and destination data

# Packages
library(readxl)
library(tidyverse)
library(lubridate)
library(dplyr)
library(openxlsx)
library(rgeos)
library(rgdal)

# Set directory
setwd("C:/Users/Connor/Documents/GitHub/MDI")

# Variables of interest
vars <- c("lga_name", "estimate_hh_Ward", "estimate_Ind_Ward", "lga_orig")

# Load shape data
shape <- st_read ("Data/nga_adm_osgof_20190417/nga_admbnda_adm2_osgof_20190417.shp")
shape$ADM2_EN <- toupper(shape$ADM2_EN)


#nigeria$lga <- nigeria$lga %>%
 #recode("ARDO - KOLA" = "ARDO-KOLA") %>%
  #recode("ASKIRA / UBA" = "ASKIRA/UBA") %>%
  #recode("KWAYA / KUSAR" = "KWAYA/KUSAR") %>%
  #recode("MAIDUGURI M. C." = "MAIDUGURI") %>%
  #recode("MAYO - BELWA" = "MAYO-BELWA") %>%
  #recode("YALMALTU/ DEBA" = "YALAMALTU/DEBA")

# Load data
round4 <- read_excel("Data/Nigeria/round4.xlsx") %>%
  select(`LGA Name`, `Estimated number of households by Ward`, `Estimated number of individuals by Ward`,
         `LGA of origin of majority`) %>%
  rename(
    lga_name = `LGA Name`,
    estimate_hh_Ward = `Estimated number of households by Ward`,
    estimate_Ind_Ward = `Estimated number of individuals by Ward`,
    lga_orig = `LGA of origin of majority`,
    )
  round4$date <- "06-30-2015"

round5 <- read_excel("Data/Nigeria/round5.xlsx") %>%
  select(vars)
  round5$date <- "08-31-2015"

round6 <- read_excel("Data/Nigeria/round6.xlsx") %>%
  select(vars)
  round6$date <- "10-31-2015"

round7 <- read_excel("Data/Nigeria/round7.xlsx") %>%
  select(vars)
  round7$date <- "12-23-2015"
  
round8 <- read_excel("Data/Nigeria/round8.xlsx") %>%
  select(vars)
  round8$date <- "02-29-2016"  

round9 <- read_excel("Data/Nigeria/round9.xlsx") %>%
  select(vars)
  round9$date <- "04-29-2016"

round10 <- read_excel("Data/Nigeria/round10.xlsx") %>%
  select(vars)
  round10$date <- "06-30-2016"  
  
round11 <- read_excel("Data/Nigeria/round11.xlsx") %>%
  select(vars)
  round11$date <- "08-31-2016"
  
round12 <- read_excel("Data/Nigeria/round12.xlsx") %>%
  select(vars)
  round12$date <- "10-26-2016"
  
round13 <- read_excel("Data/Nigeria/round13.xlsx") %>%
  select(vars)
  round13$date <- "11-24-2016"
  
round14 <- read_excel("Data/Nigeria/round14.xlsx") %>%
  select(vars)
  round14$date <- "01-25-2017"

# Merge data
nigeria <- rbind(round4, round5, round6, round7, round8, round9, round10, round11, round12, round13, round14,
                 by=c("lga_name", "lga_orig"))
  nigeria$estimate_hh_Ward <- as.numeric(nigeria$estimate_hh_Ward)
  nigeria$estimate_Ind_Ward <- as.numeric(nigeria$estimate_Ind_Ward)
  nigeria$lga_name <- as.factor(nigeria$lga_name)
  nigeria$lga_orig <- as.factor(nigeria$lga_orig)
  
nigeria <- nigeria %>%
  group_by(lga_name, lga_orig, date) %>%
  summarize(estimate_hh = sum(estimate_hh_Ward), estimate_ind = sum(estimate_Ind_Ward))
