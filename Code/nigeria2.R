# Nigeria redux
# Author: Connor Kelly
# Date: Feb 17, 2021
# Going back to re-do IOM data aggregation to ensure origin and destination data

# Packages
library(readxl)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(rgdal)
library(spdep)
library(plotly)
library(gghighlight)

# Set directory
setwd("C:/Users/Connor/Documents/GitHub/MDI")

# Variables of interest
vars <- c("lga_name", "estimate_hh_Ward", "estimate_Ind_Ward", "lga_orig")

# Load shape data
shape <- st_read ("Data/nga_adm_osgof_20190417/nga_admbnda_adm2_osgof_20190417.shp")
shape$ADM2_EN <- toupper(shape$ADM2_EN)
names(shape)
summary(shape)

row.names(shape) <- as.character(shape$ADM2_EN)

nb <- poly2nb(shape)

ggplotly(
  ggplot() +
    geom_sf(data=shape, aes(fill=ADM2_EN)) +
    gghighlight(ADM2_EN == "BASSA" | 
                  ADM2_EN == "IFELODUN" | 
                  ADM2_EN == "IREPODUN" | 
                  ADM2_EN == "NASARAWA" | 
                  ADM2_EN == "OBI" | 
                  ADM2_EN == "SURULERE")
)

# non-unique values when setting 'row.names': 'BASSA', 
#'IFELODUN', 'IREPODUN', 'NASARAWA', 'OBI', 'SURULERE'

#nigeria$lga <- nigeria$lga %>%
 #recode("ARDO - KOLA" = "ARDO-KOLA") %>%
  #recode("ASKIRA / UBA" = "ASKIRA/UBA") %>%
  #recode("KWAYA / KUSAR" = "KWAYA/KUSAR") %>%
  #recode("MAIDUGURI M. C." = "MAIDUGURI") %>%
  #recode("MAYO - BELWA" = "MAYO-BELWA") %>%
  #recode("YALMALTU/ DEBA" = "YALAMALTU/DEBA")

# Load data
round4 <- read_excel("Data/Nigeria/round4.xlsx") %>%
  rename(
    lga_name = `LGA Name`,
    estimate_hh_Ward = `Estimated number of households by Ward`,
    estimate_Ind_Ward = `Estimated number of individuals by Ward`,
    lga_orig = `LGA of origin of majority`,
    ) %>%
  select(vars)
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
  
round15 <- read_excel("Data/Nigeria/round15.xlsx") %>%
  rename(
    lga_name = `LGA`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
  ) %>%
  select(vars)
  round15$date <- "05-15-2017"

round16 <- read_excel("Data/Nigeria/round16.xlsx") %>%
  rename(
    lga_name = `LGA`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
  ) %>%
  select(vars)
  round16$date <- "05-17-2017"
  
round17 <- read_excel("Data/Nigeria/round17.xlsx") %>%
  rename(
    lga_name = `LGA`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
  ) %>%
  select(vars)
  round17$date <- "06-26-2017"
  
round18 <- read_excel("Data/Nigeria/round18.xlsx") %>%
  rename(
    lga_name = `LGA`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
  ) %>%
  select(vars)
  round18$date <- "11-23-2017"
  
round19 <- read_excel("Data/Nigeria/round19.xlsx") %>%
  rename(
    lga_name = `LGA`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
  ) %>%
  select(vars)
  round19$date <- "09-30-2017"
  
round20 <- read_excel("Data/Nigeria/round20.xlsx") %>%
  rename(
    lga_name = `LGA`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
  ) %>%
  select(vars)
  round20$date <- "12-08-2017"
  
round21 <- read_excel("Data/Nigeria/round21.xlsx") %>%
  rename(
    lga_name = `LGA`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
  ) %>%
  select(vars)
  round21$date <- "01-31-2018"
  
round22 <- read_excel("Data/Nigeria/round22.xlsx") %>%
  rename(
    lga_name = `LGA`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
  ) %>%
  select(vars)
  round22$date <- "04-30-2018"
  
round23  <- read_excel("Data/Nigeria/round23.xlsx") %>%
  rename(
    lga_name = `LGA`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
  ) %>%
  select(vars)
  round23$date <- "06-16-2018"

round24 <- read_excel("Data/Nigeria/round24.xlsx") %>%
  rename(
    lga_name = `LGA`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
  ) %>%
  select(vars)
  round24$date <- "08-06-2018"

round25 <- read_excel("Data/Nigeria/round25.xlsx") %>%
  rename(
    lga_name = `LGA`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
  ) %>%
  select(vars)
  round25$date <- "10-20-2018"
  
round26 <- read_excel("Data/Nigeria/round26.xlsx") %>%
  rename(
    lga_name = `LGA`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
  ) %>%
  select(vars)

round27 <- read_excel("Data/Nigeria/round27.xlsx") %>%
  rename(
    lga_name = `LGA`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
  ) %>%
  select(vars)
  round27$date <- "05-29-2019"
  
round28 <- read_excel("Data/Nigeria/round28.xlsx") %>%
  rename(
    lga_name = `LGA`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
  ) %>%
  select(vars)
  round28$date <- "09-16-2019"

# Merge data
nigeria <- rbind(round4, round5, round6, round7, round8, round9, round10, round11, round12, 
                 round13, round14, round15, round16, round17, round18, round19, round20, 
                 round21, round22, round23, round24, round25, round26, round27, round28,
                 by=c("lga_name", "lga_orig"))
  nigeria$estimate_hh_Ward <- as.numeric(nigeria$estimate_hh_Ward)
  nigeria$estimate_Ind_Ward <- as.numeric(nigeria$estimate_Ind_Ward)
  nigeria$lga_name <- as.factor(nigeria$lga_name)
  nigeria$lga_orig <- as.factor(nigeria$lga_orig)
  
nigeria <- nigeria %>%
  group_by(lga_name, lga_orig, date) %>%
  summarize(estimate_hh = sum(estimate_hh_Ward), estimate_ind = sum(estimate_Ind_Ward))
