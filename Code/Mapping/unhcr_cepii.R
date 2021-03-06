# Comparing Origins and Destinations (UNHCR)
# Author: Connor Kelly
# Date: Dec 15, 2020

# Packages
library(readxl)
library(tidyverse)
library(foreign)
library(haven)

# Set directory
setwd("C:/Users/Connor/Documents/GitHub/MDI")

# Load data
# UNHCR
priority_flow_long <- read_csv("Data/priority_flow_long.csv")
priority_flow_long <- priority_flow_long[,-c(1)] # Drop useless column
# CEPII distance data http://www.cepii.fr/pdf_pub/wp/2011/wp2011-25.pdf 
dist_cepii <- read_dta("Data/dist_cepii.dta")
# World Bank GDP data https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
gdp <- read_excel("Data/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_1740133.xls", skip = 2)
gdp <- pivot_longer(gdp, cols = c(5:65), names_to = "year", values_to = "gdp")
gdp <- gdp[, -c(1, 3, 4)]

# World Bank GDP per Capita https://data.worldbank.org/indicator/NY.GDP.PCAP.CD 
gdp_pc <- read_excel("Data/API_NY.GDP.PCAP.CD_DS2_en_excel_v2_1836202.xls", skip = 2)
gdp_pc <- pivot_longer(gdp_pc, cols = c(5:65), names_to = "year", values_to = "gdp_pc")
gdp_pc <- gdp_pc[, -c(1,3,4)]

gdp <- merge(gdp, gdp_pc, by=c("Country Code", "year"))

unhcr_cepii <- merge(priority_flow_long, dist_cepii, by.x=c("originiso", "destiso"), by.y=c("iso_o", "iso_d"), all.x=TRUE)

# Merge destination gdp for given year
unhcr_cepii <- merge(unhcr_cepii, gdp, by.x=c("destiso", "year"), by.y=c("Country Code", "year"))

summary(unhcr_cepii$gdp)

# See what got dropped
new_df <- unhcr_cepii[rowSums(is.na(unhcr_cepii))>0,]
unique(new_df$destiso)

# Regression
fit <- lm(refugees ~ contig + comlang_ethno + colony + col45 + dist + distw + gdp + gdp_pc, data=unhcr_cepii)
summary(fit)


