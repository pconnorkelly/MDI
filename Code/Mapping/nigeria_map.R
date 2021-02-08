# Nigeria map
# Author: Connor Kelly
# Date: Jan 20, 2021

# Packages
library(sf)
library(readxl)
library(tidyverse)

# Set directory
setwd("C:/Users/Connor/Documents/GitHub/MDI")

# Load Nigeria
nigeria <- read_csv("Data/IOM DTM/nigeria.csv")
#burundi <- burundi[-c(1,2,5,8:19)]
shape <- st_read ("Data/nga_adm_osgof_20190417/nga_admbnda_adm2_osgof_20190417.shp")
shape$ADM2_EN <- toupper(shape$ADM2_EN)

nigeria$lga <- as.factor(nigeria$lga)

levels(nigeria$lga)

ggplot() +
  geom_sf(data=shape) + coord_sf()

test <- burundi %>%
  pivot_wider(names_from = date, values_from = individual) %>%
  replace(is.na(.),0) %>%
  group_by(admin2) %>%
  summarize_each(funs(sum))

test2 <- merge(shape, test, by.x=c("admin2Name"), by.y=c("admin2"), all.x=TRUE)
test2

dates <- c("")
# Plot
ggplot(data=test2) +
  geom_sf(aes(fill = `Jun 2019`))



