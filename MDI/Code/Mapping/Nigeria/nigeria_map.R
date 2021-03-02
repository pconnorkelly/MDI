# Nigeria map
# Author: Connor Kelly
# Date: Feb 11, 2021

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

nigeria$lga <- nigeria$lga %>%
  recode("ARDO - KOLA" = "ARDO-KOLA") %>%
  recode("ASKIRA / UBA" = "ASKIRA/UBA") %>%
  recode("KWAYA / KUSAR" = "KWAYA/KUSAR") %>%
  recode("MAIDUGURI M. C." = "MAIDUGURI") %>%
  recode("MAYO - BELWA" = "MAYO-BELWA") %>%
  recode("YALMALTU/ DEBA" = "YALAMALTU/DEBA")

levels(nigeria$lga)



ggplot() +
  geom_sf(data=shape) + coord_sf()

test <- nigeria %>%
  pivot_wider(names_from = month, values_from = individual) %>%
  replace(is.na(.),0) %>%
  group_by(lga) %>%
  summarize_each(funs(sum))

test2 <- merge(shape, test, by.x=c("ADM2_EN"), by.y=c("lga"), all.x=TRUE)
test2



# Plot
ggplot(data=test2) +
  geom_sf(aes(fill = `2019-08`))



