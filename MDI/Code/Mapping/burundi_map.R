# Burundi map
# Author: Connor Kelly
# Date: Jan 20, 2021

# Packages
library(sf)
library(readxl)
library(tidyverse)

# Set directory
setwd("C:/Users/Connor/Documents/GitHub/MDI")

# Load Burundi
burundi <- read_csv("Data/IOM DTM/burundi.csv")
  burundi <- burundi[-c(1,2,5,8:19)]
shape <- st_read("Data/IOM DTM/bdi_adm_igebu_ocha_20171103_shp/bdi_admbnda_adm2_igebu_ocha_20171103.shp")

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



