# Mapping Displacement with UNHCR Data
# Author: Connor Kelly
# Date: Dec 4, 2020
# Packages
library(readxl)
library(tidyverse)
library(plotly)
library(rworldmap)
library(RColorBrewer)

fig <- plot_ly(afg, type='choropleth', locations=afg$destiso, z=afg$refugees, colorscale = "Blues_r")

fig




pal <- colorRampPalette(brewer.pal(9, 'Reds'))(length(afg$refugees))



mapDevice('x11')
spdf <- joinCountryData2Map(afg, joinCode = "ISO3", nameJoinColumn = "destiso")

mapCountryData(spdf, nameColumnToPlot = "refugees", catMethod = "pretty")






nepal.adm3.shp <- readOGR(dsn="./NepalMaps/baselayers/NPL_adm", layer="NPL_adm3", stringsAsFactors = FALSE)
nepal.adm3.shp.df <- fortify(nepal.adm3.shp, region = "NAME_3")


world.df <- fortify(wrld_simpl, region = "ISO3")

plot(nepal.adm3.shp)
