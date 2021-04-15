# Nigeria redux
# Author: Connor Kelly
# Date: March 17, 2021
# Going back to re-do IOM data aggregation to ensure origin and destination data

# Packages
library(readxl)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(rgdal)
library(spdep)
library(plotly)
library(sf)
library(gghighlight)
library(jtools)
library(zoo)
library(reshape2)
library(expp)
library(raster)
library(distances)
library(pscl)
library(OasisR)
library(conflicted)
library(lwgeom)
library(potential)
library(SpatialPosition)
library(MASS)


conflict_prefer("select", "dplyr")

# Set directory
setwd("C:/Users/Connor/Documents/GitHub/MDI")

# Variables of interest
vars <- c("lga_name", "state_name", "estimate_hh_Ward", "estimate_Ind_Ward", 
          "lga_orig", "state_orig")

# Load data
######
round4 <- read_excel("Data/Nigeria/round4.xlsx") %>%
  rename(
    lga_name = `LGA Name`,
    state_name = `State Name`,
    estimate_hh_Ward = `Estimated number of households by Ward`,
    estimate_Ind_Ward = `Estimated number of individuals by Ward`,
    lga_orig = `LGA of origin of majority`,
    state_orig = `State of origin of majority`
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
    state_name = `State of Displacement`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
    state_orig = `State of Origin of Majority`
  ) %>%
  select(vars)
  round15$date <- "05-15-2017"

round16 <- read_excel("Data/Nigeria/round16.xlsx") %>%
  rename(
    lga_name = `LGA`,
    state_name = `State of Displacement`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
    state_orig = `State of Origin of Majority`
  ) %>%
  select(vars)
  round16$date <- "05-17-2017"
  
round17 <- read_excel("Data/Nigeria/round17.xlsx") %>%
  rename(
    lga_name = `LGA`,
    state_name = `State of Displacement`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
    state_orig = `State of Origin of Majority`
  ) %>%
  select(vars)
  round17$date <- "06-26-2017"
  
round18 <- read_excel("Data/Nigeria/round18.xlsx") %>%
  rename(
    lga_name = `LGA`,
    state_name = `State of Displacement`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
    state_orig = `State of Origin of Majority`
  ) %>%
  select(vars)
  round18$date <- "11-23-2017"
  
round19 <- read_excel("Data/Nigeria/round19.xlsx") %>%
  rename(
    lga_name = `LGA`,
    state_name = `State of Displacement`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
    state_orig = `State of Origin of Majority`
  ) %>%
  select(vars)
  round19$date <- "09-30-2017"
  
round20 <- read_excel("Data/Nigeria/round20.xlsx") %>%
  rename(
    lga_name = `LGA`,
    state_name = `State of Displacement`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
    state_orig = `State of Origin of Majority`
  ) %>%
  select(vars)
  round20$date <- "12-08-2017"
  
round21 <- read_excel("Data/Nigeria/round21.xlsx") %>%
  rename(
    lga_name = `LGA`,
    state_name = `State of Displacement`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
    state_orig = `State of Origin of Majority`
  ) %>%
  select(vars)
  round21$date <- "01-31-2018"
  
round22 <- read_excel("Data/Nigeria/round22.xlsx") %>%
  rename(
    lga_name = `LGA`,
    state_name = `State of Displacement`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
    state_orig = `State of Origin of Majority`
  ) %>%
  select(vars)
  round22$date <- "04-30-2018"
  
round23  <- read_excel("Data/Nigeria/round23.xlsx") %>%
  rename(
    lga_name = `LGA`,
    state_name = `State of Displacement`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
    state_orig = `State of Origin of Majority`
  ) %>%
  select(vars)
  round23$date <- "06-16-2018"

round24 <- read_excel("Data/Nigeria/round24.xlsx") %>%
  rename(
    lga_name = `LGA`,
    state_name = `State of Displacement`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
    state_orig = `State of Origin of Majority`
  ) %>%
  select(vars)
  round24$date <- "08-06-2018"

round25 <- read_excel("Data/Nigeria/round25.xlsx") %>%
  rename(
    lga_name = `LGA`,
    state_name = `State of Displacement`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
    state_orig = `State of Origin of Majority`
  ) %>%
  select(vars)
  round25$date <- "10-20-2018"
  
round26 <- read_excel("Data/Nigeria/round26.xlsx") %>%
  rename(
    lga_name = `LGA`,
    state_name = `State of Displacement`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
    state_orig = `State of Origin of Majority`
  ) %>%
  select(vars)
  round26$date <- "01-20-2019"

round27 <- read_excel("Data/Nigeria/round27.xlsx") %>%
  rename(
    lga_name = `LGA`,
    state_name = `State of Displacement`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
    state_orig = `State of Origin of Majority`
  ) %>%
  select(vars)
  round27$date <- "05-29-2019"
  
round28 <- read_excel("Data/Nigeria/round28.xlsx") %>%
  rename(
    lga_name = `LGA`,
    state_name = `State of Displacement`,
    estimate_hh_Ward = `Estimated Household Number`,
    estimate_Ind_Ward = `Estimated Number of IDP`,
    lga_orig = `LGA of Origin of Majority`,
    state_orig = `State of Origin of Majority`
  ) %>%
  select(vars)
  round28$date <- "09-16-2019"

######  
  
# Merge data
#####
nigeria <- rbind(round4, round5, round6, round7, round8, round9, round10, round11, round12, 
                 round13, round14, round15, round16, round17, round18, round19, round20, 
                 round21, round22, round23, round24, round25, round26, round27, round28,
                 by=c("lga_name", "state_name", "lga_orig", "state_orig"))
  nigeria$estimate_hh_Ward <- as.numeric(nigeria$estimate_hh_Ward)
  nigeria$estimate_Ind_Ward <- as.numeric(nigeria$estimate_Ind_Ward)
  # Rename certain place names for consistency
  nigeria$lga_name[nigeria$lga_name == "ARDO - KOLA"] <- "ARDO-KOLA"
  nigeria$lga_name[nigeria$lga_name == "ASKIRA / UBA"] <- "ASKIRA/UBA"
  nigeria$lga_name[nigeria$lga_name == "KWAYA / KUSAR"] <- "KWAYA/KUSAR"
  nigeria$lga_name[nigeria$lga_name == "MAIDUGURI M. C."] <- "MAIDUGURI"
  nigeria$lga_name[nigeria$lga_name == "MAYO - BELWA"] <- "MAYO-BELWA"
  nigeria$lga_name[nigeria$lga_name == "YALMALTU/ DEBA"] <- "YALAMALTU/DEBA"
  nigeria$state_name[nigeria$state_name == "FCT"] <- "FEDERAL CAPITAL TERRITORY"
  # Convert to factors
  nigeria$lga_name <- as.factor(nigeria$lga_name)
  nigeria$state_name <- as.factor(nigeria$state_name)
  nigeria$lga_orig <- as.factor(nigeria$lga_orig)
  nigeria$state_orig <- as.factor(nigeria$state_orig)

# Simplify to monthly data
  nigeria$year <- substr(nigeria$date,7,10)
  nigeria$month <- substr(nigeria$date,1,2)  
    
nigeria <- nigeria %>%
  group_by(lga_name, state_name, lga_orig, state_orig, year, month) %>%
  summarize(estimate_hh = sum(estimate_hh_Ward), estimate_ind = sum(estimate_Ind_Ward))
#####

# Population data
#####
pop_adm2 <- read_csv("Data/Nigeria/nga_pop_adm2_2016.csv")
pop_adm2 <- pop_adm2 %>% select(admin2Name_en, admin1Name_en, Population2016)
pop_adm2$admin2Name_en <- toupper(pop_adm2$admin2Name_en)
pop_adm2$admin2Name_en <- as.factor(pop_adm2$admin2Name_en)
pop_adm2$admin1Name_en <- toupper(pop_adm2$admin1Name_en)
pop_adm2$admin1Name_en <- as.factor(pop_adm2$admin1Name_en)

# Origin population
nigeria <- merge(nigeria, pop_adm2, by.x=c("lga_orig", "state_orig"), 
                 by.y=c("admin2Name_en", "admin1Name_en"))

# Destination population
nigeria <- merge(nigeria, pop_adm2, by.x=c("lga_name", "state_orig"), 
                 by.y=c("admin2Name_en", "admin1Name_en"))

nigeria <- nigeria %>% rename(origin_pop = Population2016.x,
                              dest_pop = Population2016.y)

#####

# ACLED
#####
# acled <- read_csv("Data/Nigeria/2018-02-27-2021-03-03-Nigeria.csv")
# # Only data available to me for past three years
# acled$date <- strptime(acled$event_date, "%d %B %Y")
# acled$month <- format(as.Date(acled$date), "%m")
# 
# acled <- acled %>% select(event_type, admin2, admin1, month, year, fatalities)
# acled$admin1 <- toupper(acled$admin1)
# acled$admin2 <- toupper(acled$admin2)
# 
# acled$event_type <- as.factor(acled$event_type)
# 
# acled <- acled %>% group_by(event_type, admin2, admin1, month, year) %>%
#   summarize(fatalities = sum(fatalities))
# 
# acled <- pivot_wider(acled, names_from = event_type, values_from = fatalities, 
#                      values_fill = 0)
# 
# acled <- acled %>% rename(battle.fatal = `Battles`,
#                           violence.fatal = `Violence against civilians`,
#                           riots.fatal = `Riots`,
#                           explosions.fatal = `Explosions/Remote violence`)
# 
# nigeria <- merge(x=nigeria, y=acled, by.x=c("lga_orig", "state_orig", "year", 
#                 "month"), by.y=c("admin2", "admin1", "year", "month"))
# nigeria[is.na(nigeria)] <- 0

# ACLED data back to 1997
acled97 <- read_csv("Data/Nigeria/acledFatalitiesMonthlyNigeriaAdmin2.xlsx - Sheet1.csv")

acled97 <- acled97 %>% 
  replace(is.na(.),0)
acled97$ADMIN2 <- toupper(acled97$ADMIN2)

acled97 <- pivot_longer(acled97, !ADMIN2, names_to = "date", values_to="fatalities")
acled97$year <- substr(acled97$date,4,7)
acled97$month <- substr(acled97$date,1,2)  

nigeria <- merge(x=nigeria, y=acled97, by.x=c("lga_orig", "year", "month"),
                 by.y=c("ADMIN2", "year", "month"))
# Merge again on destination
nigeria <- merge(x=nigeria, y=acled97, by.x=c("lga_name", "year", "month"),
                 by.y=c("ADMIN2", "year", "month"))
nigeria[is.na(nigeria)] <- 0

nigeria <- nigeria %>%
  rename(
    fatal.origin = fatalities.x,
    fatal.dest = fatalities.y
  )

#####

# Spatial data
#####
shape <- shapefile("Data/nga_adm_osgof_20190417/nga_admbnda_adm2_osgof_20190417.shp")
shape$ADM2_EN <- toupper(shape$ADM2_EN)
shape$ADM1_EN <- toupper(shape$ADM1_EN)

adm2_codes <- as.data.frame(shape@data)
adm2_codes <- adm2_codes %>% dplyr::select(ADM2_EN, ADM2_PCODE, ADM1_EN)

row.names(shape) = shape$ADM2_PCODE

nb <- poly2nb(shape)
nb

plot(shape, col='gray', border='blue')

xy <- coordinates(shape)

plot(nb, xy, col='red', add=T)

nb.df <- neighborsDataFrame(nb)
nb.df

nb.df$neighbor <- 1
nb.df <- as.data.frame(complete(nb.df, id, id_neigh, fill=list(neighbor=0)))

nb.df <- merge(nb.df, adm2_codes, by.x=c("id"), by.y=c("ADM2_PCODE"))
nb.df <- merge(nb.df, adm2_codes, by.x=c("id_neigh"), by.y=c("ADM2_PCODE"))

nb.df <- nb.df %>% rename(
  id_dest = `id`,
  id_orig = `id_neigh`,
  lga_name = `ADM2_EN.x`,
  lga_orig = `ADM2_EN.y`,
  state_name = `ADM1_EN.x`,
  state_orig = `ADM1_EN.y`
)

nbweights <- nb2listw(nb, style="W", zero.policy=T)

#OasisR::distance(spatobj = shape)
summary(nigeria)

nigeria <- merge(nigeria, nb.df, by=c("lga_name", "state_name", "lga_orig",
                                     "state_orig"))

sf <- st_as_sf(shape)

dist <- CreateDistMatrix(sf, sf)
dist <- melt(dist)[melt(upper.tri(dist))$value,]

# create copy of dist matrix
# swap orig and dest columns
# append to original matrix
# boom, you have a full matrix

dist2 <- dist
dist2 <- dist2[,c(2,1,3)]
distance <- rbind(dist, dist2)

nigeria <- merge(nigeria, distance, by.x=c("id_orig", "id_dest"), 
                 by.y=c("Var1", "Var2"))
nigeria <- nigeria %>% rename(
  distance = value
)

summary(nigeria)

nigeria <- nigeria %>%
  mutate(origin_pop = origin_pop / 1000,
         dest_pop = dest_pop / 1000,
         distance = distance / 1000)

#####

# Models

# OLS
summary(ols_ind <- lm(estimate_ind ~ origin_pop + dest_pop + fatal.origin + 
                    fatal.dest + origin_pop*fatal.origin + dest_pop*fatal.dest +
                    neighbor + distance, data=nigeria))
# take out interactions

summary(ols_hh <- lm(estimate_hh ~ origin_pop + dest_pop + fatal.origin + 
                    fatal.dest + origin_pop*fatal.origin + dest_pop*fatal.dest +
                    neighbor + distance, data=nigeria))


# Potential next steps to improve model(s)
# forecasting in addition to now-casting
# local/global spatial models
# Full ACLED data (I was only given permission for past three years)
# ACLED event counts in addition to fatalities
# More precise distance data between LGAs / weight
# Additional predictors
# distance from foreign border?
# Lag variables
  # perhaps ACLED events are more useful when considering the prior month
# Any other ideas?
# zero inflated model, include complete pairs of LGAs, including instances of 
# zero IDPs

# Poisson
summary(p_hh <- glm(estimate_hh ~ origin_pop + dest_pop + fatal.origin +
                      fatal.dest + neighbor + distance, 
    data=nigeria, family = 'poisson'))

summary(p_ind <- glm(estimate_ind ~ origin_pop + dest_pop + fatal.origin + 
                       fatal.dest + neighbor + distance, 
    data=nigeria, family = 'poisson'))

# Zero inflated
g <- ggplot(nigeria, aes(x=estimate_ind)) + geom_density()
ggplotly(g)

## CHECK: Is this right? Set all observations below certain value to zero
summary(nigeria$estimate_hh)
nigeria$estimate_hh <- ifelse(nigeria$estimate_hh<1000,0,nigeria$estimate_hh)

# Zero inflated Poisson
summary(zp_hh <- glm(estimate_hh ~ origin_pop + dest_pop + fatal.dest + 
                       fatal.origin + neighbor + distance, 
    data=nigeria, family = 'poisson'))

nigeria$estimate_ind <- ifelse(nigeria$estimate_ind<2000,0,nigeria$estimate_ind)


summary(zp_ind <- glm(estimate_ind ~ origin_pop + dest_pop + fatal.dest +
                        fatal.origin + neighbor + distance, 
    data=nigeria, family = 'poisson'))

# negative binomial, will probably have similar answer, try
# non-independence based on origin
# cluster at originating LGA
# lock down standard errors
# theorize about destinations
# nowcast why we see flows from place a to place b
# understand relative attractiveness of destination
# measure accumulated number of displaced people in LGA up to t minus one
# use 2015-2018 data to predict, how accurately can it predict 2019?
# look at fatalities at destination

summary(zp_ind2 <- glm(estimate_ind ~ origin_pop + dest_pop + fatal.dest +
                      fatal.origin + origin_pop*fatal.origin + 
                      dest_pop*fatal.dest + neighbor + distance, data=nigeria, 
                      family = 'poisson'))


# Load to github, document
# Out of sample prediction
# Separate training months vs testing months
# 2/3 training months 1/3 testing months
# Try testing in stata
