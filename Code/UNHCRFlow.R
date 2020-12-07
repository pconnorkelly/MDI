# UNHCR Flow
# Author: Connor Kelly
# Date: Dec 2, 2020

# Packages
library(readxl)
library(tidyverse)
library(plotly)

# Set Parameters
startyear <- "2000" # specifies which year we want to start our observations (MIN: 1962, MAX: 2018)
n <- 10 # specifies # of top destinations to display
# These parameters can be changed as needed

############
# Setup and Initial Cleaning
############
setwd("C:/Users/Connor/Documents/GitHub/MDI")
flow <- read_excel("Data/new_ref_arrival_new_asy_app_1962_2019.xlsx", skip = 1)

flow[is.na(flow)] <- 0 # Replace missing values as 0

# Start by looking at origin
flow_origin <- flow %>% # Aggregate by country origin
  group_by(`Origin`, `Population type`) %>%
  summarise_if(is.numeric, sum)

flow_sum <- flow_origin %>% # Create new population type "total" equal to sum of asylum seekers and refugees from given country of origin
  group_by(`Origin`) %>%
  summarise_if(is.numeric, sum) 
flow_sum$`Population type` <- "Total" # Specify population type name
flow_sum <- flow_sum[,c(1, 60, 2:59)] # Reorder to standard format

flow_origin <- bind_rows(flow_origin, flow_sum) # Add total to original dataframe

# Priority Countries
origin <- data.frame(t(flow_sum)) # Transpose dataframe for ease of use
colnames(origin) <- flow_sum$Origin # Change column names to countries
origin <- origin[-c(1,2),] # Remove redundant rows
origin$year <- rownames(origin) # change row names to years
origin$year <- as.numeric(origin$year) # Create year variable
origin <- origin[c(231, 1:230)] # Bring year variable to front

origin[, 1:231] <- sapply(origin[, 1:231], as.character) # Convert data to numeric
# Unclear why I need to first convert from factor to character and THEN character to numeric but this works
origin[, 1:231] <- sapply(origin[, 1:231], as.numeric)##

############
# Flows from Country of Origin
############

# Afghanistan
afg_p <- ggplot(origin, aes(year, Afghanistan)) + geom_line() + ggtitle("Refugees and Aylum Seekers from Afghanistan") +
  ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(afg_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x")

# Bangladesh
bgd_p <- ggplot(origin, aes(year, Bangladesh)) + geom_line() + ggtitle("Refugees and Aylum Seekers from Bangladesh") +
  ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(bgd_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x") 

# Burundi
bdi_p <- ggplot(origin, aes(year, Burundi)) + geom_line() + ggtitle("Refugees and Aylum Seekers from Burundi") + 
  ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(bdi_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x")

# Central African Republic
caf_p <- ggplot(origin, aes(year, `Central African Rep.`)) + geom_line() + 
  ggtitle("Refugees and Aylum Seekers from Central African Republic") + ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(caf_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x")

# Chad
tcd_p <- ggplot(origin, aes(year, Chad)) + geom_line() + ggtitle("Refugees and Aylum Seekers from Chad") + 
  ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(tcd_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x")

# Ethiopia
eth_p <- ggplot(origin, aes(year, Ethiopia)) + geom_line() + ggtitle("Refugees and Aylum Seekers from Ethiopia") + 
  ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(eth_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x")

# Guatemala
gtm_p <- ggplot(origin, aes(year, Guatemala)) + geom_line() + ggtitle("Refugees and Aylum Seekers from Guatemala") +
  ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(gtm_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x")

# Iraq
irq_p <- ggplot(origin, aes(year, Iraq)) + geom_line() + ggtitle("Refugees and Aylum Seekers from Iraq") + 
  ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(irq_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x")

# Libya
lby_p <- ggplot(origin, aes(year, Libya)) + geom_line() + ggtitle("Refugees and Aylum Seekers from Libya") + 
  ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(lby_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x")

# Mali
mli_p <- ggplot(origin, aes(year, Mali)) + geom_line() + ggtitle("Refugees and Aylum Seekers from Mali") + 
  ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(mli_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x")

# Myanmar
mmr_p <- ggplot(origin, aes(year, Myanmar)) + geom_line() + ggtitle("Refugees and Aylum Seekers from Myanmar") + 
  ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(mmr_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x")

# Nigeria
nga_p <- ggplot(origin, aes(year, Nigeria)) + geom_line() + ggtitle("Refugees and Aylum Seekers from Nigeria") + 
  ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(nga_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x")

# Somalia
som_p <- ggplot(origin, aes(year, Somalia)) + geom_line() + ggtitle("Refugees and Aylum Seekers from Somalia") + 
  ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(som_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x")

# South Sudan
ssd_p <- ggplot(origin, aes(year, `South Sudan`)) + geom_line() + ggtitle("Refugees and Aylum Seekers from South Sudan") +
  ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(ssd_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x")

# Sudan
sdn_p <- ggplot(origin, aes(year, Sudan)) + geom_line() + ggtitle("Refugees and Aylum Seekers from Sudan") + 
  ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(sdn_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x")

# Syria
syr_p <- ggplot(origin, aes(year, `Syrian Arab Rep.`)) + geom_line() + ggtitle("Refugees and Aylum Seekers from Syria") +
  ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(syr_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x")

# Venezuela
ven_p <- ggplot(origin, aes(year, `Venezuela (Bolivarian Republic of)`)) + geom_line() + 
  ggtitle("Refugees and Aylum Seekers from Venezuela") + ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(ven_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x")

# Yemen
yem_p <- ggplot(origin, aes(year, Yemen)) + geom_line() + ggtitle("Refugees and Aylum Seekers from Yemen") + 
  ylab("Refugees and Asylum Seekers") + xlab("Year")
ggplotly(yem_p, dynamicTicks = TRUE) %>%
  layout(hovermode = "x")

############
# Flow Destination (Country of asylum)
############
############
# Afghanistan: Destinations
############
afg <- flow %>% 
  filter(Origin == "Afghanistan") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

afg <- pivot_longer(afg, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- afg %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

afg <- afg %>% filter(`Country of asylum` %in% top) # Only include top destinations

afg$year <- as.numeric(afg$year)
afg$`Country of asylum` <- factor(afg$`Country of asylum`, levels = top) # Sort by top destinations

afg_p <- ggplot(afg, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of Afghan Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(afg_p, dynamicTicks = TRUE)

############
# Bangladesh: Destinations
############
bgd <- flow %>% 
  filter(Origin == "Bangladesh") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

bgd <- pivot_longer(bgd, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- bgd %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

bgd <- bgd %>% filter(`Country of asylum` %in% top) # Only include top destinations

bgd$year <- as.numeric(bgd$year)
bgd$`Country of asylum` <- factor(bgd$`Country of asylum`, levels = top) # Sort by top destinations

bgd_p <- ggplot(bgd, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of Bangladeshi Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(bgd_p, dynamicTicks = TRUE)

############
# Burundi: Destinations
############
bdi <- flow %>% 
  filter(Origin == "Burundi") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

bdi <- pivot_longer(bdi, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- bdi %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

bdi <- bdi %>% filter(`Country of asylum` %in% top) # Only include top destinations

bdi$year <- as.numeric(bdi$year)
bdi$`Country of asylum` <- factor(bdi$`Country of asylum`, levels = top) # Sort by top destinations

bdi_p <- ggplot(bdi, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of Burundian Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(bdi_p, dynamicTicks = TRUE)

############
# Central African Republic: Destinations
############
caf <- flow %>% 
  filter(Origin == "Central African Rep.") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

caf <- pivot_longer(caf, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- caf %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

caf <- caf %>% filter(`Country of asylum` %in% top) # Only include top destinations

caf$year <- as.numeric(caf$year)
caf$`Country of asylum` <- factor(caf$`Country of asylum`, levels = top) # Sort by top destinations

caf_p <- ggplot(caf, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of Central African Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(caf_p, dynamicTicks = TRUE)

############
# Chad: Destinations
############
tcd <- flow %>% 
  filter(Origin == "Chad") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

tcd <- pivot_longer(tcd, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- tcd %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

tcd <- tcd %>% filter(`Country of asylum` %in% top) # Only include top destinations

tcd$year <- as.numeric(tcd$year)
tcd$`Country of asylum` <- factor(tcd$`Country of asylum`, levels = top) # Sort by top destinations

tcd_p <- ggplot(tcd, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of Chadian Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(tcd_p, dynamicTicks = TRUE)

############
# Ethiopia: Destinations
############
eth <- flow %>% 
  filter(Origin == "Ethiopia") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

eth <- pivot_longer(eth, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- eth %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

eth <- eth %>% filter(`Country of asylum` %in% top) # Only include top destinations

eth$year <- as.numeric(eth$year)
eth$`Country of asylum` <- factor(eth$`Country of asylum`, levels = top) # Sort by top destinations

eth_p <- ggplot(eth, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of Ethiopian Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(eth_p, dynamicTicks = TRUE)

############
# Guatemala: Destinations
############
gtm <- flow %>% 
  filter(Origin == "Guatemala") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

gtm <- pivot_longer(gtm, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- gtm %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

gtm <- gtm %>% filter(`Country of asylum` %in% top) # Only include top destinations

gtm$year <- as.numeric(gtm$year)
gtm$`Country of asylum` <- factor(gtm$`Country of asylum`, levels = top) # Sort by top destinations

gtm_p <- ggplot(gtm, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of Guatemalan Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(gtm_p, dynamicTicks = TRUE)

############
# Iraq: Destinations
############
irq <- flow %>% 
  filter(Origin == "Iraq") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

irq <- pivot_longer(irq, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- irq %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

irq <- irq %>% filter(`Country of asylum` %in% top) # Only include top destinations

irq$year <- as.numeric(irq$year)
irq$`Country of asylum` <- factor(irq$`Country of asylum`, levels = top) # Sort by top destinations

irq_p <- ggplot(irq, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of Iraqi Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(irq_p, dynamicTicks = TRUE)

############
# Libya: Destinations
############
lby <- flow %>% 
  filter(Origin == "Libya") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

lby <- pivot_longer(lby, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- lby %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

lby <- lby %>% filter(`Country of asylum` %in% top) # Only include top destinations

lby$year <- as.numeric(lby$year)
lby$`Country of asylum` <- factor(lby$`Country of asylum`, levels = top) # Sort by top destinations

lby_p <- ggplot(lby, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of Libyan Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(lby_p, dynamicTicks = TRUE)

############
# Mali: Destinations
############
mli <- flow %>% 
  filter(Origin == "Mali") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

mli <- pivot_longer(mli, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- mli %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

mli <- mli %>% filter(`Country of asylum` %in% top) # Only include top destinations

mli$year <- as.numeric(mli$year)
mli$`Country of asylum` <- factor(mli$`Country of asylum`, levels = top) # Sort by top destinations

mli_p <- ggplot(mli, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of Malian Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(mli_p, dynamicTicks = TRUE)

############
# Myanmar: Destinations
############
mmr <- flow %>% 
  filter(Origin == "Myanmar") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

mmr <- pivot_longer(mmr, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- mmr %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

mmr <- mmr %>% filter(`Country of asylum` %in% top) # Only include top destinations

mmr$year <- as.numeric(mmr$year)
mmr$`Country of asylum` <- factor(mmr$`Country of asylum`, levels = top) # Sort by top destinations

mmr_p <- ggplot(mmr, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of Burmese Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(mmr_p, dynamicTicks = TRUE)

############
# Nigeria: Destinations
############
nga <- flow %>% 
  filter(Origin == "Nigeria") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

nga <- pivot_longer(nga, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- nga %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

nga <- nga %>% filter(`Country of asylum` %in% top) # Only include top destinations

nga$year <- as.numeric(nga$year)
nga$`Country of asylum` <- factor(nga$`Country of asylum`, levels = top) # Sort by top destinations

nga_p <- ggplot(nga, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of Nigerian Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(nga_p, dynamicTicks = TRUE)

############
# Somalia: Destinations
############
som <- flow %>% 
  filter(Origin == "Somalia") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

som <- pivot_longer(som, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- som %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

som <- som %>% filter(`Country of asylum` %in% top) # Only include top destinations

som$year <- as.numeric(som$year)
som$`Country of asylum` <- factor(som$`Country of asylum`, levels = top) # Sort by top destinations

som_p <- ggplot(som, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of Somali Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(som_p, dynamicTicks = TRUE)

############
# South Sudan: Destinations
############
ssd <- flow %>% 
  filter(Origin == "South Sudan") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

ssd <- pivot_longer(ssd, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- ssd %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

ssd <- ssd %>% filter(`Country of asylum` %in% top) # Only include top destinations

ssd$year <- as.numeric(ssd$year)
ssd$`Country of asylum` <- factor(ssd$`Country of asylum`, levels = top) # Sort by top destinations

ssd_p <- ggplot(ssd, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of South Sudanese Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(ssd_p, dynamicTicks = TRUE)

############
# Sudan: Destinations
############
sdn <- flow %>% 
  filter(Origin == "Sudan") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

sdn <- pivot_longer(sdn, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- sdn %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

sdn <- sdn %>% filter(`Country of asylum` %in% top) # Only include top destinations

sdn$year <- as.numeric(sdn$year)
sdn$`Country of asylum` <- factor(sdn$`Country of asylum`, levels = top) # Sort by top destinations

sdn_p <- ggplot(sdn, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of Sudanese Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(sdn_p, dynamicTicks = TRUE)

############
# Syria: Destinations
############
syr <- flow %>% 
  filter(Origin == "Syrian Arab Rep.") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

syr <- pivot_longer(syr, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- syr %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

syr <- syr %>% filter(`Country of asylum` %in% top) # Only include top destinations

syr$year <- as.numeric(syr$year)
syr$`Country of asylum` <- factor(syr$`Country of asylum`, levels = top) # Sort by top destinations

syr_p <- ggplot(syr, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of Syrian Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(syr_p, dynamicTicks = TRUE)

############
# Venezuela: Destinations
############
ven <- flow %>% 
  filter(Origin == "Venezuela (Bolivarian Republic of)") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

ven <- pivot_longer(ven, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- ven %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

ven <- ven %>% filter(`Country of asylum` %in% top) # Only include top destinations

ven$year <- as.numeric(ven$year)
ven$`Country of asylum` <- factor(ven$`Country of asylum`, levels = top) # Sort by top destinations

ven_p <- ggplot(ven, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of Venezuelan Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(ven_p, dynamicTicks = TRUE)

############
# Yemen: Destinations
############
yem <- flow %>% 
  filter(Origin == "Yemen") %>%
  group_by(`Country of asylum`) %>%
  summarize_if(is.numeric, sum)

yem <- pivot_longer(yem, # Make wide data long for easier plotting
                    cols = c(startyear:59),
                    names_to = "year",
                    values_to = "asylumseekers")

top <- yem %>% group_by(`Country of asylum`) %>% # Identify top destinations
  summarize(total = sum(asylumseekers)) %>%
  arrange(desc(total))
top <- head(top$`Country of asylum`, n=n) # save top destinations as string

yem <- yem %>% filter(`Country of asylum` %in% top) # Only include top destinations

yem$year <- as.numeric(yem$year)
yem$`Country of asylum` <- factor(yem$`Country of asylum`, levels = top) # Sort by top destinations

yem_p <- ggplot(yem, aes(x=year, y=asylumseekers, color=`Country of asylum`)) + geom_line() + 
  ggtitle("Top Destinations of Yemeni Refugees and Asylum Seeekers") + xlab("Year") + ylab("Refugees and Asylum Seekers")
ggplotly(yem_p, dynamicTicks = TRUE)
