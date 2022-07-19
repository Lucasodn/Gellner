# load packages
library(tidyverse)
require(readstata13)


# Read in all Trunvereine

allturner <- read.csv("../data/Proxy Nationalism/Allturnvereine 1.csv", sep = ";")

# Select important variables for Turner
allturn <- allturner %>% dplyr::select(
  cityname, Jahr, Gesamt, Beruf_Handwerk,
  Beruf_Gelehrte, Beruf_Kaufleute, district
)


finalturncoordinates <- readstata13::read.dta13("../data/Proxy Nationalism/Final turnstädte.dta")

# Select important variables for Turner
finalturncoordinates1 <- finalturncoordinates %>% dplyr::select(
  longitude, latitude, cityname, district)

allturncoordinates <- left_join(allturn, finalturncoordinates1)

allturncoordinates1 <- distinct(allturncoordinates)
view(allturncoordinates1)



allturncoordinates <- allturncoordinates %>%
  group_by(city) %>%
  summarise(
    longitude = mean(longitude),
    latitude = sum(latitude),
    
    Beruf_Handwerk_64 = sum(Beruf_Handwerk),
    Beruf_Gelehrte_64 = sum(Beruf_Gelehrte),
    Beruf_Kaufleute_64 = sum(Beruf_Kaufleute)
  )

allturncoordinates1 <- distinct(allturncoordinates)
view(allturncoordinates1)
