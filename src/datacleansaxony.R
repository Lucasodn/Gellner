# load packages
library(tidyverse)
require(readstata13)

## Read in the .dta files

turnersaxony <- read.csv("../data/Proxy Nationalism/Turnvereine Kingdom of Saxony.csv",
                         sep = ";")

turnsaxony <- turnersaxony %>% dplyr::select(
  city, Gesamt, Beruf_Handwerk,
  Beruf_Gelehrte, Beruf_Kaufleute
)

turnsaxony <- turnsaxony %>%
  group_by(city)%>%
  summarise(Gesamt = sum(Gesamt),
            Beruf_Handwerk = sum(Beruf_Handwerk),
            Beruf_Gelehrte = sum(Beruf_Gelehrte),
            Beruf_Kaufleute = sum(Beruf_Kaufleute))


# Replace missing values with na
turnsaxony[turnsaxony == "na"] <- "Missing Value"

# Read in Occupation Census Saxony

occupation_saxony <- read.csv("../data/Data Proxy Industrializtaion/Berufsstatistik Sachsen 1861.csv",
                              sep = ";")%>%
  transmute(city,
            farmer_saxony_1861 = se_agr_for,
            industryworker_saxony_1861 = se_min + se_fac_man,
            craftmens_saxony_1861 = se_craft,
            otherworkers_saxony_1861 = se_sum_trd_ser + se_sum_IV + se_oth,
            citypop = se_sum + rel_sum)%>%
  
  select(city, farmer_saxony_1861, industryworker_saxony_1861, craftmens_saxony_1861,
         otherworkers_saxony_1861, citypop)

turnsaxony <- left_join(turnsaxony, occupation_saxony)

view(turnsaxony)
