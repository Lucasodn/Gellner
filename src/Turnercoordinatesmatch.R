# # load packages
# library(tidyverse)
# require(readstata13)
# 
# install.packages("pacman")
# 
# pacman::p_load(tidyverse, readstata13)
# 
# # Read in file containing all Turnvereine
# 
# allturner <- read.csv("../data/Proxy Nationalism/Allturnvereine 1.csv", sep = ";")
# 
# 
# 
# # Select important variables for Turner
# allturn <- allturner %>% dplyr::select(
#   cityname, Jahr, Gesamt, Beruf_Handwerk,
#   Beruf_Gelehrte, Beruf_Kaufleute, district
# )
# 
# # Read in file containing coordinates for Turncities
# finalturncoordinates <- readstata13::read.dta13("../data/Proxy Nationalism/Final turnstädte.dta" ) %>%
#   dplyr::select(
#   longitude, latitude, cityname, district)
# 
# 
# 
# # Select important variables for Turner
# finalturncoordinates1 <- finalturncoordinates 
# 
# allturncoordinates <- left_join(allturn, finalturncoordinates)
# view(allturncoordinates)
# 
# allturncoordinates1 <- distinct(allturncoordinates)
# 
# 
# # write.csv(allturncoordinates1, file = "../data/allturncoordinates.csv")
# 
# # chracter Problem set as integer
# allturncoordinates1 <- allturncoordinates1 %>% mutate(
#   Gesamt = as.integer(Gesamt),
#   eruf_Handwerk = as.integer(Beruf_Handwerk),
#   Beruf_Gelehrte = as.integer(Beruf_Gelehrte),
#   Beruf_Kaufleute = as.integer(Beruf_Kaufleute),
#   Jahr = as.integer(Jahr)
# )
# 
# 
# view(allturncoordinates1)
# 
# 
# # group by city to construct heatmap in QGIS
# allturncoordinates1 <- allturncoordinates1 %>%
#   group_by(cityname, longitude, latitude) %>%
#   summarise(
#     #longitude = mean(longitude),
#     #latitude = mean(latitude),
#     Gesamt_new = sum(Gesamt)
#     #Beruf_Handwerk_new = sum(Beruf_Handwerk),
#     #Beruf_Gelehrte_new = sum(Beruf_Gelehrte),
#     #Beruf_Kaufleute_new = sum(Beruf_Kaufleute)
#   )
# 
# allturncoordinates1 <- distinct(allturncoordinates)
# view(allturncoordinates1)
# 

###########################################################################
###########################################################################
########### DATA ABOVE IS JUST FOR REFERENCE 

#loading in packages.
pacman::p_load(tidyverse, ape)

# !!!! remember to setwd() to src file.!!!! 
 
# loading in coordinates data for Turnvereine. turnvereine with 0 members are considered
# missing values. Observations that do not have long or lat data are removed.
# this one is for capturing with number of members in Turnverein.
dat <- read.csv("../data/allturncoordinates.csv", sep = ";") %>%
  select(cityname, Gesamt, longitude, latitude) %>% 
  mutate(Gesamt = ifelse(Gesamt %in% c("na", "", "0"), NA, as.integer(Gesamt))) %>%
  filter(!is.na(longitude) | !is.na(latitude)) %>%
  group_by(cityname, longitude, latitude) %>%
  summarise(Gesamt = sum(Gesamt, na.rm = TRUE))

# this one just captures of an existing turnverein. 
dat_existing_turnverein <- read.csv("../data/allturncoordinates.csv", sep = ";") %>%
  select(cityname, longitude, latitude) %>% 
  filter(!is.na(longitude) | !is.na(latitude)) %>%
  group_by(cityname, latitude, longitude) %>%
  summarise(turnvereine = n())


# Do the same for only Prussia, since regression analyses are run for Prussia
# Again this one captures number of turner
dat_prussia <- read.csv("../data/allturncoordinates.csv", sep = ";") %>%
  select(cityname, Gesamt, longitude, latitude, district) %>% 
  mutate(Gesamt = ifelse(Gesamt %in% c("na", "", "0"), NA, as.integer(Gesamt))) %>%
  filter(!is.na(longitude) | !is.na(latitude), district %in% c("Berlin", "Breslau",  "Bromberg",
         "Coeslin", "Danzig", "Frankfurt Oder", "Gumbinnen", "Königsberg", "Liegnitz", "Magdeburg",
         "Marienwerder", "Merseburg", "Oppeln", "Posen", "Potsdam", "Regierungsbezirk Aachen",
         "Regierungsbezirk Arnsberg", "Regierungsbezirk Düsseldorf", "Regierungsbezirk Erfurt",
         "Regierungsbezirk Koblenz", "Regierungsbezirk Köln", "Regierungsbezirk Minden",
         "Regierungsbezirk Münster", "Regierungsbezirk Trier", "Stettin", "Stralsund")) %>%
  group_by(cityname, longitude, latitude, district) %>%
  summarise(Gesamt = sum(Gesamt, na.rm = TRUE))

# this one just captures of an existing turnverein. 
dat_existing_turnverein_prussia <- read.csv("../data/allturncoordinates.csv", sep = ";") %>%
  select(cityname, longitude, latitude, district) %>% 
  filter(!is.na(longitude) | !is.na(latitude), 
         district %in% c("Berlin", "Breslau",  "Bromberg","Coeslin", "Danzig", 
                         "Frankfurt Oder", "Gumbinnen", "Königsberg", "Liegnitz", 
                         "Magdeburg", "Marienwerder", "Merseburg", "Oppeln", "Posen", 
                         "Potsdam", "Regierungsbezirk Aachen","Regierungsbezirk Arnsberg", 
                         "Regierungsbezirk Düsseldorf", "Regierungsbezirk Erfurt",
                         "Regierungsbezirk Koblenz", "Regierungsbezirk Köln", 
                         "Regierungsbezirk Minden","Regierungsbezirk Münster", 
                         "Regierungsbezirk Trier", "Stettin", "Stralsund")) %>%
  group_by(cityname, latitude, longitude, district) %>%
  summarise(turnvereine = n())


# here we prepare the matrices for calculating the distance between each turnverin.
# each row lets us read off the distance to every other turnverein. that is why the
# diagonal is 0. 
dat_existing_turnverein.dists <- as.matrix(dist(
  cbind(
  dat_existing_turnverein$latitude,
  dat_existing_turnverein$longitude)))

dat_existing_turnverein.dists.inv <- 1/dat_existing_turnverein.dists

diag(dat_existing_turnverein.dists.inv) <- 0

half <-dim(dat_existing_turnverein.dists.inv)[1] / 2

dat_existing_turnverein.dists.inv[1:half, 1:half]
dat_existing_turnverein.dists.inv[is.infinite(dat_existing_turnverein.dists.inv)] <- 0 

# here, we do the same thing for Gesamt, where we capture if there is distance
# autocorrelation dependent on the number of turner, not just the vereine themselves.
dat.dists <- as.matrix(dist(
  cbind(
    dat$latitude,
    dat$longitude)))

dat.dists.inv <- 1/dat.dists

diag(dat.dists.inv) <- 0

half <-dim(dat.dists.inv)[1] / 2

dat.dists.inv[1:half, 1:half]

dat.dists.inv[is.infinite(dat.dists.inv)] <- 0 

# Compute Moran's I

Moran.I(dat_existing_turnverein$turnvereine, dat_existing_turnverein.dists.inv)

Moran.I(dat$Gesamt, dat.dists.inv)


# Now For Turnvereine existing in Prussia
dat_existing_turnverein_prussia.dists <- as.matrix(dist(
  cbind(
    dat_existing_turnverein_prussia$latitude,
    dat_existing_turnverein_prussia$longitude)))

dat_existing_turnverein_prussia.dists.inv <- 1/dat_existing_turnverein_prussia.dists

diag(dat_existing_turnverein_prussia.dists.inv) <- 0

half <-dim(dat_existing_turnverein_prussia.dists.inv)[1] / 2

dat_existing_turnverein_prussia.dists.inv[1:half, 1:half]
dat_existing_turnverein_prussia.dists.inv[is.infinite(dat_existing_turnverein_prussia.dists.inv)] <- 0 

# Lastly for capturing distance autocorrelation based on Gesamt turner

dat_prussia.dists <- as.matrix(dist(
  cbind(
    dat_prussia$latitude,
    dat_prussia$longitude)))

dat_prussia.dists.inv <- 1/dat_prussia.dists

diag(dat_prussia.dists.inv) <- 0

half <-dim(dat_prussia.dists.inv)[1] / 2

dat_prussia.dists.inv[1:half, 1:half]

dat_prussia.dists.inv[is.infinite(dat_prussia.dists.inv)] <- 0 

# Compute Moran's I for Prussia

Moran.I(dat_existing_turnverein_prussia$turnvereine, dat_existing_turnverein_prussia.dists.inv)

Moran.I(dat_prussia$Gesamt, dat_prussia.dists.inv)

# calculating a coefficient for divsion as test
test <- data.frame(x =1, y =  0.17, z = 0.13)

sd(test)



