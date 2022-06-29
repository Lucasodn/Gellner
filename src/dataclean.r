# load packages
library(tidyverse)
require(readstata13)

## Read in the .dta files
# For Proxy Industrialization: population growth
# hornung_dt_cs <- readstata13::read.dta13("../data/Population&Rail/hornung-rail-cross-section.dta")
hornung_dt_ps <- readstata13::read.dta13("../data/Population&Rail/hornung-rail-panel-city.dta")

# Read in the .dta files
# For Proxy Nationalism Turner in Prussia
turnen <- read.csv("../data/Proxy Nationalism/Turnvereine Prussia.csv", sep = ";")

readstata13::read.dta13("../data/Population&Rail/hornung-rail-panel-city.dta")

# Select important variables for Turner
turn <- turnen %>% dplyr::select(
  city, Jahr, Gesamt, Beruf_Handwerk,
  Beruf_Gelehrte, Beruf_Kaufleute
)


# Replace missing values with na
turn[turn == "na"] <- "Missing Value"


# Select important variables for Population census. Kreiskey1849 is particularly
# important to match Turncities to their county, to then later
# match the with other data sets containing data only on county level.

dat <- hornung_dt_ps %>%
  dplyr::select(townkey1849, city, year, popcivil, kreiskey1849) %>%
  dplyr::filter(year %in% c(1864, 1843))


# This is a function to check mismatched names between two arrays.
# First, get a unique array of names,


match_checker <- function(a, b) {
  require(tidyverse)
  if (length(a) > length(b)) {
    longer_list <- a
    shorter_list <- b
  } else {
    longer_list <- b
    shorter_list <- a
  }
  out <- data.frame(name = shorter_list, is_matched = NA)
  for (i in 1:dim(out)[1])
  {
    out$is_matched[i] <- out$name[i] %in% longer_list
  }
  return(out)
}


a <- unique(dat$city)
b <- unique(turn$city)

# Then, run the match_checker.

matches <- match_checker(a, b)

# if you print matches, it will return the list with T and F.

# if you run this, you can see how many remain:

print("cases matched:")
sum(matches$is_matched)
print("cases remaining:")
sum(!matches$is_matched)

write.csv(dplyr::filter(
  matches,
  is_matched == FALSE
),
"../data/Proxy Nationalism/matches_reference.csv",
sep = ";"
)


matchmaker <- read.csv("../data/Proxy Nationalism/name_matches.csv", sep = ";")

for (i in 1:length(matchmaker$Old)) {
  turn$city <- gsub(matchmaker$Old[i], matchmaker$New[i], turn$city)
}

turn <- turn %>% filter(!(city == ""))

# Now manually check the city names of Turner and Hornung cities in csv.file
# and rename them to then match them.
# Run the process again so all possible matches are matched.

# Drop Variable Jahr (Gründungsjahr Verein) because its confusing and not necessary.

## turn <- turn %>% select(!Jahr)
## dat <- dat %>% select(!Jahr)


# Merge the corrected version of Turner to Hornung cities for full match.

maindf <- left_join(dat, turn, by = "city")




# Replace NAs with 0s, as missing values mean no turnvereien present.
maindf$Gesamt <- ifelse(is.na(maindf$Gesamt), 0, maindf$Gesamt)
maindf$Beruf_Handwerk <- ifelse(is.na(maindf$Beruf_Handwerk), 0, maindf$Beruf_Handwerk)
maindf$Beruf_Gelehrte <- ifelse(is.na(maindf$Beruf_Gelehrte), 0, maindf$Beruf_Gelehrte)
maindf$Beruf_Kaufleute <- ifelse(is.na(maindf$Beruf_Kaufleute), 0, maindf$Beruf_Kaufleute)


maindf[maindf == "Missing Value"] <- NA


# Change the Turner variables to integrals, since they were characters.

maindf <- maindf %>% mutate(
  Gesamt = as.integer(Gesamt),
  Beruf_Handwerk = as.integer(Beruf_Handwerk),
  Beruf_Gelehrte = as.integer(Beruf_Gelehrte),
  Beruf_Kaufleute = as.integer(Beruf_Kaufleute)
)


# We want to sum "Gesamt" Turner in same city to calculate Turner share on population.

maindf <- as_tibble(maindf)

# Processing maindf to summarize total number of turner per town, not per turnverein.
# The 64 at the end of the variable name is designed to highlight the fact that the number of
# turner is only for the year 1864.The mean is to not sum up the population
# of a city in which are more than one Turnverein.

maindf <- maindf %>%
  group_by(townkey1849, city, year, kreiskey1849) %>%
  summarise(
    popcivil = mean(popcivil),
    Gesamt64 = sum(Gesamt),
    Beruf_Handwerk_64 = sum(Beruf_Handwerk),
    Beruf_Gelehrte_64 = sum(Beruf_Gelehrte),
    Beruf_Kaufleute_64 = sum(Beruf_Kaufleute)
  )

# Calculate Population Growth from 1843 to 1864.

## pop_growth <- maindf %>%
##   group_by(townkey1849) %>%
##   summarise(pop_growth_rate = (popcivil / lag(popcivil) - 1)) %>%
##   filter(!is.na(pop_growth_rate))

## # Calculate the share of Turnern in Population in 1864.
## turnshare <- maindf %>%
##   filter(year == 1864) %>%
##   group_by(townkey1849) %>%
##   summarize(turner_share = Gesamt64 / popcivil)

# Join variable pop_growth and turnshare to main data set

## maindf <- left_join(maindf, turnshare)
## maindf <- left_join(maindf, pop_growth)

# Two things achieved: First we now can analyse the role of population growth
# on Turner. Second we have data set with Turner matched to their county (Kreiskey)
# and city (townkey) as they are used in other data sets.


maindf[is.na(maindf)] <- 0

maindf <- maindf %>%
  filter(year == 1864) %>%
  group_by(kreiskey1849) %>%
  summarise(
    popcivil = sum(popcivil),
    Gesamt64 = sum(Gesamt64),
    Beruf_Handwerk_64 = sum(Beruf_Handwerk_64),
    Beruf_Gelehrte_64 = sum(Beruf_Gelehrte_64),
    Beruf_Kaufleute_64 = sum(Beruf_Kaufleute_64)
  )

# this list is to help choose which kreiskeys are merged into one.

kreiskeychanger <- data.frame(oldkk = c(4, 40, 65, 249, 286), newkk = c(3, 39, 64, 246, 281))

maindf
# read in file delienating the changes in the kreiskey over time. as the kreiskeys in 1849
# are not the same as in 1864.

mergerfile <- read.csv("../data/ipehd_merge_county.csv", sep = ";") %>%
  select(kreiskey1864, kreiskey1849)

# merge with main dataset.
maindf <- left_join(maindf, mergerfile)

# this function changes the kreiskeys depnding on the old-new data frame provided earler.
# this needs to be given the exact column names. TODO make generalizable
kreiskeycleaner <- function(x, y) {
  for (i in 1:length(x$kreiskey1864)) {
    for (j in 1:length(y$oldkk)) {
      if (x$kreiskey1864[i] == y$oldkk[j]) {
        x$kreiskey1864[i] <- y$newkk[j]
      }
    }
  }
  return(x)
}


# reading in the data set providing the population by age bracket. we use this to get
# a proxy for the working population.

pop15to65 <- read.csv("../data/Data Proxy Industrializtaion/ipehd_1864_pop_demo (1).csv",
  sep = ","
) %>% select(kreiskey1864, pop1864_tot_15to65)

pop15to65 <- kreiskeycleaner(pop15to65, kreiskeychanger)


# Here we are just grouping by doubled or trippled kreiskeys and summing the values
# to get a single value per kreiskey
# egal welche variablen du bei pop15 dazufügst, musst du die summarizen
pop15to65 <- pop15to65 %>%
  group_by(kreiskey1864) %>%
  summarize(pop1864_tot_15to65 = sum(pop1864_tot_15to65))




countypop1849 <- read.csv("../data/Data Proxy Industrializtaion/Religion/ipehd_1849_rel_deno.csv",
  sep = ","
) %>%
  mutate(
    countypop1849 = sum(
      rel1849_pro,
      rel1849_cat,
      rel1849_greek,
      rel1849_menno,
      rel1849_jew
    ),
    rel_other = sum(
      rel1849_greek,
      rel1849_menno,
      rel1849_jew
    )
  )
# doing the same thing for 1864 motherfuckerrrr
countypop1864 <- read.csv("../data/Data Proxy Industrializtaion/Religion/ipehd_1864_rel_deno (1).csv",
                          sep = ",") %>% mutate(countypop1864 = (rel1864_pro +
                                                                   rel1864_cat + 
                                                                   rel1864_jew +
                                                                   rel1864_oth),
                                                rel1864_other = (rel1864_jew +
                                                                   rel1864_oth)) %>%
  select(kreiskey1864,
         countypop1864,
         rel1864_pro,
         rel1864_cat,
         rel1864_other)



countypop1849 <- kreiskeycleaner(countypop1849, kreiskeychanger)
# save file for future procesing
saveRDS(maindf, file = "../data/turner_share.RDS")
