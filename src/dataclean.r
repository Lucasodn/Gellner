# load packages
library(tidyverse)
## Read in the .dta files
hornung_dt_cs <- readstata13::read.dta13("../data/Population&Rail/hornung-rail-cross-section.dta")
hornung_dt_ps <- readstata13::read.dta13("../data/Population&Rail/hornung-rail-panel-city.dta")

turnen <- read.csv("../data/Proxy Nationalism/Turnvereine Prussia.csv", sep = ";")

colnames(turnen)
turn <- turnen %>% dplyr::select(city, Jahr, Gesamt, Beruf_Handwerk, Beruf_Gelehrte, Beruf_Kaufleute)

turn[turn == "na"] <- "Missing Value"



dat <- hornung_dt_ps %>%
  dplyr::select(townkey1849, city, year, popcivil, kreiskey1849) %>%
  dplyr::filter(year %in% c(1864, 1843))


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

# This is a function to check mismatched names between two arrays.
# First, get a unique array of names,
a <- unique(dat$city)
b <- unique(turn$city)

# Then, run the match_checker.

matches <- match_checker(a, b)

# if you print matches, it will return the list with T and F.

matches

# if you run this, you can see how many remain:

print("cases matched:")
sum(matches$is_matched)
print("cases remaining:")
sum(!matches$is_matched)

write.csv(dplyr::filter(matches,
                        is_matched == FALSE),
          "../data/Proxy Nationalism/matches_reference.csv",
          sep = ";")

# The issue here is the matching is not working currently. it seems ot be an issue with the CSV encoding.


matchmaker <- read.csv("../data/Proxy Nationalism/name_matches.csv", sep=";")

matchmaker$Old


for (i in 1:length(matchmaker$Old)) {
  turn$city <- gsub(matchmaker$Old[i], matchmaker$New[i], turn$city)
}

turn <- turn %>% filter(!(city == ""))

#Drop Variable Jahr (Gründungsjahr Verein)
turn <- turn %>% select(!Jahr)
dat <- dat %>% select(!Jahr)



# Merge Turner to Hornung Cities
maindf <- left_join(dat, turn, by = "city")

# replace NAs with 0s, as missing values mean no turnverien present.
maindf$Gesamt <- ifelse(is.na(maindf$Gesamt), 0, maindf$Gesamt)
maindf$Beruf_Handwerk <- ifelse(is.na(maindf$Beruf_Handwerk), 0, maindf$Beruf_Handwerk)
maindf$Beruf_Gelehrte <- ifelse(is.na(maindf$Beruf_Gelehrte), 0, maindf$Beruf_Gelehrte)
maindf$Beruf_Kaufleute <- ifelse(is.na(maindf$Beruf_Kaufleute), 0, maindf$Beruf_Kaufleute)


maindf[maindf == "Missing Value"] <- NA


maindf <- maindf %>% mutate(
  Gesamt = as.integer(Gesamt),
  Beruf_Handwerk = as.integer(Beruf_Handwerk),
  Beruf_Gelehrte = as.integer(Beruf_Gelehrte),
  Beruf_Kaufleute = as.integer(Beruf_Kaufleute))


# We want to sum "Gesamt" Turner in same city and calculate Turner share on population "popcivil"

maindf <- as_tibble(maindf)

# Processing maindf to summarize total number of turner per town, not per turnverein.
# the 64 at the end of the variable name is designed to highlight the fact that the number of
# turner is only for the year 1864
maindf <- maindf %>%
  group_by(townkey1849, city, year, kreiskey1849) %>%
  summarise(popcivil = mean(popcivil),
            Gesamt64 = sum(Gesamt),
            Beruf_Handwerk_64 = sum(Beruf_Handwerk),
            Beruf_Gelehrte_64 = sum(Beruf_Gelehrte),
            Beruf_Kaufleute_64 = sum(Beruf_Kaufleute))
  


