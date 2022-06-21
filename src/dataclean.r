# load packages
library(tidyverse)
## Read in the .dta files
hornung_dt_cs <- readstata13::read.dta13("../data/Population&Rail/hornung-rail-cross-section.dta")
hornung_dt_ps <- readstata13::read.dta13("../data/Population&Rail/hornung-rail-panel-city.dta")

turnen <- read.csv("../data/Proxy Nationalism/Turnvereine Prussia.csv", sep = ";")

colnames(turnen)
turn <- turnen %>% dplyr::select(city, Jahr, Gesamt, Beruf_Handwerk, Beruf_Gelehrte, Beruf_Kaufleute)

head(turn)
# selecting for city, year, popcivil
## head(hornung_dt_ps)
# select only the years relevant, so year 1842

dat <- hornung_dt_ps %>%
  dplyr::select(city, year, popcivil, kreiskey1849) %>%
  dplyr::filter(year %in% c(1864, 1843)) %>%
  group_by(city, kreiskey1849) %>%
  summarise(delta = (popcivil / lag(popcivil) - 1)) %>%
  filter(!is.na(delta))


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

dplyr::filter(matches, is_matched == FALSE)

turn$city <- gsub("Preussisch-Eylau", "Pr. Eylau", turn$city)
