# load packages
library(tidyverse)
## Read in the .dta files
hornung_dt_cs <- readstata13::read.dta13("../data/Population&Rail/hornung-rail-cross-section.dta")
hornung_dt_ps <- readstata13::read.dta13("../data/Population&Rail/hornung-rail-panel-city.dta")


# selecting for city, year, popcivil
hornung_dt_ps <- dplyr::select(hornung_dt_ps, city, year, popcivil)


# select only the years relevant, so year 1842
class(hornung_dt_ps)

hornung_dt_ps <- dhornung_dt_ps %>%
  dplyr::select(city, year, popcivil) %>%
  dplyr::filter(year %in% c(1864, 1843)) %>%
  ## hornung_dt_ps <- filter(hornung_dt_ps, "year" == 1842)

  head(test)
