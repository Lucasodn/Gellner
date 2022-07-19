# load packages
library(tidyverse)
require(readstata13)

# Read in for plotting existing and founding Trunvereine

foundingturner <- read.csv("../data/Proxy Nationalism/Graphfounding.csv",
                          sep = ";")

# Plot
foundingturner %>%
  tail(58) %>%
  ggplot( aes(x=Year, y=Newly.Founded)) +
  geom_point()
ggtitle("Newly founded Trunvereine")

stargazer(models,
          out = "../publish/full_modelindustry1864.tex",
          title = "Linear Model Results Industry Worker 1864",
          covariate.labels = modelnames,
          dep.var.labels = "Share of Turner", font.size = "tiny")

foundingturner %>%
  tail(58) %>%
  ggplot( aes(x=Year, y=Existing)) +
  geom_line() +
  geom_point()
ggtitle("Existing Turnvereine")