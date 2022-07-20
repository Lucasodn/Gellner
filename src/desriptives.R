# load packages
library(tidyverse)
require(readstata13)

# Read in for plotting existing and founding Trunvereine

foundingturner <- read.csv("../data/Proxy Nationalism/Graphfounding.csv",
                          sep = ";")

# Plot
foundingturner %>%
  tail(58) %>%
  ggplot(aes(x=Year, y=Newly.Founded)) +
  geom_point() +
  labs(title = "Newly founded Turnvereine over time",
       x ="Year",
       y = "Number of Turnvereine") + theme_minimal()

# this command saves the last plot
ggsave("../publish/foundingturner.png", width = 7, height = 7)


ggtitle("Newly founded Trunvereine") 



foundingturner %>%
  tail(58) %>%
  ggplot( aes(x=Year, y=Existing)) +
  geom_line() +
  geom_point()
ggtitle("Existing Turnvereine")