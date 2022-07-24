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

# Occupational status of the Turnverein members

Occupation <- c("Farmers", "Craftsmen", "Artisans and Factory Workers", "Merchants",
                "Students", "Technicians and Artists", "Clergymen", "Doctors", 
                "Teachers and Writers", "Advocates", "Soldiers", "Other professions")
Number <- c(11258, 75501, 12989, 35101, 2361, 5496, 
         292, 2803, 4573, 5125, 1484, 11020)

occupation <- data.frame(Occupation, Number)

print(occupation)

occtab <- matrix(c(0.07, 0.45, 0.08, 0.21, 0.01, 0.03, 0.002, 
                   0.02, 0.03, 0.03, 0.01, 0.07), ncol=1, byrow=TRUE)
colnames(occtab) <- c("Total Number")
rownames(occtab) <- c("Farmers", "Craftsmen", "Artisans and Factory Workers", "Merchants",
                "Students", "Technicians and Artists", "Clergymen", "Doctors", 
                "Teachers and Writers", "Advocates", "Soldiers", "Other professions")
occtab <- as.table(occtab)

# Simple Horizontal Bar Plot with Added Labels
counts <- table(occtab)
barplot(counts, main="Occupation", horiz=TRUE, las=1,
        names.arg=c("Farmers", "Craftsmen", "Artisans and Factory Workers", "Merchants",
                    "Students", "Technicians and Artists", "Clergymen", "Doctors", 
                    "Teachers and Writers", "Advocates", "Soldiers", "Other professions"))



occstatus <- read.csv("../data/Proxy Nationalism/Oupatianal status.csv", sep = ";")

occstatus <- occstatus %>% mutate(
  Occupation = as.factor(Occupation))

barplot(occstatus$Occupation ~ occstatus$Total.Number,
        horiz = TRUE)


