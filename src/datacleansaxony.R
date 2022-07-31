# load packages
library(tidyverse)
require(readstata13)

## Read in the .dta file Turnvereine in the Kingdom of Saxony

turnersaxony <- read.csv("../data/Proxy Nationalism/Turnvereine Saxony/Turnvereine Kingdom of Saxony.csv",
                         sep = ";")

turnsaxony <- turnersaxony %>% dplyr::select(
  cityname, Gesamt, Beruf_Handwerk,
  Beruf_Gelehrte, Beruf_Kaufleute
)


# Group by City
turnsaxony <- turnsaxony %>%
  group_by(cityname)%>%
  summarise(Gesamt = sum(Gesamt),
            Beruf_Handwerk = sum(Beruf_Handwerk),
            Beruf_Gelehrte = sum(Beruf_Gelehrte),
            Beruf_Kaufleute = sum(Beruf_Kaufleute))

# Read in Occupation Census Saxony, remember the Census only contains Data for the
# 103 biggest cities. Since there ar 203 Cities containing Turnvereine we obviously 
# loose at least 100 when matching them.

occupation_saxony <- read.csv("../data/Data Proxy Industrializtaion/Berufsstatistik Sachsen 1861.csv",
                              sep = ";")%>%
  transmute(cityname,
            farmer_saxony_1861 = se_agr_for,
            industryworker_saxony_1861 = se_min + se_fac_man,
            craftmens_saxony_1861 = se_craft,
            otherworkers_saxony_1861 = se_sum_trd_ser + se_sum_IV + se_oth,
            citypop = se_sum + rel_sum)%>%
  
  select(cityname, farmer_saxony_1861, industryworker_saxony_1861, craftmens_saxony_1861,
         otherworkers_saxony_1861, citypop)

# Match Cities in Census with Turncities, 90 of the 103 Cities in the Census 
# contain a Turnverein

occupation_saxony <- left_join(occupation_saxony, turnsaxony)

# Run analysis

occupation_saxony %>%
  ggplot(aes(industryworker_saxony_1861)) +
  geom_histogram()


occupation_saxony %>%
  ggplot(aes(Gesamt)) +
  geom_histogram()

# Get the correlation coefficient 

cor(occupation_saxony$Gesamt, occupation_saxony$industryworker_saxony_1861)


# creating a new dataframe with just the shares. 
saxony_share <- occupation_saxony %>% transmute(
  citypop,
  farmer_saxony_1861,
  industryworker_saxony_1861,
  Gesamt,
  craftmens_saxony_1861,
  turner_sharesaxony = Gesamt/citypop,
  farmer_sharesaxony = farmer_saxony_1861/citypop,
  industryworker_sharesaxony = industryworker_saxony_1861/citypop,
  craftsmen_sharesaxony = craftmens_saxony_1861/citypop,)

###########################################################################
#                 MAIN MODEL Kingdom of Saxony only Industry
###########################################################################

models_saxony <- list()


models_saxony$basic <- lm(data = saxony_share, turner_sharesaxony ~ industryworker_sharesaxony)


###########################################################################
# MODEL NAMES
modelnames_saxony <- c("Industry worker share")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models_saxony,
          out = "../publish/full_modelsaxony.tex",
          title = "Linear Model Results Saxony",
          covariate.labels = modelnames_saxony,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(models_saxony,type = "text",
          out = "../misc/modelsaxony.txt",
          title = "Linear Model Results Saxony",
          covariate.labels = modelnames_saxony,
          dep.var.labels = "Share of Turner")


###########################################################################
#                 SUB MODEL Kingdom of Saxony Farmer
###########################################################################

models_farmersaxony <- list()


models_farmersaxony$with_farmer <- lm(data = saxony_share, turner_sharesaxony ~ farmer_sharesaxony)

###########################################################################
# MODEL NAMES
modelnames_farmersaxony <- c("Farmer share")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models_farmersaxony,
          out = "../publish/full_modelfarmersaxony.tex",
          title = "Linear Model Results Saxony Farmer",
          covariate.labels = modelnames_farmersaxony,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(models_farmersaxony,type = "text",
          out = "../misc/modelfarmersaxony.txt",
          title = "Linear Model Results Saxony",
          covariate.labels = modelnames_farmersaxony,
          dep.var.labels = "Share of Turner")

###########################################################################
#                 MAIN MODEL Kingdom of Saxony
###########################################################################

 models_craftsaxony <- list()


models_craftsaxony$with_craft <- lm(data = saxony_share, turner_sharesaxony ~ craftsmen_sharesaxony)

###########################################################################
# MODEL NAMES
modelnames_craftsaxony <- c("Craftsmen share")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models_craftsaxony,
          out = "../publish/full_modelcraftsaxony.tex",
          title = "Linear Model Results Saxony Craft",
          covariate.labels = modelnames_craftsaxony,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(models_craftsaxony,type = "text",
          out = "../misc/modelcraftsaxony.txt",
          title = "Linear Model Results Saxony Craft",
          covariate.labels = modelnames_craftsaxony,
          dep.var.labels = "Share of Turner")

###########################################################################
#                 MAIN MODEL Kingdom of Saxony Absolut Numbers
###########################################################################

models_saxony <- list()

models_saxony$basic <- lm(data = saxony_share, Gesamt ~ citypop)

models_saxony$ind <- lm(data = saxony_share, Gesamt ~ citypop + 
                            industryworker_saxony_1861)

models_saxony$full <- lm(data = saxony_share, Gesamt ~ citypop + 
                            industryworker_saxony_1861 + 
                            craftmens_saxony_1861)


###########################################################################
# MODEL NAMES
modelnames_saxonyabsolut <- c("Citypopulation",
                       "Industry worker share",
                       "Craftsmen")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models_saxony,
          out = "../publish/full_modelsaxonyabsolut.tex",
          title = "Linear Model Results Saxony",
          covariate.labels = modelnames_saxonyabsolut,
          dep.var.labels = "Number of Turner", font.size = "tiny")


stargazer(models_saxony,type = "text",
          out = "../misc/modelsaxonyabsolut.txt",
          title = "Linear Model Results Saxony",
          covariate.labels = modelnames_saxonyabsolut,
          dep.var.labels = "Number of Turner")

