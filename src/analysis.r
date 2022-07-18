pacman::p_load(tidyverse,
               pastecs,
               stargazer,
               ggpubr,
               ggscatter)

install.packages("ggpubr")

library(ggpubr)

# this is our analysis file. first we load in the data. select only columns without NA for industry workers.
dat <- readRDS("../data/turner_share.RDS") %>%
  distinct() %>%
  # Exclude all NAs, note that you exclude Berlin, because NA for occ1864
  filter(!is.na(occ1864_ind),
              !is.na(Gesamt64),
              !is.na(countypop1864),
              !is.na(rel1864_pro),
              !is.na(rel1864_cat),
              !is.na(rel1864_other))


dat %>%
  ggplot(aes(occ1864_ind)) +
  geom_histogram()


dat %>%
  ggplot(aes(Gesamt64)) +
  geom_histogram()


# dealing with duisburg essen edge case
edgecase <- filter(dat, kreiskey1849 %in% c(289, 290))

edgecase <- edgecase %>% group_by(kreiskey1849) %>% summarize(popcivil = mean(popcivil),
                                                  Gesamt64 = mean(Gesamt64),
                                                  Beruf_Handwerk_64 = mean(Beruf_Handwerk_64),
                                                  Beruf_Gelehrte_64 = mean(Beruf_Gelehrte_64),
                                                  Beruf_Kaufleute_64 = mean(Beruf_Kaufleute_64),
                                                  kreiskey1864 = as.integer(mean(kreiskey1864)),
                                                  pop1864_tot_15to65 = sum(pop1864_tot_15to65),
                                                  countypop1849 = mean(countypop1849),
                                                  rel1849_pro = mean(rel1849_pro),
                                                  rel1849_cat = mean(rel1849_cat),
                                                  rel1849_other = mean(rel1849_other),
                                                  countypop1864 = sum(countypop1864),
                                                  rel1864_pro = sum(rel1864_pro),
                                                  rel1864_cat = sum(rel1864_cat),
                                                  rel1864_other = sum(rel1864_other),
                                                  occ1864_ind = sum(occ1864_ind),
                                                  students1864 = sum(students1864),
                                                  textileworker1849 = mean(textileworker1849),
                                                  metal_miningworker1849 = mean(metal_miningworker1849),
                                                  other_factoryworker1849 = mean(other_factoryworker1849),
                                                  all_factoryworker1849 = mean(all_factoryworker1849),
                                                  farmer1849 = mean(farmer1849),
                                                  steamengines1849 = mean(steamengines1849),
                                                  crafters = mean(crafters),
                                                  students1849 = mean(students1849))
dat <- dat[-c(which(dat$kreiskey1849 %in% c(289, 290))),]


edgecase
dat <- rbind(dat, edgecase)

# get the correlation coefficient, which is 0.44
cor(dat$Gesamt64, dat$occ1864_ind)


# creating a new dataframe with just the shares. 
dat_share <- dat %>% transmute(
  kreiskey1864,
  countypop1864,
  turner_share = Gesamt64/countypop1864, 
  indworker_share1864 = occ1864_ind/countypop1864,
  prot_share1864 = rel1864_pro/countypop1864,
  cat_share1864 = rel1864_cat/countypop1864,
  otherrel_share1864 = rel1864_other/countypop1864,
  student_share1864 = students1864/countypop1864,
  textileworker_share1849 = textileworker1849/countypop1849,
  metal_miningworker_share1849 = metal_miningworker1849/countypop1849,
  other_factoryworker_share1849 = other_factoryworker1849/countypop1849,
  factoryworker_share1849 = all_factoryworker1849/countypop1849,
  farmer_share1849 = farmer1849/countypop1849,
  crafter_share1849 = crafters/countypop1849,
  steamengines_mining1849 = steamengines1849/metal_miningworker1849,
  steamengines_capita1849 = steamengines1849/countypop1849,
  student_share1849 = students1849/countypop1849,
  prot_share1849 = rel1849_pro/countypop1849,)

view(dat_share)
# Regression industry worker and Truner
###########################################################################
#                 MAIN MODEL Industry worker 1864
###########################################################################

models <- list()


models$basic <- lm(data = dat_share, turner_share ~ indworker_share1864)


models$with_prot <- lm(data = dat_share, turner_share ~ indworker_share1864 +
                         prot_share1864)

models$with_edu <- lm(data = dat_share, turner_share ~ indworker_share1864 +
                        student_share1864)

models$full <- lm(data = dat_share, turner_share ~ indworker_share1864 +
                     prot_share1864 +
                    student_share1864)
###########################################################################
# MODEL NAMES
modelnames <- c("Industry worker share",
                "Protestant share",
                "Enrollment rate")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models,
          out = "../publish/full_modelindustry1864.tex",
          title = "Linear Model Results Industry Worker 1864",
          covariate.labels = modelnames,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(models,type = "text",
          out = "../misc/modelindustry1864.txt",
          title = "Linear Model Results Industry Worker 1864",
          covariate.labels = modelnames,
          dep.var.labels = "Share of Turner")

# Regression industry worker and Turner
###########################################################################
#                 MAIN MODEL Steam Engines 1849 - try per capita
###########################################################################

models_steam <- list()


models_steam$basic <- lm(data = dat_share, turner_share ~ steamengines_capita1849)

models_steam$with_prot <- lm(data = dat_share, turner_share ~ steamengines_capita1849 +
                         prot_share1864)

models_steam$with_edu <- lm(data = dat_share, turner_share ~ steamengines_capita1849 +
                        student_share1864)

models_steam$full <- lm(data = dat_share, turner_share ~ steamengines_capita1849 +
                    prot_share1864 +
                    student_share1864)
###########################################################################
# MODEL NAMES
modelnames_steam<- c("Steam Engines per capita",
                "Protestant share",
                "Enrollment rate")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models_steam,
          out = "../publish/full_modelsteampercapita.tex",
          title = "Linear Model Results Steam Engines per capita",
          covariate.labels = modelnames_steam,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(models_steam
          ,type = "text",
          out = "../misc/modelsteampercapita.txt",
          title = "Linear Model Results Steam Engines per capita",
          covariate.labels = modelnames_steam,
          dep.var.labels = "Share of Turner")


###########################################################################
#                 MAIN MODEL Farmer 1849
###########################################################################

models_farmer <- list()


models_farmer$basic <- lm(data = dat_share, turner_share ~ farmer_share1849)

models_farmer$with_prot <- lm(data = dat_share, turner_share ~ farmer_share1849 +
                               prot_share1864)

models_farmer$with_edu <- lm(data = dat_share, turner_share ~ farmer_share1849 +
                              student_share1864)

models_farmer$full <- lm(data = dat_share, turner_share ~ farmer_share1849 +
                          prot_share1864 +
                          student_share1864)
###########################################################################
# MODEL NAMES
modelnames_farmer<- c("Farmer share",
                     "Protestant share",
                     "Enrollment rate")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models_farmer,
          out = "../publish/full_modelfarmer.tex",
          title = "Linear Model Results Farmer share",
          covariate.labels = modelnames_farmer,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(models_farmer,
          type = "text",
          out = "../misc/modelfarmer.txt",
          title = "Linear Model Results Farmer share",
          covariate.labels = modelnames_farmer,
          dep.var.labels = "Share of Turner")

###########################################################################
#                 MAIN MODEL Craftsmen 1849
###########################################################################

models_crafts <- list()


models_crafts$basic <- lm(data = dat_share, turner_share ~ crafter_share1849)

models_crafts$with_prot <- lm(data = dat_share, turner_share ~ crafter_share1849 +
                                prot_share1864)

models_crafts$with_edu <- lm(data = dat_share, turner_share ~ crafter_share1849 +
                               student_share1864)

models_crafts$full <- lm(data = dat_share, turner_share ~ crafter_share1849 +
                           prot_share1864 +
                           student_share1864)
###########################################################################
# MODEL NAMES
modelnames_crafts<- c("Craftsmen share",
                      "Protestant share",
                      "Enrollment rate")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models_crafts,
          out = "../publish/full_modelcrafts.tex",
          title = "Linear Model Results Craftsmen share",
          covariate.labels = modelnames_crafts,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(models_crafts,
          type = "text",
          out = "../misc/modelcrafts.txt",
          title = "Linear Model Results Craftsmen share",
          covariate.labels = modelnames_farmer,
          dep.var.labels = "Share of Turner")


###########################################################################
#                 MAIN MODEL Metal and Mining Worker 1849
###########################################################################

models_metal <- list()


models_metal$basic <- lm(data = dat_share, turner_share ~ metal_miningworker_share1849)

models_metal$with_prot <- lm(data = dat_share, turner_share ~ metal_miningworker_share1849 +
                                prot_share1864)

models_metal$with_edu <- lm(data = dat_share, turner_share ~ metal_miningworker_share1849 +
                               student_share1864)

models_metal$full <- lm(data = dat_share, turner_share ~ metal_miningworker_share1849 +
                           prot_share1864 +
                           student_share1864)
###########################################################################
# MODEL NAMES
modelnames_metal<- c("Metal and Mining share",
                      "Protestant share",
                      "Enrollment rate")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models_metal,
          out = "../publish/full_modelmetal.tex",
          title = "Linear Model Results Metal and Mining share",
          covariate.labels = modelnames_metal,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(models_metal,
          type = "text",
          out = "../misc/modelmetal.txt",
          title = "Linear Model Results Metal and Mining share",
          covariate.labels = modelnames_metal,
          dep.var.labels = "Share of Turner")


###########################################################################
#                 MAIN MODEL Factory Worker 1849
###########################################################################

models_factory <- list()


models_factory$basic <- lm(data = dat_share, turner_share ~ factoryworker_share1849)

models_factory$with_prot <- lm(data = dat_share, turner_share ~ factoryworker_share1849 +
                               prot_share1864)

models_factory$with_edu <- lm(data = dat_share, turner_share ~ factoryworker_share1849 +
                              student_share1864)

models_factory$full <- lm(data = dat_share, turner_share ~ factoryworker_share1849 +
                          prot_share1864 +
                          student_share1864)
###########################################################################
# MODEL NAMES
modelnames_factory<- c("Factory worker share",
                     "Protestant share",
                     "Enrollment rate")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models_factory,
          out = "../publish/full_modelfactory.tex",
          title = "Linear Model Results Factory",
          covariate.labels = modelnames_factory,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(models_factory,
          type = "text",
          out = "../misc/modelfactory.txt",
          title = "Linear Model Results Factory",
          covariate.labels = modelnames_factory,
          dep.var.labels = "Share of Turner")
############################################################################
            # Plotting some graphs

ggscatter(dat_share, x = "turner_share", y = "indworker_share1864",
          add = lm(data = dat_share, turner_share ~ indworker_share1864), # Add regression line
          conf.int = TRUE,                                  # Add confidence interval
          add.params = list(color = "blue",
                            fill = "lightgray")
)+
  stat_cor(method = "pearson", label.x = 0.2, label.y = 1)















###########################################################################
# Only counties where Turnvereine exist

dat_share1 <- filter(dat_share, turner_share > 0)

models <- list()


models$basic1 <- lm(data = dat_share1, turner_share ~ indworker_share)

models$with_prot1 <- lm(data = dat_share1, turner_share ~ indworker_share +
                         prot_share)

# models$with_cat1 <- lm(data = dat_share1, turner_share ~ indworker_share +
#                       cat_share)

models$with_edu1 <- lm(data = dat_share1, turner_share ~ indworker_share +
                        student_share)

models$full1 <- lm(data = dat_share1, turner_share ~ indworker_share +
                    prot_share +
                    # cat_share +
                    student_share)
###########################################################################
# MODEL NAMES
modelnames <- c("Industry worker share",
                "Protestant share",
                # "Catholic share",
                "Enrollment rate")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models,
          out = "../publish/full_model1.tex",
          title = "Linear Model Results with only Turncities",
          covariate.labels = modelnames,
          dep.var.labels = "Share of Turner", font.size = "tiny")

















models[["basic"]] <- lm(data = dat_share, turner_share ~ indworker_share)

models[["basic_and_controls"]] <- lm(data = dat_share, turner_share ~ indworker_share + prot_share)

summary(models$basic_and_controls)

# Regression education and Truner

cor(dat_share$turner_share, dat_share$student_share)

models[["basic"]] <- lm(data = dat_share, turner_share ~ student_share)

summary(models$basic)

models[["basic_and_controls"]] <- lm(data = dat_share, turner_share ~ indworker_share * student_share + prot_share)

summary(models$basic_and_controls)



# creating a new dataframe with just the shares of Population 15 to 65
dat_share15to65 <- dat %>% transmute(
  kreiskey1864,
  turner_share = Gesamt64/pop1864_tot_15to65, 
  indworker_share = occ1864_ind/pop1864_tot_15to65,
  prot_share = rel1864_pro/pop1864_tot_15to65,
  cat_share = rel1864_cat/pop1864_tot_15to65,
  other_share = rel1864_other/pop1864_tot_15to65
)

#(From Becker) We start with a parsimonious model that controls only for basic 
# demographic and geographic measures, namely the shares of the population aged 
# below 15 and above 60 (and the size of the county area).

cor(dat_share15to65$turner_share, dat_share15to65$indworker_share)

models[["basic"]] <- lm(data = dat_share15to65, turner_share ~ indworker_share)

summary(models$"basic")

models[["basic_and_controls"]] <- lm(data = dat_share15to65, turner_share ~ indworker_share + prot_share + cat_share)
modelnames[["basic_and_controls"]] <- c("Industry Workers", "Protestants", "Catholics")

summary(models$basic_and_controls)




models$full <- lm(data = dat_share15to65, turner_share ~ indworker_share + student_share + prot_share + cat_share) 

# for exporting tables, we use the "publish" directory. naming them .tex at the end allows latex to read them in. 

# export tables for use in latex
stargazer(models$basic_and_controls, out = "../publish/basic_model.tex",
          title = "this is our first tex table!", covariate.labels = modelnames$basic_and_controls)


 stargazer(kreiskeychanger,
           summary = FALSE,
           out = "../publish/descriptive.tex", column.labels = c)










