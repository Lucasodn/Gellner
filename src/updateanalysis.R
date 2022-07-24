pacman::p_load(tidyverse,
               pastecs,
               stargazer)


# this is our analysis file. first we load in the data. select only columns without NA for industry workers.
dat <- readRDS("../data/turner_sharechange.RDS") %>%
  distinct()

# # Exclude all NAs, note that you exclude Berlin, because NA for occ1864
# filter(!is.na(occ1864_ind),
#             !is.na(Gesamt64),
#             !is.na(countypop1864),
#             !is.na(rel1864_pro),
#             !is.na(rel1864_cat),
#             !is.na(rel1864_other))

# dealing with Duisburg and Essen as edge case
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
                                                              students1849 = mean(students1849),
                                                              servs = mean(servs))
dat <- dat[-c(which(dat$kreiskey1849 %in% c(289, 290))),]


dat <- rbind(dat, edgecase)



dat1 <- dat %>% group_by(kreiskey1849) %>% summarize(popcivil = mean(popcivil),
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
                                                              students1849 = mean(students1849),
                                                              servs = mean(servs))

scatter.smooth(dat1$countypop1864, dat1$occ1864_ind, main='Hours studied vs. Exam Score')
scatter.smooth(dat1$Gesamt64, dat1$countypop1864, main='Hours studied vs. Exam Score')
scatter.smooth(dat1$Gesamt64, dat1$occ1864_ind, main='Hours studied vs. Exam Score')
scatter.smooth(dat1$Gesamt64, dat1$steamengines1849, main='Hours studied vs. Exam Score')


dat1 %>%
  ggplot(aes(occ1864_ind)) +
  geom_histogram()


dat1 %>%
  ggplot(aes(Gesamt64)) +
  geom_histogram()

dat1 %>%
  ggplot(aes(steamengines1849)) +
  geom_histogram()

dat1 %>%
  ggplot( aes(x=Gesamt64)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

# Chart
p <- ggplot(dat1, aes(x=x) ) +
  # Top
  geom_density( aes(x = Gesamt64, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=4.5, y=0.25, label="variable1"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = occ1864_ind, y = -..density..), fill= "#404080") +
  geom_label( aes(x=4.5, y=-0.25, label="variable2"), color="#404080") +
  xlab("value of x")
# get the correlation coefficient between Industryworker and Turner, which is 0.44
dat.cor <- dat1[complete.cases(dat1),]

cor(dat.cor$Gesamt64, dat.cor$occ1864_ind)


# creating a new dataframe with just the shares. 
dat_share <- dat1 %>% transmute(
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
  prot_share1849 = rel1849_pro/countypop1849,
  turner_share15to65 = Gesamt64/pop1864_tot_15to65,
  indworker_share15to65 = occ1864_ind/pop1864_tot_15to65)

# Get the Correlation coefficient for Turnershare and Indushare, which is 0,286

dat.cor <- dat_share[complete.cases(dat_share),]

cor(dat.cor$turner_share, dat.cor$indworker_share1864)


# Distribution of Data
dat_share %>%
  ggplot(aes(indworker_share1864)) +
  geom_histogram()

dat_share %>%
  ggplot(aes(turner_share)) +
  geom_histogram()


scatter.smooth(turner_share, indworker_share1864, main='Hours studied vs. Exam Score')

boxplot(turner_share)
boxplot(indworker_share1864)

# Regression industry worker and Truner
###########################################################################
#                 MAIN MODEL Industry worker 1864
###########################################################################

#FILTER FOR BERLIN 85, Potsdam 95, Frankfurt 107, 211 Magdeburg, 227 Halle, 
#Münsterkandkreis 249, Köln Landkreis 286, Elberfeld 301,  339 Aachen Landkreis

dat_shareindu <- dat_share[!(dat_share$kreiskey1864 == 85 | 
                               dat_share$kreiskey1864 == 95 | 
                               dat_share$kreiskey1864 == 107 |
                               dat_share$kreiskey1864 == 211 |
                               dat_share$kreiskey1864 == 227 |
                               dat_share$kreiskey1864 == 249 |
                               dat_share$kreiskey1864 == 286 |
                               dat_share$kreiskey1864 == 301 |
                               dat_share$kreiskey1864 == 339),]

models <- list()


models$basic <- lm(data = dat_shareindu, turner_share ~ indworker_share1864)


models$with_prot <- lm(data = dat_shareindu, turner_share ~ indworker_share1864 +
                         prot_share1864)

models$with_edu <- lm(data = dat_shareindu, turner_share ~ indworker_share1864 +
                        student_share1864)

models$full <- lm(data = dat_shareindu, turner_share ~ indworker_share1864 +
                    prot_share1864 +
                    student_share1864)


plot(models$basic, 1)
plot(models$basic, 3)
plot(models$full, 1)
plot(models$full, 3)


#produce residual vs. fitted plot
res <- resid(models$basic)
plot(fitted(models$basic), res)

qqnorm(res)
qqline(res)

###########################################################################
# MODEL NAMES
modelnames <- c("Industry worker share",
                "Protestant share",
                "Enrollment rate")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models,
          out = "../publish/full_modelindustry1864update.tex",
          title = "Linear Model Results Industry Worker 1864",
          covariate.labels = modelnames,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(models,type = "text",
          out = "../misc/modelindustry1864update.txt",
          title = "Linear Model Results Industry Worker 1864",
          covariate.labels = modelnames,
          dep.var.labels = "Share of Turner")

# Regression industry worker and Turner
###########################################################################
#                 MAIN MODEL Steam Engines 1849 - try per capita
###########################################################################
# dat_sharesteam <- dat_share[!(dat_share$kreiskey1864 == 85 | 
#                               dat_share$kreiskey1864 == 294),]


models_steam <- list()


models_steam$basic <- lm(data = dat_share, turner_share ~ steamengines_capita1849)

models_steam$with_prot <- lm(data = dat_share, turner_share ~ steamengines_capita1849 +
                               prot_share1864)

models_steam$with_edu <- lm(data = dat_share, turner_share ~ steamengines_capita1849 +
                              student_share1864)

models_steam$full <- lm(data = dat_share, turner_share ~ steamengines_capita1849 +
                          prot_share1864 +
                          student_share1864)

plot(models_steam$basic, 1)
plot(models_steam$basic, 3)
plot(models_steam$full, 1)
plot(models_steam$full, 3)

res <- resid(models_steam$basic)
plot(fitted(models_steam$basic), res)

qqnorm(res)
###########################################################################
# MODEL NAMES
modelnames_steam<- c("Steam Engines per capita",
                     "Protestant share",
                     "Enrollment rate")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models_steam,
          out = "../publish/full_modelsteampercapitaupdate.tex",
          title = "Linear Model Results Steam Engines per capita",
          covariate.labels = modelnames_steam,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(models_steam
          ,type = "text",
          out = "../misc/modelsteampercapitaupdate.txt",
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
          out = "../publish/full_modelfarmerupdate.tex",
          title = "Linear Model Results Farmer share",
          covariate.labels = modelnames_farmer,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(models_farmer,
          type = "text",
          out = "../misc/modelfarmerupdate.txt",
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
          out = "../publish/full_modelcraftsupdate.tex",
          title = "Linear Model Results Craftsmen share",
          covariate.labels = modelnames_crafts,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(models_crafts,
          type = "text",
          out = "../misc/modelcraftsupdate.txt",
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
plot(models_metal$basic, 1)
plot(models_metal$basic, 3)
plot(models_metal$full, 1)
plot(models_metal$full, 3)
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
plot(models_factory$basic, 1)
plot(models_factory$basic, 3)
plot(models_factory$full, 1)
plot(models_factory$full, 3)
###########################################################################
# MODEL NAMES
modelnames_factory<- c("Factory worker share",
                       "Protestant share",
                       "Enrollment rate")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models_factory,
          out = "../publish/full_modelfactoryupdate.tex",
          title = "Linear Model Results Factory",
          covariate.labels = modelnames_factory,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(models_factory,
          type = "text",
          out = "../misc/modelfactoryupdate.txt",
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























