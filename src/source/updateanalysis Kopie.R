pacman::p_load(tidyverse,
               pastecs,
               stargazer,
               coefplot,
               caret)



# this is our analysis file. first we load in the data. select only columns without NA for industry workers.
dat <- readRDS("../data/turner_sharechange.RDS") %>%
  distinct()


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
                                                              servs = mean(servs),
                                                              area1816_qkm = mean(area1816_qkm))

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
                                                              servs = mean(servs),
                                                              area1816_qkm = mean(area1816_qkm))

scatter.smooth(dat1$countypop1864, dat1$occ1864_ind, main='Countypop vs. Ind worker')
scatter.smooth(dat1$Gesamt64, dat1$countypop1864, main='Countypop vs. Turner')
scatter.smooth(dat1$Gesamt64, dat1$occ1864_ind, main='Ind worker vs. ETurner')
scatter.smooth(dat1$Gesamt64, dat1$steamengines1849, main='Steam Engines vs. Turner')
scatter.smooth(dat1$Gesamt64, dat1$students1849, main='Students 1849 vs. Turner')
scatter.smooth(dat1$Gesamt64, dat1$students1864, main='Students 1864 vs. Turner')

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
  ggplot(aes(countypop1864)) +
  geom_histogram()

dat1 %>%
  ggplot( aes(x=Gesamt64)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

# get the correlation coefficient between Industryworker and Turner, which is 0.51
dat.cor <- dat1[complete.cases(dat1),]

cor(dat.cor$Gesamt64, dat.cor$occ1864_ind)

shapiro.test(dat1$Gesamt64)
shapiro.test(dat1$occ1864_ind)

cor.test(dat1$Gesamt64, dat1$occ1864_ind, 
         method = "pearson")
cor.test(dat1$Gesamt64, dat1$countypop1864, 
         method = "pearson")

# creating a new dataframe with the shares. 
dat_share <- dat1 %>% transmute(
  kreiskey1864,
  countypop1864,
  Gesamt64,
  occ1864_ind,
  rel1864_pro,
  students1864,
  area1816_qkm,
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
  indworker_share15to65 = occ1864_ind/pop1864_tot_15to65,
  popgrowth = countypop1864/countypop1849 - 1)

mean(dat_share$turner_share)

# Correlationcoefficient Matrix 1864 

test <- data.frame(dat_share$turner_share, dat_share$indworker_share1864, dat_share$prot_share1864, 
                   dat_share$student_share1864, dat_share$area1816_qkm)

cor.matrix <- cor(test, use="complete.obs", method = "pearson")

modelnames <- c("",
                "Turner",
                "Industry worker",
                "Protestants",
                "Students",
                "Countyarea")


stargazer(cor.matrix, out = "../publish/final publish/Correlation Matrix 1864.tex",
          covariate.labels = modelnames,
          title = "Correlation Matrix 1864 Shares", font.size = "tiny")

# Correlationcoefficient Matrix 1849

test <- data.frame(dat_share$turner_share, dat_share$prot_share1849, dat_share$student_share1849,
                   dat_share$factoryworker_share1849, dat_share$crafter_share1849,
                   dat_share$steamengines_capita1849, dat_share$farmer_share1849, dat_share$area1816_qkm)

cor.matrix <- cor(test, use="complete.obs", method = "pearson")

modelnames <- c("",
                "Turner",
                "Protestants",
                "Students",
                "Factory worker",
                "Craftsmen",
                "Steam Engines",
                "Farmer",
                "Countyarea")


stargazer(cor.matrix, out = "../publish/final publish/Correlation Matrix 1849.tex",
          covariate.labels = modelnames,
          title = "Correlation Matrix 1849 Shares", font.size = "tiny")

# Get the Correlation coefficient for Turnershare and Indushare, which is 0,286

dat.cor <- dat_shareindu[complete.cases(dat_shareindu),]

cor(dat.cor$turner_share, dat.cor$indworker_share1864)

shapiro.test(dat_share$turner_share)
shapiro.test(dat_share$indworker_share1864)

cor.test(dat_share$turner_share, dat_share$indworker_share1864, 
         method = "pearson")


# Distribution of Data
dat_share %>%
  ggplot(aes(indworker_share1864)) +
  geom_histogram()

dat_share %>%
  ggplot(aes(turner_share)) +
  geom_histogram()


scatter.smooth(turner_share, indworker_share1864, main='Turner and Industry worker')

plot(turner_share, farmer_share1849)


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

models$with_edu <- lm(data = dat_shareindu, turner_share ~ indworker_share1864 +
                        student_share1864)


models$with_prot <- lm(data = dat_shareindu, turner_share ~ indworker_share1864 +
                         prot_share1864)


models$full <- lm(data = dat_shareindu, turner_share ~ indworker_share1864 +
                    student_share1864+
                    prot_share1864 +
                    area1816_qkm)

coefplot(models$full)

# VIF(models$full)


# Wichtig für appendix

plot(models$basic, 1)
plot(models$basic, 3)
plot(models$full, 1)
plot(models$full, 3)

# Residuals vs inverse normal
# Residuals vs metrischen Variablen


#produce residual vs. fitted plot
res <- resid(models$basic)
plot(fitted(models$basic), res)

qqnorm(res)
qqline(res)

###########################################################################
# MODEL NAMES
modelnames <- c("Industry worker share 1864",
                "Enrollment rate 1864",
                "Protestant share 1864",
                "Countysize")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models,
          out = "../publish/final publish/full_modelindustry1864update.tex",
          title = "Linear Model Results Industry Worker 1864",
          covariate.labels = modelnames,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(models,type = "text",
          out = "../misc/modelindustry1864update.txt",
          title = "Linear Model Results Industry Worker 1864",
          covariate.labels = modelnames,
          dep.var.labels = "Share of Turner")

###########################################################################
#                 MAIN MODEL Enrollment Rate 1864 oder 1849
###########################################################################
#Without Berlin
#dat_share <- dat_share[!(dat_share$kreiskey1864 == 85),]

models_edu <- list()


models_edu$basic <- lm(data = dat_share, turner_share ~ student_share1849)

models_edu$with_prot <- lm(data = dat_share, turner_share ~ student_share1849 +
                         prot_share1849)

coefplot(models_edu$with_prot)


plot(models_edu$basic, 1)
plot(models_edu$basic, 3)
plot(models_edu$with_prot, 1)
plot(models_edu$with_prot, 3)


#produce residual vs. fitted plot
res <- resid(models_edu$basic)
plot(fitted(models_edu$basic), res)

qqnorm(res)
qqline(res)

###########################################################################
# MODEL NAMES
modelnames_edu <- c("Enrollment rate",
                "Protestant share")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models_edu,
          out = "../publish/full_modeleducation1864.tex",
          title = "Linear Model Results Enrollement Rate 1864",
          covariate.labels = modelnames_edu,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(models_edu,type = "text",
          out = "../misc/modeleducation1864.txt",
          title = "Linear Model results Enrollement Rate 1864",
          covariate.labels = modelnames_edu,
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
                               prot_share1849)

models_steam$with_edu <- lm(data = dat_share, turner_share ~ steamengines_capita1849 +
                              student_share1849)

models_steam$full <- lm(data = dat_share, turner_share ~ steamengines_capita1849 +
                          prot_share1849 +
                          student_share1849 +
                          area1816_qkm)

#dev.off()
coefplot(models_steam$full)
VIF(models_steam$full)

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
                     "Enrollment rate",
                     "Countysize")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models_steam,
          out = "../publish/final publish/full_modelsteampercapitaupdate.tex",
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
                                prot_share1849)

models_farmer$with_edu <- lm(data = dat_share, turner_share ~ farmer_share1849 +
                               student_share1849)

models_farmer$full <- lm(data = dat_share, turner_share ~ farmer_share1849 +
                           prot_share1849 +
                           student_share1849)
###########################################################################
# MODEL NAMES
modelnames_farmer<- c("Farmer share 1849",
                      "Protestant share 1849",
                      "Enrollment rate 1849")


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
modelnames_crafts<- c("Craftsmen share 1849",
                      "Protestant share 1849",
                      "Enrollment rate 1849")


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
                               prot_share1849)

models_metal$with_craft <- lm(data = dat_share, turner_share ~ metal_miningworker_share1849 +
                              student_share1849)

models_metal$full <- lm(data = dat_share, turner_share ~ metal_miningworker_share1849 +
                          prot_share1849 +
                          student_share1849)
coefplot(models_metal$full)
plot(models_metal$basic, 1)
plot(models_metal$basic, 3)
plot(models_metal$full, 1)
plot(models_metal$full, 3)
###########################################################################
# MODEL NAMES
modelnames_metal<- c("Metal and Mining share 1849",
                     "Protestant share 1849",
                     "Enrollment rate 1849")


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
                                 prot_share1849)

models_factory$with_edu <- lm(data = dat_share, turner_share ~ factoryworker_share1849 +
                                student_share1849)

models_factory$with_craft <- lm(data = dat_share, turner_share ~ factoryworker_share1849 +
                                crafter_share1849)

models_factory$with_farmer <- lm(data = dat_share, turner_share ~ factoryworker_share1849 +
                                  farmer_share1849)

models_factory$full <- lm(data = dat_share, turner_share ~ factoryworker_share1849 +
                            prot_share1849 +
                            student_share1849 +
                            crafter_share1849 +
                            farmer_share1849 + 
                            area1816_qkm)
coefplot(models_factory$full)
VIF(models_factory$full)

plot(models_factory$basic, 1)
plot(models_factory$basic, 3)
plot(models_factory$full, 1)
plot(models_factory$full, 3)
###########################################################################
# MODEL NAMES
modelnames_factory<- c("Factory worker share 1849",
                       "Protestant share 1849",
                       "Enrollment rate 1849",
                       "Craftsmen share 1849",
                       "Farmer share 1849",
                       "Countysize")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models_factory,
          out = "../publish/final publish/full_modelfactoryupdate.tex",
          title = "Linear Model Results Factory",
          covariate.labels = modelnames_factory,
          dep.var.labels = "Share of Turner 1864", font.size = "tiny")


stargazer(models_factory,
          type = "text",
          out = "../misc/modelfactoryupdate.txt",
          title = "Linear Model Results Factory",
          covariate.labels = modelnames_factory,
          dep.var.labels = "Share of Turner")
############################################################################

###########################################################################
#                 MAIN MODEL Metal and  other Factory 1849
###########################################################################

models_metalandther <- list()


models_metalandther$basic <- lm(data = dat_share, turner_share ~ metal_miningworker_share1849)

models_metalandther$with_prot <- lm(data = dat_share, turner_share ~ metal_miningworker_share1849 +
                               prot_share1864)

models_metalandther$with_edu <- lm(data = dat_share, turner_share ~ metal_miningworker_share1849 +
                              other_factoryworker_share1849)

models_metalandther$full <- lm(data = dat_share, turner_share ~ metal_miningworker_share1849 +
                          prot_share1864 +
                          other_factoryworker_share1849)
plot(models_metalandther$basic, 1)
plot(models_metalandther$basic, 3)
plot(models_metalandther$full, 1)
plot(models_metalandther$full, 3)
###########################################################################
# MODEL NAMES
modelnames_metalandther<- c("Metal and Mining share",
                     "Protestant share",
                     "Other Factory Worker")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models_metalandther,
          out = "../publish/full_modelmetalandother.tex",
          title = "Linear Model Results Metal and Other share",
          covariate.labels = modelnames_metalandother,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(models_metalandther,
          type = "text",
          out = "../misc/modelmetalandother.txt",
          title = "Linear Model Results Metal and Other share",
          covariate.labels = modelnames_metalandother,
          dep.var.labels = "Share of Turner")


###########################################################################
#                 MAIN MODEL County Size Industry worker Share 1864
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

modelscitysize <- list()


modelscitysize$basic <- lm(data = dat_shareindu, turner_share ~ countypop1864)

modelscitysize$with_ind <- lm(data = dat_shareindu, turner_share ~ countypop1864 +
                                indworker_share1864)


modelscitysize$with_prot <- lm(data = dat_shareindu, turner_share ~ countypop1864 +
                                prot_share1864)

models$with_edu <- lm(data = dat_shareindu, turner_share ~ countypop1864 +
                       student_share1864)

modelscitysize$full <- lm(data = dat_shareindu, turner_share ~ countypop1864 +
                            indworker_share1864 +
                            prot_share1864 + 
                            student_share1864)


plot(modelscitysize$basic, 1)
plot(modelscitysize$basic, 3)
plot(modelscitysize$full, 1)
plot(modelscitysize$full, 3)


#produce residual vs. fitted plot
res <- resid(modelscitysize$basic)
plot(fitted(modelscitysize$basic), res)

qqnorm(res)
qqline(res)

###########################################################################
# MODEL NAMES
modelnames_citysize <- c("County population",
                        "Industry worker share",
                        "Protestant share",
                        "Enrollment rate")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(modelscitysize,
          out = "../publish/full_modelindustryshare1864countypopulation.tex",
          title = "Linear Model Results County Population 1864",
          covariate.labels = modelnames_citysize,
          dep.var.labels = "Share of Turner", font.size = "tiny")


stargazer(modelscitysize,type = "text",
          out = "../misc/modelindustryshare1864countypopulation.txt",
          title = "Linear Model Results County Population 1864",
          covariate.labels = modelnames_citysize,
          dep.var.labels = "Share of Turner")


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























