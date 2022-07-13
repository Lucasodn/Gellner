pacman::p_load(tidyverse,
               pastecs,
               stargazer)

# this is our analysis file. first we load in the data. select only columns without NA for industry workers.
dat <- readRDS("../data/turner_share.RDS") %>%
  distinct() %>%
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

# get the correlation coefficient, which is 0.44
cor(dat$Gesamt64, dat$occ1864_ind)


# creating a new dataframe with just the shares. 
dat_share <- dat %>% transmute(
  kreiskey1864,
  turner_share = Gesamt64/countypop1864, 
  indworker_share1864 = occ1864_ind/countypop1864,
  prot_share1864 = rel1864_pro/countypop1864,
  cat_share1864 = rel1864_cat/countypop1864,
  otherrel_share1864 = rel1864_other/countypop1864,
  student_share1864 = students1864/countypop1864,
  textileworker_share1849 = textileworker1849/countypop1849,
  metal_miningworker_share1849 = metal_miningworker1849/countypop1849,
  other_factoryworker_share1849 = other_factoryworker1849/countypop1849,
  farmer_share1849 = farmer1849/countypop1849,
  crafter_share1849 = crafters/countypop1849,
  steamengines_mining1849 = steamengines1849/metal_miningworker1849,
  steamengines_capita1849 = steamengines1849/countypop1849,
  student_share1849 = students1849/countypop1849)

# Regression industry worker and Truner
###########################################################################
#                 MAIN MODEL 1864
###########################################################################

models <- list()


models$basic <- lm(data = dat_share, turner_share ~ indworker_share)

models$with_prot <- lm(data = dat_share, turner_share ~ indworker_share +
                                                 prot_share)

models$with_cat <- lm(data = dat_share, turner_share ~ indworker_share +
                                             cat_share)

models$with_edu <- lm(data = dat_share, turner_share ~ indworker_share +
                                        student_share)

models$full <- lm(data = dat_share, turner_share ~ indworker_share +
                                 prot_share +
                                 cat_share +
                                 student_share)
###########################################################################
# MODEL NAMES
modelnames <- c("Industry worker share",
                "Protestant share",
                "Catholic share",
                "Enrollment rate")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models,
          out = "../publish/full_model.tex",
          title = "Linear Model Results",
          covariate.labels = modelnames,
          dep.var.labels = "Share of Turner", font.size = "tiny")


# Regression industry worker and Truner
###########################################################################
#                 MAIN MODEL 1849
###########################################################################

models <- list()


models$basic1849 <- lm(data = dat_share, turner_share ~ textileworker_share1849 +
                         metal_miningworker_share1849 + other_factoryworker_share1849)

models$with_crafters <- lm(data = dat_share, turner_share ~ textileworker_share1849 +
                             metal_miningworker_share1849 + other_factoryworker_share1849
                           + crafter_share1849)

models$with_farmer <- lm(data = dat_share, turner_share ~ textileworker_share1849 +
                           metal_miningworker_share1849 + other_factoryworker_share1849
                         + farmer_share1849)

models$with_steamengines <- lm(data = dat_share, turner_share ~ textileworker_share1849 +
                                 metal_miningworker_share1849 + other_factoryworker_share1849
                          + steamengines_capita1849)

models$with_education <- lm(data = dat_share, turner_share ~ textileworker_share1849 +
                                 metal_miningworker_share1849 + other_factoryworker_share1849
                               + student_share1849)

models$full1849 <- lm(data = dat_share, turner_share ~ textileworker_share1849 +
                        metal_miningworker_share1849 + other_factoryworker_share1849
                      + crafter_share1849
                      + farmer_share1849
                      + steamengines_capita1849
                      + student_share1849)
###########################################################################
# MODEL NAMES
modelnames <- c("Factory worker share",
                "Craftmens share",
                "Farmer share",
                "Steamengines",
                "School Enrollment rate")


###########################################################################
# EXTRACT MODEL FOR PUBLISHING
stargazer(models,
          out = "../publish/full_model1849.tex",
          title = "Linear Model Results by Industrialization in 1849",
          covariate.labels = modelnames,
          dep.var.labels = "Share of Turner", font.size = "tiny")

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










