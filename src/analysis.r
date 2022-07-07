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

models[["basic"]] <- lm(data = dat, Gesamt64 ~ occ1864_ind)
models[["basic_and_controls"]] <- lm(data = dat, Gesamt64 ~ occ1864_ind + countypop1864)



# creating a new dataframe with just the shares. 
dat_share <- dat %>% transmute(
  kreiskey1864,
  turner_share = Gesamt64/countypop1864, 
  indworker_share = occ1864_ind/countypop1864,
  prot_share = rel1864_pro/countypop1864,
  cat_share = rel1864_cat/countypop1864,
  other_share = rel1864_other/countypop1864
)

view(dat_share)

cor(dat_share$turner_share, dat_share$indworker_share)

models[["basic"]] <- lm(dat_share = dat_share, turner_share ~ indworker_share)

models[["basic_and_controls"]] <- lm(dat_share = dat_share, Gesamt64 ~ occ1864_ind + countypop1864)





