
# read in cleaned data
turner <- readRDS("../data/turner_share.RDS")


turner
reg_pop_turn.lm <- lm(turner_share ~ pop_growth_rate, data = turner, na.rm = TRUE)
summary(reg_pop_turn.lm)


