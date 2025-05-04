library(readr)
library(AER)
df <- read_csv("/Users/gabriellamessenger/Desktop/Math386-2/Math386/Psets/ang_ev_1980.csv")

# part 1

morekids_model <- lm(morekids ~ samesex, data = df)
mom_worked_model <- lm(mom_worked ~ samesex, data=df)
mom_weeks_worked_model <- lm(mom_weeks_worked ~ samesex, data=df)

summary(morekids_model)
# table 5 value: 0.060 vs. 0.059 here ~ almost identical

summary(mom_worked_model)
# table 5 value: -0.0080 vs -0.0076 here ~ very close

summary(mom_weeks_worked_model)
# table 5 value: -0.3826 vs -0.365 here ~ very close

# part 2
iv_mom_worked_model <- ivreg(mom_worked ~ morekids | samesex, data=df)
iv_mom_weeks_worked_model <- ivreg(mom_weeks_worked ~ morekids | samesex, data=df)

summary(iv_mom_worked_model)
# table 5 value: -0.133 vs ~ -0.129 

summary(iv_mom_weeks_worked_model)
# table 5 value: -6.38 vs ~ -6.137

# Both estimates using samesex as the IV for morekids returned values within 0.35 of the reported estimates from the paper.
# mom_worked much closer to reported value than mom_weeks_worked, but both are very similar.


# part 3

# If we divide the beta_1 estimates of outcomes by the beta_1 estimate for treatment, we get the values in part b (approximately).
# i.e.,  beta_1 = (slope of the reduced form) / (slope of the first stage)
# -0.0076 / 0.059 ~ -0.1288
# -0.365 / 0.059 ~ -6.1864


# part 4

# built-in IV regression
iv2_mom_worked_model <- ivreg(mom_worked ~ morekids | samesex + twins_2, data=df)
iv2_mom_weeks_worked_model <- ivreg(mom_weeks_worked ~ morekids | samesex + twins_2, data=df)


# TSLS in 2 stages

first_stage_model <- lm(morekids ~ samesex + twins_2, data=df)
df$morekids_hat <- predict(first_stage_model)
# use estimates from first stage in second stage OLS:

ss_mom_worked_model <- lm(mom_worked ~ morekids_hat, data=df)
ss_mom_weeks_worked_model <- lm(mom_weeks_worked ~ morekids_hat, data=df)


# compare outcomes
# mom_worked: built-in IV vs TSLS
summary(iv2_mom_worked_model)
summary(ss_mom_worked_model)


# mom_weeks_worked: built-in IV vs TSLS
summary(iv2_mom_weeks_worked_model)
summary(ss_mom_weeks_worked_model)


