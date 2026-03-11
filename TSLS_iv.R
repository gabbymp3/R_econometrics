library(readr)
library(AER)

# load data
df <- read_csv("ang_ev_1980.csv")

# part 1: linear regression of outcome and treatment on samesex instrument

# regress morekids on samsex instrument (first stage)
morekids_model <- lm(morekids ~ samesex, data = df)
summary(morekids_model)

# regress mom_worked on samesex instrument
mom_worked_model <- lm(mom_worked ~ samesex, data=df)
summary(mom_worked_model)

# regress mom_weeks_worked on samesex instrument
mom_weeks_worked_model <- lm(mom_weeks_worked ~ samesex, data=df)
summary(mom_weeks_worked_model)


# part 2: IV regression of outcome on morekids using samesex instrument

#mom_worked outcome
iv_mom_worked_model <- ivreg(mom_worked ~ morekids | samesex, data=df)
summary(iv_mom_worked_model)

# mom_weeks_worked outcome
iv_mom_weeks_worked_model <- ivreg(mom_weeks_worked ~ morekids | samesex, data=df)
summary(iv_mom_weeks_worked_model)


# Both estimates using samesex as the IV for morekids returned values within 0.35 of the reported estimates from the paper.
# mom_worked much closer to reported value than mom_weeks_worked, but both are very similar.


# part 3: compute Wald estimates

# If we divide the beta_1 estimates of outcomes by the beta_1 estimate for treatment, we get the values in part b (approximately).
# i.e.,  beta_1 = (slope of the reduced form) / (slope of the first stage)

# -0.0076 / 0.059 ~ -0.1287
# -0.365 / 0.059 ~ -6.1864




# part 4: built-in IV regression with both instruments - samesex and twins_2

# mom_worked model
iv2_mom_worked_model <- ivreg(mom_worked ~ morekids | samesex + twins_2, data=df)
summary(iv2_mom_worked_model)

# mom_weeks_worked model
iv2_mom_weeks_worked_model <- ivreg(mom_weeks_worked ~ morekids | samesex + twins_2, data=df)
summary(iv2_mom_weeks_worked_model)

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


