library(readr)
library(AER)
df <- read_csv("/Users/gabriellamessenger/Desktop/Math386-2/Math386/Psets/ang_ev_1980.csv")

# 1 - For the each of the two outcomes, explain which regression you ran in your
# previous problem set can be interpreted as LATE. Run these regressions
# again and report their numbers.

# twins instrument: exogeneity does not hold
# same-sex instrument: exogeneity holds, monotonicity implies no defiers, i.e.,
                      # no couples with preference: A(1) = 0 and A(0) = 1
# although monotonicity is more difficult to defend, the same-sex instrument can 
# be argued to satisfy both exogeneity and monotonicity, so the TSLS regression 
# using same-sex as the IV will equal the LATE

iv_mom_worked_model <- ivreg(mom_worked ~ 
                               morekids | samesex, data=df)

iv_mom_weeks_worked_model <- ivreg(mom_weeks_worked ~ 
                                     morekids | samesex, data=df)


# mom_worked
summary(iv_mom_worked_model)

# mom_weeks_worked
summary(iv_mom_weeks_worked_model)


# 2 Compute the default standard errors associated with each of these LATE 
# estimates that are reported in R.

# LATE morekids on mom_worked: default std. error = 0.0261
# LATE morekids on mom_weeks_worked: default std. error = 1.1713


# 3 Test the hypothesis that each of the LATE parameters is zero and then
# test the hypothesis that they are equal to the ones that were reported in
# the original paper.

# a) H_0: B_j = 0
t_stat_mom_worked = -0.1286 / (0.0261)
t_stat_mom_weeks_worked = -6.1372 / 1.1713

# reject if |t_stat| > 1.96 (testing at 95% confidence level)
print(t_stat_mom_worked)
print(t_stat_mom_weeks_worked)

# H_0 is rejected

# b) H_0: B_j = reported estimate from paper
t_stat_mom_worked = (-0.1286 + 0.133) / (0.0261)
t_stat_mom_weeks_worked = (-6.1372 + 6.38) / 1.1713

# reject if |t_stat| > 1.96 (testing at 95% confidence level)
print(t_stat_mom_worked)
print(t_stat_mom_weeks_worked)

# Fail to reject H_0.


# 4 Re-do (c) this time using robust standard errors.

coeftest(iv_mom_worked_model,vcov = vcovHC(iv_mom_worked_model, 
                                           type = "HC0", df = Inf))

coeftest(iv_mom_weeks_worked_model,vcov = vcovHC(iv_mom_weeks_worked_model, 
                                                 type = "HC0", df = Inf))

# a) H_0: B_j = 0

t_stat_mom_worked = (-0.128617) / 0.026099
t_stat_mom_weeks_worked = (-6.13721) / 1.17140
# reject if |t_stat| > 1.96
print(t_stat_mom_worked)
print(t_stat_mom_weeks_worked)

# H_0 is rejected.

# b) H_0: B_j = reported estimate from paper

t_stat_mom_worked = (-0.128617 + 0.133) / 0.026099
t_stat_mom_weeks_worked = (-6.13721 + 6.38) / 1.17140
# reject if |t_stat| > 1.96
print(t_stat_mom_worked)
print(t_stat_mom_weeks_worked)

# Fail to reject H_0.