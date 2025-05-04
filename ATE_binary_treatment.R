library(lmtest)
library(sandwich)
library(grf)
library(glmnet)
library(splines)
library(ggplot2)
library(reshape2)
library(RColorBrewer)

# discrete colorblind palette
cb_colors <- brewer.pal(n = 8, name = "Dark2")

# read data
data <- read.csv("https://docs.google.com/uc?id=1AQva5-vDlgBcM_Tv9yrO8yMYRfQJgqo_&export=download")
n <- nrow(data)


# treatment: does the the gov't spend too much on "welfare" (1) or "assistance to the poor" (0)
treatment <- "w"

# outcome: 1 for 'yes', 0 for 'no'
outcome <- "y"

# covariates
covariates <- c("age", "polviews", "income", "educ", "marital", "sex")

# valid in the randomized setting, not in observational 
Y <- data[,outcome]
A <- data[,treatment]
ate.est <- mean(Y[A==1]) - mean(Y[A==0])
ate.se <- sqrt(var(Y[A == 1]) / sum(A == 1) + var(Y[A == 0]) / sum(A == 0))
ate.tstat <- ate.est / ate.se
ate.pvalue <- 2*(pnorm(1 - abs(ate.est/ate.se)))
ate.results <- c(estimate=ate.est, std.error=ate.se, t.stat=ate.tstat, pvalue=ate.pvalue)
print('ATE Results:')
print(ate.results)

fmla <- formula(paste(outcome, '~', treatment))  # y ~ w
t.test(fmla, data=data)

# do not use! SE are not robust to heteroskedasticity;
fmla <- formula(paste0(outcome, '~', treatment))
ols <- lm(fmla, data=data)
coef(summary(ols))[2,]


# probabilistically dropping observations in a manner that depends on x

# copy old dataset
data.exp <- data

# defining the group that will be dropped with some high probability
grp <- ((data$w == 1) &  # if treated AND...
          (
            (data$age > 45) |           # belongs an older group OR
              (data$polviews < 5)       # more conservative
          )) | # OR
  ((data$w == 0) &                      # if untreated AND...
     (
       (data$age < 45) |           # belongs a younger group OR
         (data$polviews > 4)        # more liberal
     )) 

# individuals in the group above have a smaller chance of being kept in the sample
prob.keep <- ifelse(grp, .15, .85)
keep.idx <- as.logical(rbinom(n=nrow(data), prob=prob.keep, size = 1))

# drop
data <- data[keep.idx,]


W <- model.matrix(formula("~ 0 + age + polviews"), data.exp)  # old 'experimental' dataset
A <- data.exp$w
Y <- data.exp$y
par(mfrow=c(1,2))
for (a in c(0, 1)) {
  plot(W[A==a,1] + rnorm(n=sum(A==a), sd=.1), W[A==a,2] + rnorm(n=sum(A==a), sd=.1), 
       pch=ifelse(Y, 23, 21), cex=1, 
       col=ifelse(Y, adjustcolor(cb_colors[1],alpha.f=0.3), adjustcolor(cb_colors[3], alpha.f=0.3)),
       bg=ifelse(Y, adjustcolor(cb_colors[1],alpha.f=0.3), adjustcolor(cb_colors[3], alpha.f=0.3)),
       main=ifelse(a, "Treated", "Untreated"),
       xlab="age", ylab="polviews", las=1)
}


W <- model.matrix(formula("~ 0 + age + polviews"), data)
A <- data$w
Y <- data$y
par(mfrow=c(1,2))
for (a in c(0, 1)) {
  plot(W[A==a,1] + rnorm(n=sum(A==a), sd=.1), W[A==a,2] + rnorm(n=sum(A==a), sd=.1), 
       pch=ifelse(Y, 23, 21), cex=1, 
       col=ifelse(Y, adjustcolor(cb_colors[1],alpha.f=0.3), adjustcolor(cb_colors[3], alpha.f=0.3)),
       bg=ifelse(Y, adjustcolor(cb_colors[1],alpha.f=0.3), adjustcolor(cb_colors[3], alpha.f=0.3)), 
       main=ifelse(a, "Treated", "Untreated"),
       xlab="age", ylab="polviews", las=1)
}





# not for use in observational settings.
# only to show how the difference-in-means estimator is biased in that case!
fmla <- formula(paste0(outcome, '~', treatment))
ols <- lm(fmla, data=data)
coeftest(ols, vcov=vcovHC(ols, type='HC2'))[2,]




# fitting some model of E[Y|X,W]
fmla <- as.formula(paste0(outcome, "~ ", paste("bs(", covariates, ", df=3)", "*", treatment, collapse="+")))
model <- lm(fmla, data=data)  

# predict E[Y|X,W=w] for w in {0, 1}
data.1 <- data
data.1[,treatment] <- 1
data.0 <- data
data.0[,treatment] <- 0
muhat.treat <- predict(model, newdata=data.1)
muhat.ctrl <- predict(model, newdata=data.0)

# Averaging predictions and taking their difference
ate.est <- mean(muhat.treat) - mean(muhat.ctrl)
print('Estimated ATE:')
print(ate.est)


