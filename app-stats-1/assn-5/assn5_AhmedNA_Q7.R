# Author: Ahmed N Amanullah
# St Num: 1001325773
# STA2101 Assignment 5 Ques 7

# first read data
d_source <- "http://www.utstat.toronto.edu/~brunner/data/illegal/"
d_source <- paste(d_source, "birdlung.data.txt",sep="")
bl_data <- read.table(d_source)

# rename variables
cnames <- c("lc", "sex", "ses", "bk", "age", "yrs_s", "cig")
colnames(bl_data) <- cnames

# proportion of data investigation
# make tables for each binary variable 
# Lung cancer
lc_freq <- table(bl_data$lc)
lc_prop <- prop.table(lc_freq)
# report proportion
sprintf("Lung cancer, No?: %.3f, Yes?: %.3f", lc_prop[1],lc_prop[2])

# Sex
sex_freq <- table(bl_data$sex)
sex_prop <- prop.table(sex_freq)
# report proportion
sprintf("Sex, Male: %.3f, Female: %.3f", sex_prop[1],sex_prop[2])

# Socioeconomic Status
ses_freq <- table(bl_data$ses)
ses_prop <- prop.table(ses_freq)
# report proportion
sprintf("SE Status, Low: %.3f, High: %.3f", ses_prop[1],ses_prop[2])

# Birdkeeping
bk_freq <- table(bl_data$bk)
bk_prop <- prop.table(bk_freq)
# report proportion
sprintf("Birdkeep, No?: %.3f, Yes?: %.3f", bk_prop[1],bk_prop[2])

# for part (b) want to compute G2 for null hypothesis test 
#	 (i.e all else considered, is bk related to chance of lc)
# fit logistic regression model
# fit full model
log_m1 <- glm(lc ~ sex+ses+bk+age+yrs_s+cig,data = bl_data,
family = binomial)
# check summary
summary(log_m1)

# fit reduced model (with just to control variables)
log_m2 <- glm(lc ~ sex+ses+age+yrs_s+cig,data = bl_data,family = binomial)
# check summary
summary(log_m2)

# G2
G2 <- log_m2$deviance - log_m1$deviance

# Report G2
sprintf("G2: %.3f", G2)

# for part (d) need to compute and report p-value
# degrees of freedom is 1 (only one equal sign in null hypothesis)
dof <- 1
p_val <- 1 - pchisq(G2, df = dof)
# Report p-value
sprintf("p-value: %.5f", p_val)

# what happens when anova is used?
anova(log_m2, log_m1, test = "Chisq")

# part e
# see if we reject
# significance level
alpha <- 0.05
reject <- 'no'
if(p_val<alpha){
	reject <- 'yes'
}
sprintf('Reject?: %s', reject)

# report exponential of bk estimated coefficient
odds_r <- exp(log_m1$coefficients["bk"])
sprintf("odds_r: %.3f", odds_r)

# part (f) requires the estimation of lung cancer probability
# for non-smoking (yrs_s=cig=0) women (sex=1) of avg age (age = mean(age))
#	and low se status (ses = 0) who birdkeeps (bk = 1)
# use predict to obtain estimate probability
age_bar <- mean(bl_data$age)
input_f <- data.frame(sex=1,ses=0,bk=1,age=age_bar,yrs_s=0,cig=0)
pre_f <- predict(log_m1,newdata=input_f,type="response", se.fit=T);pre_f

# compare with manual process
x <- c(1, 1, 0, 1, age_bar, 0, 0)
xb <- sum(log_m1$coefficients*x) 
phat <- exp(xb)/(1+exp(xb))
# report
sprintf("(f) estimated prob: %.3f", phat)

# part (g): need confidence interval for answer in (g)
# first try out delta method se computation just for comparison
# covariance matrix
Vhat <- vcov(log_m1)
# delta method
denom <- (1+exp(xb))^2
gdot <- x*exp(xb)/denom
gdot <- matrix(gdot, nrow = 1)
se_f_manual <- sqrt(gdot%*%Vhat%*%t(gdot))
# report
sprintf("(f) estimated se: %.4f", se_f_manual)

# Obtain and report CI
# bounds
lowerCL <- pre_f$fit - qnorm(0.975)*pre_f$se.fit
upperCL <- pre_f$fit + qnorm(0.975)*pre_f$se.fit
# now prediction
phat <- pre_f$fit
results <- cbind(phat, lowerCL, upperCL)
colnames(results) <- c("phat", "LowerCL", "UpperCL")
results

# part (h) - compute the 95% CI using different approach
#	this time we first obtain CI for log odds
# use predict without response this time...
# plan to use predict to get s.e as well
# note that alternatively could have gotten s.e from estimated
#	covariance matrix as sqrt(x*Vhat*t(x)) but since results are same
#	omitting for sake of brevity
pre_h <- predict(log_m1,newdata=input_f, se.fit=T);pre_h

# Obtain and report CI for log odds
# bounds
lowerCL <- pre_h$fit - qnorm(0.975)*pre_h$se.fit
upperCL <- pre_h$fit + qnorm(0.975)*pre_h$se.fit
# now prediction
log_odds <- pre_h$fit
results <- cbind(log_odds, lowerCL, upperCL)
colnames(results) <- c("log_odds", "LowerCL", "UpperCL")
results

# next want to use the fact that p(x) = exp(x)/(1+exp(x)) is increasing
#	to obtain the 95% CI for prob
# bounds
lowerCL <- exp(lowerCL)/(1+exp(lowerCL))
upperCL <- exp(upperCL)/(1+exp(upperCL))
# now prediction
phat <- exp(log_odds)/(1+exp(log_odds))
results <- cbind(phat, lowerCL, upperCL)
colnames(results) <- c("phat", "LowerCL", "UpperCL")
results 

# part (j) LR test equivalent
# fit model for control variables
# fit reduced model (with just to control variables)
log_m3 <- glm(lc ~ ses+bk+age+yrs_s+cig,data = bl_data,family = binomial)
# check summary
summary(log_m3)

# G2
G2 <- log_m3$deviance - log_m1$deviance

# Report G2
sprintf("G2: %.3f", G2)

# degrees of freedom is 1 (only one equal sign in null hypothesis)
dof <- 1
p_val <- 1 - pchisq(G2, df = dof)
# Report p-value
sprintf("p-value: %.5f", p_val)

# what happens when anova is used?
anova(log_m3, log_m1, test = "Chisq")

# part k
# Using LR test to see if any variable is related to chance of lung cancer
# use null.deviance item of full model to get the negative LL of the 
#	reduced model with just the intercept term
# LR test stat
G2 <- log_m1$null.deviance - log_m1$deviance
# p-value; degrees of freedom is 6 as there are 6 explanatory variables
#	whose coefficients are 0 under null hypothesis
pval <- 1-pchisq(G2, df = 6)
# report results
sprintf("G2: %.3f", G2)
sprintf("p-value: %.5f", pval)

# part l - repeat k, but using the Wald test
# Borrow Prof. Brunner's Wtest function
source("http://www.utstat.utoronto.ca/~brunner/Rfunctions/Wtest.txt")
# Obtain full model coefficients
beta_hat1 <- log_m1$coefficients
# We will have 6 rows - 1 each for the 6 explanatory variables
# As under null hypothesis the regression coefficients for those are 0
L1 <- cbind(c(0,0,0,0,0,0), diag(6))
# covariance matrix
Vhat <- vcov(log_m1)
# Carry out Wald test
Wtest(L1, beta_hat1, Vhat)

# part m
# fit logistic regression model with just cig as explanatory variable
log_m4 <- glm(lc ~ cig,data = bl_data,family = binomial)
# check summary
summary(log_m4)

# obtain odds ratio for part (i)
beta_1 <- log_m4$coefficients["cig"]
# odds ratio
r <- exp(10*beta_1)
# report
sprintf("Odds multiplied by r: %.5f", r)

# part (ii) - obtain asymptotic vairance of r from (i)
# use vcov to get Vhat from model
Vhat <- vcov(log_m4)
# gdot for r
gdot <- c(0, 10*exp(10*beta_1))
gdot <- matrix(gdot, nrow = 1)
# get aymptotic variance by delta method
r_var <- gdot %*% Vhat %*% t(gdot)
# report asymptotic variance
sprintf("Asymptotic variance of r: %.5f", r_var)