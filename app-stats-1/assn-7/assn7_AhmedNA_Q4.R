# Author: Ahmed N Amanullah
# St Num: 1001325773
# STA2101 Assignment 7 Ques 4

# first read data
d_source <- "http://www.utstat.toronto.edu/~brunner/data/legal/"
d_source <- paste(d_source, "openpigs.data.txt",sep="")
pigs_data <- read.delim(d_source, header = F, skip = 5, sep = "")

# get only the measurement data
pigs_data <- pigs_data[2:5]
# rename columns
# brd1 and brd2 are the 2 measurements for our explanatory variable
#	in our model: number of breeding sows on June 1
# bth1 and bth2 are the 2 measurements for our response variable
#	in our model: number of sows giving birth next summer 
colnames(pigs_data) <- c("brd1", "bth1", "brd2", "bth2") 

# verify number of cases
dim(pigs_data)

# ok... after much headache got data to a variable!

# part (a) need a correlation matrix of the observed variables
# compute and display said matrix
rho <- cor(pigs_data); rho

# part b - fitting model using lavaan
# first install - need to do this only once...
# install.packages("lavaan", dependencies = T)

# installed as of 06/11/2019!

# load lavaan
library(lavaan)

# use lavaan to fit our model
pigsmodel = 
	#############################################
	# Latent variable model
	# ---------------------
   'bth ~ beta*brd
	#
	# Measurement model
	# -----------------
	brd =~ 1*brd1 + 1*brd2
	bth =~ 1*bth1 + 1*bth2
	#
	# Variances and Covariances
	# -------------------------
	# Of latent explanatory variables
	brd ~~ phi*brd
	# of errors terms in latent regression
	bth ~~ psi*bth
	# of measurement errors for 1st set
	brd1 ~~ w1*brd1; brd1 ~~ w12*bth1
			bth1 ~~ w2*bth1
	# of measurement errors for 2nd set
	brd2 ~~ w3*brd2; brd2 ~~ w34*bth2
			bth2 ~~ w4*bth2
	'############ End of pigsmodel ##############

# fit model
fit1 = lavaan(pigsmodel, data = pigs_data)

# display summary
summary(fit1)

# display log likelihood for confirming match with prof
logLik(fit1)

# Got log likelihood of 'log Lik.' -1901.717 (df=9)
#	Matches with prof, good!

# parts (c) and (d) answerable from summary - no compt required!

# part (e) - numerical version of consistent estimate of beta from
#	3 (g)
# first need sample covariance matrix of observed data
sigma <- cov(pigs_data); sigma

# get numerical estimate of estimator from 3 (g) and display
beta_hat <- sigma[1,4]/sigma[1,3]; beta_hat

# estmate using other expression
beta_hat2 <- sigma[2,3]/sigma[1,3]; beta_hat2

# estimator based on using both beta structural equations
beta_hat3 <- (sigma[1,4]+sigma[2,3])/(2*sigma[1,3]); beta_hat3 

# part (f) - large scale conf. interval to answer from (d)
# print coef() and vcov() outputs incase it is necessary
coef(fit1)
vcov(fit1)
# first retrieve the estimate from fitted model using coef
beta_hat <- coef(fit1)[1]
# next retrieve standard error for beta using vcov()
beta_se <- sqrt(vcov(fit1)[1,1])
# compute large scale 95% CI
beta_lCL <- beta_hat - qnorm(0.975)*beta_se
beta_uCL <- beta_hat + qnorm(0.975)*beta_se
# display in neat table
results <- cbind(beta_hat, beta_lCL, beta_uCL)
colnames(results) <- c("beta_hat", "lowerCL", "upperCL")
results 

# for checking computed CI
parameterEstimates(fit1)

# part (g) can be retrieved from summary

# part (h) can also be retrieved from summary

# part (i) - testing (h) using Wald test
#	that is, we need to test w12 = 0 and w34 = 0 using Wald
# first borrow Wtest function written by Prof. Brunner
source("http://www.utstat.toronto.edu/~brunner/Rfunctions/Wtest.txt")

# next set up L matrix 
# first for w12 = 0
LL <- cbind(0, 0, 0, 0, 1, 0, 0, 0, 0)
LL <- rbind(LL,cbind(0, 0, 0, 0, 0, 0, 0, 1, 0))

# now apply wald test
Wtest(LL, coef(fit1), vcov(fit1))

# part (j) - testing equality of two measurement error covariance
#	matrices
# part (j) i. - test if they are equal
# setup L s.t. w1 = w3, w2 = w4 and w12 =  w34
LL <- cbind(0, 0, 0, 1, 0, 0, -1, 0, 0)
LL <- rbind(LL,cbind(0, 0, 0, 0, 1, 0, 0, -1, 0))
LL <- rbind(LL,cbind(0, 0, 0, 0, 0, 1, 0, 0, -1))

# now apply wald test
Wtest(LL, coef(fit1), vcov(fit1))

# part (j) ii. - testing which measurement is more accurate
# test 1: testing w1 = w3
LL <- cbind(0, 0, 0, 1, 0, 0, -1, 0, 0)

# now apply wald test
Wtest(LL, coef(fit1), vcov(fit1))

# test 1: testing w2 = w4
LL <- cbind(0, 0, 0, 0, 0, 1, 0, 0, -1)

# now apply wald test
Wtest(LL, coef(fit1), vcov(fit1))
