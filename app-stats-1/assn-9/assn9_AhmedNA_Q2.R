# Author: Ahmed N Amanullah
# St Num: 1001325773
# STA2101 Assignment 9 Ques 2

# first read the data
# first read data
d_source <- "http://www.utstat.toronto.edu/~brunner/data/legal/"
d_source <- paste(d_source, "co-opManager.data.txt",sep="")
mdata <- read.table(d_source, header = T)

# see if data loaded alright
head(mdata)
summary(mdata)
dim(mdata)

# dim returns: 98  9
# so all cases are being read

# ok, now bigger fish to fry...
# try to fit model with lavaan
# load library
library(lavaan)

# model string
mmodel =
	#############################################
   '
   	# Latent variable model
	# ---------------------
   	perf ~ gamma1*know + gamma2*ploss + gamma3*educat + beta21*sat
   	sat ~ beta12*perf
	#
	# Measurement model
	# -----------------
	know =~ lambda1*know1 + lambda2*know2
	ploss =~ lambda3*ploss1 + lambda4*ploss2
	perf =~ lambda5*perf1 + lambda6*perf2
	sat =~ lambda7*sat1 + lambda8*sat2
	#
	# Variances and Covariances
	# -------------------------
	# Of latent explanatory variables
	know ~~ phix11*know; know ~~ phix12*ploss; know ~~ phix13*educat
			ploss ~~ phix22*ploss; ploss ~~ phix23*educat
					educat ~~ phix33*educat
	# of errors terms in latent regression
	perf ~~ psi1*perf
	sat ~~ psi2*sat
	# of measurement errors
	know1 ~~ omega1*know1 
	know2 ~~ omega2*know2
	ploss1 ~~ omega3*ploss1 
	ploss2 ~~ omega4*ploss2
	perf1 ~~ omega5*perf1 
	perf2 ~~ omega6*perf2
	sat1 ~~ omega7*sat1 
	sat2 ~~ omega8*sat2
	# Bounds (variances are postive)
	phix11 > 0; phix22 > 0; phix33 > 0
	psi1 > 0; psi2 > 0
	omega1 > 0; omega2 > 0; omega3 > 0; omega4 > 0
	omega5 > 0; omega6 > 0; omega7 > 0; omega8 > 0
	'############ End of mmodel ############## 
	
# model string ready, now to see if it fits...
# Moment of truth... the palpitations....
# fit model
fit1 = lavaan(mmodel, data = mdata)

# display summary
summary(fit1)

# display log likelihood
logLik(fit1)

# Model does not fit... issue with standard error
# Try a model with no factor loadings...
# model string
mmodel2 =
	#############################################
   '
   	# Latent variable model
	# ---------------------
   	perf ~ gamma1*know + gamma2*ploss + gamma3*educat + beta12*sat
   	sat ~ beta21*perf
	#
	# Measurement model
	# -----------------
	know =~ 1*know1 + 1*know2
	ploss =~ 1*ploss1 + 1*ploss2
	perf =~ 1*perf1 + 1*perf2
	sat =~ 1*sat1 + 1*sat2
	#
	# Variances and Covariances
	# -------------------------
	# Of latent explanatory variables
	know ~~ phix11*know; know ~~ phix12*ploss; know ~~ phix13*educat
			ploss ~~ phix22*ploss; ploss ~~ phix23*educat
					educat ~~ phix33*educat
	# of errors terms in latent regression
	perf ~~ psi1*perf
	sat ~~ psi2*sat
	# of measurement errors
	know1 ~~ omega1*know1 
	know2 ~~ omega2*know2
	ploss1 ~~ omega3*ploss1 
	ploss2 ~~ omega4*ploss2
	perf1 ~~ omega5*perf1 
	perf2 ~~ omega6*perf2
	sat1 ~~ omega7*sat1 
	sat2 ~~ omega8*sat2
	# Bounds (variances are postive)
	phix11 > 0; phix22 > 0; phix33 > 0
	psi1 > 0; psi2 > 0
	omega1 > 0; omega2 > 0; omega3 > 0; omega4 > 0
	omega5 > 0; omega6 > 0; omega7 > 0; omega8 > 0
	'############ End of mmodel ############## 

# Second try
# fit model
fit2 = lavaan(mmodel2, data = mdata)

# display summary
summary(fit2)

# display log likelihood
logLik(fit2)

# Ok this one fits, and the G^2 matches the prof's (29.357),
#	so keep this. p-value is 0.207

# Try one with half the loadings just for funsies
# model string
mmodel3 =
	#############################################
   '
   	# Latent variable model
	# ---------------------
   	perf ~ gamma1*know + gamma2*ploss + gamma3*educat + beta21*sat
   	sat ~ beta12*perf
	#
	# Measurement model
	# -----------------
	know =~ 1*know1 + lambda2*know2
	ploss =~ 1*ploss1 + lambda4*ploss2
	perf =~ 1*perf1 + lambda6*perf2
	sat =~ 1*sat1 + lambda8*sat2
	#
	# Variances and Covariances
	# -------------------------
	# Of latent explanatory variables
	know ~~ phix11*know; know ~~ phix12*ploss; know ~~ phix13*educat
			ploss ~~ phix22*ploss; ploss ~~ phix23*educat
					educat ~~ phix33*educat
	# of errors terms in latent regression
	perf ~~ psi1*perf
	sat ~~ psi2*sat
	# of measurement errors
	know1 ~~ omega1*know1 
	know2 ~~ omega2*know2
	ploss1 ~~ omega3*ploss1 
	ploss2 ~~ omega4*ploss2
	perf1 ~~ omega5*perf1 
	perf2 ~~ omega6*perf2
	sat1 ~~ omega7*sat1 
	sat2 ~~ omega8*sat2
	# Bounds (variances are postive)
	phix11 > 0; phix22 > 0; phix33 > 0
	psi1 > 0; psi2 > 0
	omega1 > 0; omega2 > 0; omega3 > 0; omega4 > 0
	omega5 > 0; omega6 > 0; omega7 > 0; omega8 > 0
	'############ End of mmodel ############## 

# Second try
# fit model
fit3 = lavaan(mmodel3, data = mdata)

# display summary
summary(fit3)

# display log likelihood
logLik(fit3)

# Interestingly this one seems to fit too but with different G^2 
#	than prof's (21.505); p-value is higher too (0.368)

# will continue analysis of question with fit2 as the G^2 for that
#	matches with prof's, so prof is porbably using that model...

# parts (a)-(d) answerable from output

# part (e) - Wald test for all regression coefficients
# first borrow Wtest function written by Prof. Brunner
source("http://www.utstat.toronto.edu/~brunner/Rfunctions/Wtest.txt")

# retrieve coefficients and asymptotic covariances of parameters
mcoef <- coef(fit2);mcoef
mvcov <- vcov(fit2);mvcov

# get number of parameters in model
nparam <- length(mcoef);nparam

# by inspecting mcoef, we see the regression coefficients occupy
#	the first 5 elements
# so L matrix will have 5 rows - one for each hypothesis
#	and the first 5 columns will be an identity matrix, with
#	the remaining 16 columns all 0
LL <- cbind(diag(5),matrix(rep(0, (nparam-5)*5),nrow=5)); LL

# now apply Wald test
Wtest(LL,  mcoef, mvcov)

# part (f): The reliability estimates and 95% CI of the knowledge
#	measures - can use lavaan for this:
mmodel2b =
	#############################################
   '
   	# Latent variable model
	# ---------------------
   	perf ~ gamma1*know + gamma2*ploss + gamma3*educat + beta12*sat
   	sat ~ beta21*perf
	#
	# Measurement model
	# -----------------
	know =~ 1*know1 + 1*know2
	ploss =~ 1*ploss1 + 1*ploss2
	perf =~ 1*perf1 + 1*perf2
	sat =~ 1*sat1 + 1*sat2
	#
	# Variances and Covariances
	# -------------------------
	# Of latent explanatory variables
	know ~~ phix11*know; know ~~ phix12*ploss; know ~~ phix13*educat
			ploss ~~ phix22*ploss; ploss ~~ phix23*educat
					educat ~~ phix33*educat
	# of errors terms in latent regression
	perf ~~ psi1*perf
	sat ~~ psi2*sat
	# of measurement errors
	know1 ~~ omega1*know1 
	know2 ~~ omega2*know2
	ploss1 ~~ omega3*ploss1 
	ploss2 ~~ omega4*ploss2
	perf1 ~~ omega5*perf1 
	perf2 ~~ omega6*perf2
	sat1 ~~ omega7*sat1 
	sat2 ~~ omega8*sat2
	# Bounds (variances are postive)
	phix11 > 0; phix22 > 0; phix33 > 0
	psi1 > 0; psi2 > 0
	omega1 > 0; omega2 > 0; omega3 > 0; omega4 > 0
	omega5 > 0; omega6 > 0; omega7 > 0; omega8 > 0
	# Parameter functions
	relk1 := phix11/(phix11+omega1)
	relk2 := phix11/(phix11+omega2)
	'############ End of mmodel ############## 

# fit model, display parameter estimates
fit2b = lavaan(mmodel2b, data = mdata)

# display summary
summary(fit2b)

# display log likelihood
logLik(fit2b)

# param ests
pEsts <- parameterEstimates(fit2b);pEsts 

# Just display output for the rels
pEsts[30:31,c(1,5,9,10)]
# the above gives the reliabilities ests and their CIs

# Try out delta method too
# first the reliability est
relk1 <- mcoef[6]/(mcoef[6] + mcoef[14]); relk1
# ok good so far

# delta method
n <- dim(mdata)[1]; n # sample size
# gdot
gdot <- mcoef[14]/((mcoef[6] + mcoef[14])^2)
gdot <- c(gdot, -mcoef[6]/((mcoef[6] + mcoef[14])^2))
gdot <- matrix(gdot, nrow=1); gdot
# asymptotic covariance
Sigma <- c(mvcov[6,6], mvcov[6,14], mvcov[6,14], mvcov[14,14])
Sigma <- matrix(Sigma, nrow = 2); Sigma 
# now get the asymptotic variance of relk1:
v_relk1 <- (1/n)*(gdot%*%Sigma%*%t(gdot)); v_relk1
se_relk1 <- sqrt(v_relk1); se_relk1

# delta method se doesn't match se from parameterEstimates 
#	(difference of factor of 10!)
# for now, will use answers from lavaan as it is the simple way
#	as the question asks + I maybe doing something wrong...

# part (g): Test Ho: omega1 = omega2 (i.e. reliabilities of 
#	knowledge measures being equal)
# Use Wtest()
# L matrix will have 1 and -1 in positions of omega1 and omega2
# These are columns 14 and 15 in the coef() output...
LL<- matrix(rep(0, 13),nrow=1)
LL <- cbind(LL,1,-1,matrix(rep(0, nparam-15),nrow=1)); LL
# apply Wtest:
Wtest(LL,  mcoef, mvcov)

# part (h) - Work done on paper; no R needed
# correlation compt assuming measurement not equal
cov_d1d2 <- mcoef[6]
vard1_vard2 <- (sqrt((mcoef[6]+mcoef[14])*(mcoef[6]+mcoef[15])))
corr_d1d2 <- cov_d1d2/vard1_vard2;corr_d1d2 


# part (i) - generate correlation matrix of observable data
mcor <- cor(mdata); mcor