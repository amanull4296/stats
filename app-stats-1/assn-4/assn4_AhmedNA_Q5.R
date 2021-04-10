# Author: Ahmed N Amanullah
# St Num: 1001325773
# STA2101 Assignment 4 Ques 5

# Load the data:
d_source <- "http://www.utstat.toronto.edu/~brunner/data/illegal/"
d_source <- paste(d_source, "bp.data.txt", sep = "")
bp_data <- read.table(d_source, header = T)

# To test for diagonal covariance matrix for variables
#	education, number of cigs/day, weight - so extract those data 
#	first
X <- data.frame(bp_data$edu, bp_data$cig, bp_data$wt)
colnames(X) <- c("edu", "cig", "wt")
# use summary on this subset
summary(X)
par(mfrow=c(2,2))
boxplot(X$edu)
boxplot(X$cig)
boxplot(X$wt)

# extract observations with cig values of 999 and edu values of 99
# 	as they look like coded in numbers representing missing data
#	(seems unrealistic compared to other observations)
X_nm <- X[X$edu != 99,]
X_nm <- X_nm[X_nm$cig != 999,]

# check summary and number of observations before/after filtering
summary(X_nm)
n_old <- dim(X)[1]
n <- dim(X_nm)[1]
# display number of observations
sprintf("Observations, before filter: %d, after filter: %d", n_old, n)

# carry out large sample likelihood ratio test
# n retrieved earlier, get MLE covariance matrix
# i.e. need to adjust to divide by n
Sigma_hat <- var(X_nm)*((n-1)/n)
# compute derived diagonal covariance matrix null hypothesis test
G2 <- n*(sum(log(diag(Sigma_hat))) - log(det(Sigma_hat)))
# degrees of freedom
p <- dim(X_nm)[2]
dof <-  (1/2)*p*(p-1)
# get p-value
pval <- 1 - pchisq(G2, df = dof)
# print test-stat, p-value and test result
sprintf("G2: %.5f; pval: %.5f", G2, pval)
# see if we reject
# significance level
alpha <- 0.05
reject <- 'no'
if(pval<alpha){
	reject <- 'yes'
}
sprintf('Reject?: %s', reject)