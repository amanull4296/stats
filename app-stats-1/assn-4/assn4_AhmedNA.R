# Author: Ahmed N Amanullah
# St Num: 1001325773
# STA2101 Assignment 4

# Question 1
# set working directory to the folder containing the data
wdir <- "/Users/amanull9/Documents/UofT MSc in Stats/Student/"
wdir <- paste(wdir, "Fall/STA2101 (Applied Stats I)/", sep = "")
wdir <- paste(wdir, "Assignments/Assn4", sep = "")
setwd(wdir)
# read data in
uniapp_data <- read.csv(file="uniapplicants.csv",header=T, sep=",")

# part (a)
# Ok, now fit a linear reg model with GPA as response and Math as
#	as explanatory
lm1 <- lm(GPA ~ MATH, data = uniapp_data)
# get summary
summary(lm1) 
# predict the GPA forecasted for Math score of 700
gpa_hat <- predict(lm1, data.frame(MATH=700))
# print forecast
sprintf("Predicted GPA for SAT Math score of 700: %.2f", gpa_hat)

# part (b) (i)
# Fit full model (with both SAT scores)
lm2 <- lm(GPA ~ VERBAL+MATH, data = uniapp_data)
# get summary
summary(lm2)

# part (b) (ii)
# Control for Math, is Verbal score related to GPA?
# reduced model and full model already fit - use anova 
anova(lm1, lm2)

# part (b) (iii)
# Allow for Verbal, is Math related to GPA?
# fit reduced model with just verbal (control group)
lm3 <- lm(GPA ~ VERBAL, data = uniapp_data)
# use anova to do the F test
anova(lm3, lm2)

# part (b) (iv)
# predict GPA for Verbal and Math scores of 650 and 700 respectively
gpa_hat <- predict(lm2, data.frame(VERBAL=650,MATH=700))
# print result
sprintf("Predicted GPA for Verbal 650 and Math 700: %.2f", gpa_hat)

# part (b) (v)
# retrieve ftest function written by Prof Brunner
source("http://www.utstat.utoronto.ca/~brunner/Rfunctions/ftest.txt")
# run ftest
ftest(lm2, rbind(c(0, 1, -1)))

# Question 2
# first read the data 
d_source <- "http://www.utstat.toronto.edu/~brunner/data/legal/"
d_source <- paste(d_source, "mystery.data.txt",sep="")

'''
# (is there a easier way to do this?) Long winded 1st attempt
# read as strings
mystery_data <- readChar(d_source,10000)
# separate into lines
m_lines <- strsplit(mystery_data, split="\n")
# use strsplit to separate records
m_data <- as.vector(lapply(m_lines, strsplit, split = "  | "))
# use a loop
m_data2 <- numeric(0)
for (i in c(1:length(m_data))){
	for (j in c(1:length(m_data[[i]]))){
		for (k in c(1:length(m_data[[i]][[j]]))){
			m_data2 <- c(m_data2, as.numeric(m_data[[i]][[j]][k]))
		}
	}
} 
'''

# Quick load method
m_data <- scan(d_source)

# part (a): For density function provided find MLE
# function to compute negative log likelihood
mdll <- function(theta, datta){
	# parameters
	mu <- theta[1]
	theta2 <- theta[2]
	# length of data
	n <- length(datta)
	# intermediate term in log-likelihood expression
	intermediate <- log(1+exp(theta2*(datta-mu)))
	# negative log likelihood (written in 2 lines)
	mdll <- n*(theta2*mu-log(theta2)-theta2*mean(datta))
	mdll <- mdll + 2*sum(intermediate)
	mdll
}

# initial estimates (1st try)
mu <- 0
theta2 <- 1

# mom + arbitrary initial ests
mu <- mean(m_data)
theta2 <- 1

# use nlm (non-linear minimization) of R
theta_search <- nlm(mdll, c(mu, theta2), hessian=T, datta=m_data)

# print output
theta_search

# check eigenvalues of of hessian of search
# If both eigenvalues positive, we have positive definiteness
# which implies a local minimum of negative ll as required
eigen(theta_search$hessian)$values

# plot theta and y's (slices across optimal value)
# mu
mu_dom <- seq(from=0.1, to=10, by=0.1)
y <- mu_dom - mu_dom
for(i in 1:length(mu_dom)) {
	y[i] <- mdll(c(mu_dom[i], 0.8760225),m_data)	
}
plot(mu_dom, y, type='l')

# theta
t_dom <- seq(from=0.1, to=10, by=0.1)
y <- t_dom - t_dom
for(i in 1:length(t_dom)) {
	y[i] <- mdll(c(3.3392024, t_dom[i]),m_data)	
}
plot(t_dom, y, type='l')

# part (b): 95% confidence interval for theta
# first invert hessian
ts_hess <- theta_search$hessian
# asymptotic covariance matrix
v_hat <- solve(ts_hess)
# since theta is the 2nd element, we need the sqrt of the 2nd
# diagonal element
theta_se <- sqrt(v_hat[2,2])
# get confidence interval bounds
theta_hat <- theta_search$estimate[2]
theta_clb <- theta_hat - qnorm(0.975)*theta_se
theta_cub <- theta_hat + qnorm(0.975)*theta_se
# print output
sprintf("theta 0.95 CI: (%.3f, %.3f)", theta_clb, theta_cub)

# part (c): test mu = 0 at 0.05 significance
# 1st diagonal element of inverted hessian will give mu's se
mu_se <- sqrt(v_hat[1,1])
# compute test statistic
z_t <- theta_search$estimate[1]/mu_se
# get critical value
z_c <- qnorm(0.975)
# see if hypothesis is rejected
# display computation and test results
reject <- 'no'
if(abs(z_t)>z_c){
	reject <- 'yes'
}
# print the test stat and the critical value
sprintf('z_t: %.2f, z_c: %.2f', z_t, z_c)
sprintf('Reject?: %s', reject)

# Question 5: Large sample likelihood ratio test for multi-normal
# Load the data:
d_source <- "http://www.utstat.toronto.edu/~brunner/data/illegal/"
d_source <- paste(d_source, "bp.data.txt", sep = "")
bp_data <- read.table(d_source, header = T)
# ok first need a function for computing the negative ll
mnll <- function(parameters, datta){
	# number of random variables making up the normal
	# number of elements in parameters in p + p^2
	# number of parameters 
	nop <- length(parameters)
	p <- (-1 + sqrt(1 + 4*nop))/2
	# number of observations
	n <- length(datta)/p
	Data <- matrix(datta, n, p)
	# unpack into Mu and Sigma
	# first p elements is Mu
	Mu <- matrix(parameters[1:p], ncol = 1)
	Sigma <- matrix(parameters[(p+1):nop], p,p,byrow = T)
	# the first intermediate term related to determinant of Sigma
	inter1 <- (-n/2)*log(det(Sigma))
	# second one related to the pi one
	inter2 <- (-n*p/2)*log(2*pi)
	# the third one related to the exponential
	# sigma hat
	y_bar <- matrix(colMeans(Data), ncol = 1)
	Sigma_hat <- matrix(0, p, p)
	for (i in c(1:n)){
		ymybar <- matrix(Data[i, ], ncol = 1) - y_bar
		Sigma_hat <- Sigma_hat + (1/n)*(ymybar)%*%t(ymybar)
	} 
	Sigma_inv <- solve(Sigma)
	inter3_1 <- sum(diag(Sigma_hat%*%Sigma_inv))
	inter3_2 <- t(y_bar - Mu)%*%Sigma_inv%*%(y_bar-Mu)
	# exponential term
	inter3 <- (-n/2)*(inter3_1 + inter3_2)
	#return negative ll
	mnll <- -(inter1+inter2+inter3)
	mnll
}

# Ok, now to use this in 2 numerical searches
# first the unrestrained mles
# need good initial estimates
# Idea: Try MoM
# data in matrix form
X <- data.matrix(bp_data)
n <- dim(X)[1]
p <- dim(X)[2]
Mu <- matrix(colMeans(X), ncol = 1)
Sigma_hat <- matrix(0, p, p)
for (i in c(1:n)){
	ymybar <- matrix(X[i, ], ncol = 1) - Mu
	Sigma_hat <- Sigma_hat + (1/n)*(ymybar)%*%t(ymybar)
} 

# wrap the Mu and Sigma into a vector
parameters <- as.vector(Mu)
parameters <- c(parameters, as.vector(t(Sigma_hat)))

# now to try the nlm...
mnparam_search <- nlm(mnll, parameters, hessian=T, datta=as.vector(X))
mnparam_search

# Ok got this to output, but won't be needing this...
# will work with question 4's result
# idea is to isolate out the subset of variables we are interested in
# 	and then apply the likelihood ratio test stat expression derived
#	in Q4
# first extract out the variables data we are interested in
# filter out very outlying values (observed using summary() and 
#	boxplot()) - ask if this is what is meant by missing data...
# remove filter for bp as we only care about variables we're testing
#bp_data_nm <- bp_data[bp_data$bp != 999,]
bp_data_nm <- bp_data[bp_data$edu != 99,]
bp_data_nm <- bp_data_nm[bp_data_nm$cig != 999,]

#X <- cbind(bp_data$edu, bp_data$cig, bp_data$wt)
X <- cbind(bp_data_nm$edu, bp_data_nm$cig, bp_data_nm$wt)

# next calculate the sample variance-covariance matrix
n <- dim(X)[1]
Sigma_hat <- var(X)*((n-1)/n)
#now the likelihood ratio
G2 <- n*(sum(log(diag(Sigma_hat))) - log(det(Sigma_hat)))
# df = number of non-redundant linear combinations
# s_12 = 0; s_13 = 0; s_23 = 0
# df = 3
# get p-value
pval <- 1-pchisq(G2, df = 3)

# print the test stat and the p-val
sprintf("G2: %.5f; pval: %.5f", G2, pval)
# see if we reject
# significance level
alpha <- 0.05
reject <- 'no'
if(pval<alpha){
	reject <- 'yes'
}
sprintf('Reject?: %s', reject)

# Question 8: XY data: looking at SLR parameter estimation provided
# 	X and Y are random
# first retrieve the data
d_source <- "http://www.utstat.toronto.edu/~brunner/data/legal/"
d_source <- paste(d_source, "xy.data.txt", sep = "")
xy_data <- read.table(d_source, header = T)

# part (a)
# fit regression line through origin
lm1 <- lm(y ~ x-1, data = xy_data)

# get summary
summary(lm1)

# compute confidence interval
# get estimate
beta1 <- summary(lm1)$coefficients[1]
# get standard error
beta1_se <- summary(lm1)$coefficients[2]
# compute confidence interval bounds and output
# inferential investigation of regression parameters derived from
#	normality assumption
# so will use t-distribution
# length of data
n <- dim(xy_data)[1]
beta1_clb <- beta1 - qt(0.975, df=n-1)*beta1_se
beta1_cub <- beta1 + qt(0.975, df=n-1)*beta1_se
# print output
sprintf("beta1 0.95 CI: (%.3f, %.3f)", beta1_clb, beta1_cub)

# part (b)
# first get estimate for beta2
beta2 <- sum(xy_data$y)/sum(xy_data$x)
# next need to compute derived asymptotic se
# length of data
n <- dim(xy_data)[1]

# error terms
epsilon <- xy_data$y - beta2*xy_data$x
# sample variance of error 
epsilon_se <- var(epsilzon)
# sample mean of x
x_bar <- mean(xy_data$x)
# sample variance of x
#x_se <- var(xy_data$x)
# now compute the asymptotic sample variance
beta2_var <- epsilon_se/(n*(x_bar^2))
#beta2_var <- beta2_var - ((2*(n-1)*(beta2^2)*x_se)/(n*(x_bar^2)))
# ....variance is negative....
# 95% interval
# compute confidence interval bounds and output
beta2_clb <- beta2 - qnorm(0.975)*sqrt(beta2_var)
beta2_cub <- beta2 + qnorm(0.975)*sqrt(beta2_var)
# print output
sprintf("beta2 0.95 CI: (%.3f, %.3f)", beta2_clb, beta2_cub)

'''
# mean of y
y_bar <- mean(xy_data$y)
# mean x
x_bar <- mean(xy_data$x)
# sample variances
xy_vars <- var(xy_data)
x_var <- xy_vars[1,1]
y_var <- xy_vars[2,2]
xy_cov <- xy_vars[1,2]
# asymptotic var
beta2_var <- ((y_bar^2)/(x_bar^4))*x_var + y_var/(x_bar^2)
beta2_var <- beta2_var - (2*n*xy_cov*y_bar)/(x_bar^3)
beta2_var <- beta2_var/n
# ...still negative... (something else is wrong)
'''