# Author: Ahmed N Amanullah
# St #: 1001325773
# STA2101 Assignment 1

# Question 4 computation
# Observed data
X <- c(1.37, 2.89, 1.52, 1.77, 1.04, 2.71, 1.19, 1.13, 15.66, 1.43)
# number of obs
n <- length(X)
# natural log of obs
lnX <- log(X)
# compute MLE
theta_hat <- n/sum(lnX)
# display MLE
sprintf('mle estimate, theta_hat: %.6f', theta_hat)

# Question 6 b computation
# Sample mean
Y_bar <- 2.57
# number of obs
n <- 23
# sample standard dev
s <- sqrt(5.85)
# critical value
t <- 2.07
# compute lower and upper bounds as derived in (a)
lower <- Y_bar - t*(s/sqrt(n))
upper <- Y_bar + t*(s/sqrt(n))
# display bounds
sprintf('conf interval: %.2f < mu < %.2f', lower, upper) 

# Question 6 c i computation
# Sample mean
Y_bar <- 2.57
# number of obs
n <- 23
# sample standard dev
s <- sqrt(5.85)
# null hypothesis mean
mu <- 3
# test statistic computation
t <- (sqrt(n)*(Y_bar - mu))/s
#display computation
sprintf('T: %.2f', t)

# Question 8 c computation
# null hypothesis theta
theta_o <- 1/2
# size of test is 0.05; critical value corresponding to this
alpha <- 0.05
z_c <- -qnorm(alpha/2)
# sample size
n <- 100
# true proportion
theta <- 0.55
# compute power
# intermediate terms
x1 <- sqrt(n)*((theta_o-theta)/sqrt(theta*(1-theta)))
x2 <- z_c*sqrt((theta_o*(1-theta_o))/(theta*(1-theta)))
# power
power <- 1-pnorm(x1+x2)+pnorm(x1-x2)
# display computation
sprintf('Power: %.2f', power)

# Question 8 d computation
# null hypothesis theta
theta_o <- 1/2
# size of test is 0.05; critical value corresponding to this
alpha <- 0.05
z_c <- -qnorm(alpha/2)
# true proportion
theta <- 0.55
# beta (power)
beta <- 0.80
# get least sample size - need to implement Newton's method for this
# least sample size Newton's method
# initial estimate (prof said between 700 and 800 so choose)
n <- 750
# intermediate terms
x1 <- sqrt(n)*((theta_o-theta)/sqrt(theta*(1-theta)))
x2 <- z_c*sqrt((theta_o*(1-theta_o))/(theta*(1-theta)))
x3 <- (theta_o-theta)/(2*sqrt(n*theta*(1-theta)))
# next estimate
f_n <- 1-beta-pnorm(x1+x2)+pnorm(x1-x2)
fp_n <- dnorm(x1-x2)*x3-dnorm(x1+x2)*x3
n_ip1 <- n - (f_n/fp_n)
# use while loop with accepted tolerance on when to stop
tol <- 0.1
while((n_ip1-n)>tol){
	n <- n_ip1
	# intermediate terms
	x1 <- sqrt(n)*((theta_o-theta)/sqrt(theta*(1-theta)))
	x2 <- z_c*sqrt((theta_o*(1-theta_o))/(theta*(1-theta)))
	x3 <- (theta_o-theta)/(2*sqrt(n*theta*(1-theta)))
	# next estimate
	f_n <- 1-beta-pnorm(x1+x2)+pnorm(x1-x2)
	fp_n <- dnorm(x1-x2)*x3-dnorm(x1+x2)*x3
	n_ip1 <- n - (f_n/fp_n)
}
# get accepted tolerance estimate
n <- n_ip1
# print estimated least sample size giving at least required power
# display computation
sprintf('Least sample size: %.0f', n)

# Question 9 h computation
# data input
x <- c(8,7,7,9,4)
y <- c(9,13,9,8,6)

# b0
b0 <- mean(y)
# b1
# Sxy
Sxy <- sum((y-mean(y))*(x-mean(x)))
# Sxx
Sxx <- sum((x-mean(x))^2)
# b1
b1 <- Sxy/Sxx
# print computation
sprintf('b0: %.2f , b1: %.2f', b0, b1)