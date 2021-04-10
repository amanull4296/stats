# Author: Ahmed N Amanullah
# St Num: 1001325773
# STA2101 Assignment 3

# Question 1 computation
# 1 (b)
# test stat
z_n <- 2*sqrt(100)*(asin(sqrt(0.6))-asin(sqrt(0.5)))
# get the critical value
# significance level
alpha <- 0.05
# critical value
z <- -qnorm(alpha/2)
reject <- 'no'
better <- 'no'
if(abs(z_n)>z){
	reject <- 'yes'
	if(z_n>0){
		better <- 'yes'
	}
}
# print the test stat and the critical value
sprintf('Z_n: %.2f, Z_crit: %.2f', z_n, z)
sprintf('Reject?: %s, Better?: %s', reject, better)

# 1(d)
# provided sample mean and size
x_bar <- 60/100
n <- 100
# sample standard deviation
y_sd <- 1/sqrt(n*x_bar*(1-x_bar))
# critical value
alpha <- 0.05
z <- -qnorm(alpha/2)
# confidence interval bounds
y_bar <- log(x_bar/(1-x_bar))
lb <- y_bar - z*y_sd
ub <- y_bar + z*y_sd
# print bounds
sprintf('0.95 conf int: (%.2f, %.2f)', lb, ub)

# 2 (f)
# Provided sample mean
X_bar <- 9.2
# sample size
n <- 30
# null hypothesis
lambda_o <- 8
# test statistic
z_t <- sqrt(n)*(2*sqrt(X_bar)-2*sqrt(lambda_o))
# critical value
alpha <- 0.05
z_c <- -qnorm(alpha) # note that this is 1-sided
# display computation and test results
reject <- 'no'
if(z_t>z_c){
	reject <- 'yes'
}
# print the test stat and the critical value
sprintf('z_t: %.2f, z_c: %.2f', z_t, z_c)
sprintf('Reject?: %s', reject)