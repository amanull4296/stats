# Author: Ahmed N Amanullah
# St Num: 1001325773
# STA2101 Assignment 4 Ques 2

#first read data
d_source <- "http://www.utstat.toronto.edu/~brunner/data/legal/"
d_source <- paste(d_source, "mystery.data.txt",sep="")
m_data <- scan(d_source)

# part (a) - obtaining the MLE estimates
# compute mystery data minus log likelihood
mdmll <- function(Theta, datta){
	# mu
	mu <- Theta[1]
	# theta
	theta <- Theta[2]
	# number of observations
	n <- length(datta)
	# mean of data
	x_bar <- mean(datta)
	# intermediate term 
	intermediate <- 2*sum(log(1 + exp(theta*(datta-mu))))
	# minus ll
	mdmll <- -n*(log(theta)+theta*(x_bar - mu)) + intermediate
	mdmll
}

# initial estimates for mu and theta
m_0 <- 0
t_0 <- 1
# implement nlm
Theta_search <- nlm(mdmll, c(m_0,t_0), hessian = T, datta = m_data)
# display output
Theta_search 
# reveal eigenvalues for the hessian to assess positive definiteness
eigen(Theta_search$hessian)$values

# part (b)
# Obtain approximate 95% confidence interval for theta
# retrieve MLE estimates
mu <- Theta_search$estimate[1]
theta <- Theta_search$estimate[2] 
# next invert the hessian to get the aymptotic covariance matrix
V_hat <- solve(Theta_search$hessian)
# standard error the theta should be the second diagonal element
theta_se <- sqrt(V_hat[2,2])
# obtain the 95% confidence interval bounds
# critical value
z_c <- qnorm(1 - 0.05/2)
# bounds
theta_clb <- theta - z_c*theta_se
theta_cub <- theta + z_c*theta_se
# display bounds
# print output
sprintf("theta 0.95 CI: (%.3f, %.3f)", theta_clb, theta_cub)
# 95% CI for mu (just in case)
# standard error the mu should be the first diagonal element
mu_se <- sqrt(V_hat[1,1])
# obtain the 95% confidence interval bounds
# critical value already computed
# bounds
mu_clb <- mu - z_c*mu_se
mu_cub <- mu + z_c*mu_se
# display bounds
# print output
sprintf("mu 0.95 CI: (%.3f, %.3f)", mu_clb, mu_cub)

# part (c) - testing mu = 0
# mu MLE and se already extracted earlier
z_t <- mu/mu_se
# critical value extracted earlier in b as z_c - go ahead and compare
reject <- 'no'
if(z_t>z_c){
	reject <- 'yes'
}
# compute p-value as well
pval <- 2*(1-pnorm(abs(z_t)))
# print the test stat and the critical value
sprintf('z_t: %.2f, z_c: %.2f', z_t, z_c)
sprintf('Reject?: %s', reject)
sprintf('p_value: %s', pval)