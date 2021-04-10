# Author: Ahmed N Amanullah
# St Num: 1001325773
# STA2101 Assignment 4 Ques 8

# first retrieve the data
d_source <- "http://www.utstat.toronto.edu/~brunner/data/legal/"
d_source <- paste(d_source, "xy.data.txt", sep = "")
xy_data <- read.table(d_source, header = T)

# part (a)
# fit regression line through origin
lm1 <- lm(y ~ x-1, data = xy_data)

# get summary
summary(lm1)

# check to see if parameter estimate in summary and given by formula
#	match
beta1_form <- sum(xy_data$x*xy_data$y)/sum(xy_data$x^2)
sprintf("via formula: %.4f", beta1_form)

# they do, so just use the one from summary
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
# degrees of freedom n-1 as only one parameter estimated in this
#	regression model
beta1_clb <- beta1 - qt(0.975, df=n-1)*beta1_se
beta1_cub <- beta1 + qt(0.975, df=n-1)*beta1_se
# print output
sprintf("beta1: %.3f", beta1)
sprintf("beta1 0.95 CI: (%.3f, %.3f)", beta1_clb, beta1_cub)

# part (b)
# first get estimate for beta2
beta2 <- sum(xy_data$y)/sum(xy_data$x)
# next need to compute derived asymptotic se
# length of data
n <- dim(xy_data)[1]

# next compute the standard error derived using delta method
# sample means and variances
mu_x <- mean(xy_data$x)
mu_y <- mean(xy_data$y)
# am not using the (n-1)/n correction factor here because otherwise answer
#	is off from prof's (by 0.001); maybe he forgot to do it? Or we're not
#	doing it here for some reason?
Sigma <- var(xy_data)

#compute s.e.
beta2_var <- (mu_y^2/mu_x^4)*Sigma[1,1] + (Sigma[2,2]/mu_x^2)
beta2_var <- (1/n)*(beta2_var - 2*(mu_y/mu_x^3)*Sigma[1,2])
beta2_se <- sqrt(beta2_var)

# 95% interval
# compute confidence interval bounds and output
beta2_clb <- beta2 - qnorm(0.975)*beta2_se
beta2_cub <- beta2 + qnorm(0.975)*beta2_se
# print output
sprintf("beta2: %.3f", beta2)
sprintf("beta2 0.95 CI: (%.3f, %.3f)", beta2_clb, beta2_cub)