# Author: Ahmed N Amanullah
# St #: 1001325773
# STA2101 Assn2

# Question 1 computation
A <- rbind(c(1, 2), c(2, 4))
B <- rbind(c(0, 2), c(2, 1))
C <- rbind(c(2, 0), c(1, 2))

# AB and AC computation
AB <- A %*% B
AC <- A %*% C

# display output
print(AB)
print(AC)

# Question 17 computation
# set seed
set.seed(9999)
# Monte Carlo sample size
m <- 10000
# generate uniform density variates
# density function, f, being used is that of the uniform distribution
# with endpoints 0 and 0.5 (the integral bounds)
X <- runif(m, 0, 0.5)
# next compute Y from X
# note that density function, f(x) = 2 for 0<=x<=0.5 and 0 elsewhere
# so Y = (h(X)/f(X)) = (1/2)*h(X), where h(X) is integrand provided
Y <- 0.5*exp(cos(1/X))
# compute mean of Y - this is the approximate integral by SLLN
I <- mean(Y)
# need variance of Y for computing confidence interval
s <- var(Y)
# next need the standard normal test stat for the significance level
alpha <- 0.01
z_c <- -qnorm(alpha/2)
# now compute lower and upper bounds
# interval below derived from dist'n sqrt(m)*(Y_bar-mu) ~ N(0,sigma^2)
# which is provided by the CLT
# not using t-stat as random variates not from a normal dist'n
lb <- I - z_c*(sqrt(s)/sqrt(m))
ub <- I + z_c*(sqrt(s)/sqrt(m))
# print the estimate
sprintf('Integral: %.4f',I)
# print the cf
sprintf('Lower confidence limit: %.4f', lb)
sprintf('Upper confidence limit: %.4f', ub)