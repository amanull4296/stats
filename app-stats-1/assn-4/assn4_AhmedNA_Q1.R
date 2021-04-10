# Author: Ahmed N Amanullah
# St Num: 1001325773
# STA2101 Assignment 4 Ques 1

# first read data
d_source <- "http://www.utstat.toronto.edu/~brunner/data/legal/"
d_source <- paste(d_source, "openSAT.data.txt",sep="")
uniapp_data <- read.table(d_source, header = T)

# Will seek to predict GPA from SAT scores (GPA is thus response var)

# Part (a)
# fit model with only Math as predictor
lm_a <- lm(GPA ~ MATH, uniapp_data)
# summary of fitted model
summary(lm_a)
# predict GPA of student with Math SAT score of 700
gpa_m700 <- predict(lm_a, data.frame(MATH=700))
sprintf("Predicted GPA for SAT Math score of 700: %.2f", gpa_m700)

# Part (b)
# fit model with both test scores as predictor
lm_b <- lm(GPA ~ MATH + VERBAL, uniapp_data)
# summary of fitted model
summary(lm_b)

# Part (b) (i) (B) (also part (b) (iii))
# Testing for coefficient for MATH = 0
# So we control for VERBAL
# we could fit a reduced model with just the verbal score
lm_v <- lm(GPA ~ VERBAL, uniapp_data)
# and use anova comparing the just verbal and full models
anova(lm_v, lm_b)

# Part (b) (i) (C) (also part (b) (ii))
# Testing for coefficient for VERBAL = 0
# So we control for MATH
# reduced model with just the math score already fit (lm_a)
# use anova comparing the just math and full models
anova(lm_a, lm_b)

# part (b) (iv)
# predict GPA for math and verbal scores of 700 and 650
gpa_m700_v650 <- predict(lm_b, data.frame(MATH=700, VERBAL=650))
sprintf("Pred GPA for Verbal 650 and Math 700: %.2f", gpa_m700_v650) 

# part (b) (v)
# test beta1 = beta2 <=> beta1 - beta2 = 0
# retrieve ftest function shared in class
source("http://www.utstat.utoronto.ca/~brunner/Rfunctions/ftest.txt")
# run ftest
ftest(lm_b, rbind(c(0, 1, -1)))