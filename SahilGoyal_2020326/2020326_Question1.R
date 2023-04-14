rm(list = ls())
set.seed(12345)
library(stats)

num_bread <- 10
samp_mean <- 17
samp_std <- 0.5
mean_null <- 15
alpha <- 0.05

test_stat <- (samp_mean - mean_null) / (samp_std / sqrt(num_bread))
cat("The test statistic is:", test_stat)

test_crit <- qnorm(p = alpha,
                   lower.tail = F)
cat("The critical value is:", test_crit)

p_val <- pnorm(q = test_stat,
               lower.tail =  F)
cat("The P value is:", p_val)
