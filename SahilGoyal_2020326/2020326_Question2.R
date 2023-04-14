rm(list = ls())
set.seed(12345)
library(stats)

num_death <- 75
samp_mean <- 17.4
samp_std <- 6.3
mean_null <- 15
alpha <- 0.05

test_stat <- (samp_mean - mean_null) / (samp_std / sqrt(num_death))
cat("The test statistic is:", test_stat)

test_crit <- qt(p = alpha / 2,
                df = num_death - 1,
                lower.tail = T)
cat("The first critical value is:", test_crit)
test_crit <- qt(p = alpha / 2,
                df = num_death - 1,
                lower.tail = F)
cat("The second critical value is:", test_crit)

p_val <- 2 * pt(q = test_stat,
                df = num_death - 1,
                lower.tail = F)
cat("The P value is:", p_val)
