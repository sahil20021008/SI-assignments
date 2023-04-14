rm(list = ls())
set.seed(12345)
library(stats)

num_field <- 10
mean_null <- 12
x <- c(14.3, 12.6, 13.7, 10.9, 13.7, 12.0, 11.4, 12.0, 12.6, 13.1)
samp_mean <- mean(x)
samp_std <- sd(x)
alpha <- 0.05


test_stat <- (samp_mean - mean_null) / (samp_std / sqrt(num_field))
cat("The test statistic is:", test_stat)

test_crit <- qt(p = alpha / 2,
                df = num_field - 1,
                lower.tail = T)
cat("The first critical value is:", test_crit)
test_crit <- qt(p = alpha / 2,
                df = num_field - 1,
                lower.tail = F)
cat("The second critical value is:", test_crit)

p_val <- 2 * pt(q = test_stat,
                df = num_field - 1,
                lower.tail = F)
cat("The P value is:", p_val)
