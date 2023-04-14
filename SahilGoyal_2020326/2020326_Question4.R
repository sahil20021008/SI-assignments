rm(list = ls())
set.seed(12345)
library(stats)

n1 <- 9
n2 <- 16
mean1 <- 2
mean2 <- 3.2
std1 <- sqrt(0.75)
std2 <- 1
alpha <- 0.05

test_stat <-
  (mean2 - mean1) / (sqrt(((std1 ^ 2) / n1) + ((std2 ^ 2) / n2)))
cat("The test statistic is:", test_stat)

test_crit <- qt(p = alpha / 2,
                df = min(n1 - 1, n2 - 1),
                lower.tail = T)
cat("The first critical value is:", test_crit)
test_crit <- qt(p = alpha / 2,
                df = min(n1 - 1, n2 - 1),
                lower.tail = F)
cat("The second critical value is:", test_crit)

p_val <- 2 * pt(q = test_stat,
                df = min(n1 - 1, n2 - 1),
                lower.tail = F)
cat("The P value is:", p_val)
