rm(list = ls())
set.seed(12345)
library(stats)

la <- c(49, 53, 51, 52, 47, 50, 52, 53)
lb <- c(52, 55, 52, 53, 50, 54, 54, 53)

num_child <- 8
alpha <- 0.05

temp <- t.test(
  x = la,
  y = lb,
  paired = T,
  alternative = "two.sided"
)
print(temp)
cat("The test statistic is:", temp$statistic)
test_crit <- qt(p = alpha / 2,
                df = num_child - 1,
                lower.tail = T)
cat("The first critical value is:", test_crit)
test_crit <- qt(p = alpha / 2,
                df = num_child - 1,
                lower.tail = F)
cat("The second critical value is:", test_crit)
cat("The P value is:", temp$p.value)
