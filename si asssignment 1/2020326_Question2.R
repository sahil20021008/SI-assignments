rm(list = ls())
set.seed(12345)
library(stats)

# a part

data <-
  read.table(
    "C:/Users/91982/Downloads/SahilGoyal_2020326/data.csv",
    sep = ",",
    header = T
  )
data <- data[, 2]

mean = mean(data)
variance = var(data)


log_like <- function(info, data) {
  return (-(-500 * .5 * log(2 * pi * info[2]) - sum((info[1] - data) ** 2) *
              0.5 / info[2]))
}

mle <- function(mean, variance, data) {
  info <- c(mean, variance)
  nlminb(info, log_like, data = data)
}
print(mle(mean, variance, data))

# b part

mean.seq <- seq(3999, 4001, by = 0.1)
variance.seq <- seq(14, 16, by = 0.1)

ll1 <- c()
for (i in mean.seq) {
  temp <- -1 * log_like(c(i, variance), data)
  ll1 <- c(ll1, temp)
}
ll2 <- c()
for (i in variance.seq) {
  temp <- -1 * log_like(c(mean, i), data)
  ll2 <- c(ll2, temp)
}
plot(mean.seq,
     ll1,
     ylab = "Likelihood Function",
     xlab = "Mean",
     main = "Likelihood function with fixed variance")
plot(variance.seq,
     ll2,
     ylab = "Likelihood Function",
     xlab = "Variance",
     main = "Likelihood Function with fixed mean")
