rm(list = ls())
set.seed(12345)
library(stats)
# part a

ran_exp <- function(n, lambda) {
  rexp(n, rate = lambda)
}

n <- 1000

samp1 <- ran_exp(n, 1)
samp2 <- ran_exp(n, 2)
samp3 <- ran_exp(n, 3)
samp4 <- ran_exp(n, 4)

plot(samp1,
     type = "l",
     col = "yellow",
     main = "lambda = 1")
plot(samp2,
     type = "l",
     col = "green",
     main = "lambda = 2")
plot(samp3,
     type = "l",
     col = "blue",
     main = "lambda = 3")
plot(samp4,
     type = "l",
     main = "lambda = 4",
     col = "red")

# b part
mle_exp <- function(par, data) {
  lambda = par[1]
  n <- length(data)
  return(-(n * log(lambda) - lambda * sum(data)))
}
# for lambda = 1
m1 <- nlminb(c(1), objective = mle_exp, data = samp1)
print(m1)
#using Method of Moments
m1 <- nlminb(c(1 / mean(samp1)), objective = mle_exp, data = samp1)
print(m1)
# using third set of values
m1 <- nlminb(c(0.5), mle_exp, data = samp1)
print(m1)

# for lambda = 2
m2 <- nlminb(c(2), mle_exp, data = samp2)
print(m2)
# using Method of Moments
m2 <- nlminb(c(1 / mean(samp2)), mle_exp, data = samp2)
print(m2)
#using third set of values
m2 <- nlminb(c(1.5), mle_exp, data = samp2)
print(m2)

#using lambda = 3
m3 <- nlminb(c(3), mle_exp, data = samp3)
print(m3)
# using Method of Moments
m3 <- nlminb(c(1 / mean(samp3)), mle_exp, data = samp3)
print(m3)
# using third set of values
m3 <- nlminb(c(2.5), mle_exp, data = samp3)
print(m3)

# using lambda = 4
m4 <- nlminb(c(4), mle_exp, data = samp4)
print(m4)
# using method of moments
m4 <- nlminb(c(1 / mean(samp4)), mle_exp, data = samp4)
print(m4)
#using third set of values
m4 <- nlminb(c(3.5), mle_exp, data = samp4)
print(m4)

l1 <- seq(0, 2, by = 0.05)
l2 <- seq(1, 3, by = 0.05)
l3 <- seq(2, 4, by = 0.05)
l4 <- seq(3, 5, by = 0.05)
c1 <- c()
c2 <- c()
c3 <- c()
c4 <- c()
for (i in l1) {
  c1 <- c(c1, -mle_exp(i, samp1))
}
for (i in l2) {
  c2 <- c(c2, -mle_exp(i, samp2))
}
for (i in l3) {
  c3 <- c(c3, -mle_exp(i, samp3))
}
for (i in l4) {
  c4 <- c(c4, -mle_exp(i, samp4))
}
plot(l1,
     c1,
     xlab = "lambda",
     ylab = "Likelihood Function",
     main = "Data with lambda = 1")
plot(l2,
     c2,
     xlab = "lambda",
     ylab = "Likelihood Function",
     main = "Data with lambda = 2")
plot(l3,
     c3,
     xlab = "lambda",
     ylab = "Likelihood Function",
     main = "Data with lambda = 3")
plot(l4,
     c4,
     xlab = "lambda",
     ylab = "Likelihood Function",
     main = "Data with lambda = 4")
