### Title:    Central Limit Theorem Demo
### Author:   Kyle M. Lang
### Created:  2020-02-26
### Modified: 2020-02-26

n     <- 500
nReps <- 50000

b <- rep(NA, nReps)
x <- runif(n)
for(rp in 1 : nReps) {
                                        #x <- runif(n)
    y <- 0.5 * x + rgamma(n, 1, 1) #rnorm(n)
    
    b[rp] <- coef(lm(y ~ x))[2]
}

                                        #hist(b, freq = FALSE)
                                        #plot(density(rgamma(10000, 1, 1)))

plot(density(b))
x <- seq(min(b), max(b), length.out = 10000)
lines(x = x, y = dnorm(x, mean(b), sd(b)), col = "red")

X <- matrix(runif(2000), 1000)

dim(X)

b <- matrix(c(0.5, 0.75))

y <- X %*% b

fit <- lm(y ~ X)

A <- solve(crossprod(X)) %*% t(X)

A %*% y

H <- X %*% A

p1 <- H %*% y
p0 <- fitted(fit)

p1 - p0

tmp <- c()
for(i in 1 : length(y)) tmp[i] <- sum(H[i, ] * y)

p1 - tmp

tmp1 <- tmp2 <- c()
for(n in 1 : length(y)) {
    tmp1[n] <- A[1, n] * y[n]
    tmp2[n] <- A[2, n] * y[n]
}

sum(tmp1)
sum(tmp2)

colSums(tmp)

n <- 10000

x <- runif(n)
y <- x * 0.5 + rnorm(n, 0, 0.1)

xm <- mean(x)
ym <- mean(y)

summary(lm(y ~ x))

cov(x, y) / var(x)

crossprod(x - xm, y - ym) / crossprod(x - xm)

tmp1 <- tmp2 <- c()
for(i in 1 : n) {
    tmp1[i] <- (x[i] - xm) * (y[i] - ym) 
    tmp2[i] <- (x[i] - xm)^2
}

sum(tmp1) / sum(tmp2)
sum(tmp)

mean((x / var(x)) * y)

