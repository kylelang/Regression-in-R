### Title:    Imputation Visualizations
### Author:   Kyle M. Lang
### Created:  2020-09-17
### Modified: 2020-09-17

set.seed(235711)

library(mvtnorm)
library(ggplot2)
library(LaplacesDemon)

source("../../../code/supportFunctions.R")

## Simulate some toy data:
nObs <- 1000 # Sample Size
pm   <- 0.3  # Proportion Missing

sigma <- matrix(c(1.0, 0.5, 0.0,
                  0.5, 1.0, 0.3,
                  0.0, 0.3, 1.0),
                ncol = 3)

dat0 <- as.data.frame(rmvnorm(nObs, c(0, 0, 0), sigma))
colnames(dat0) <- c("y", "x", "z")
dat1 <- dat0

## Impose MAR Nonresponse:
rVec <- with(dat1, x < quantile(x, probs = pm))

dat1[rVec, "y"] <- NA

## Subset the data:
yMis <- dat1[rVec, ]
yObs <- dat1[!rVec, ]

## Visualize the hypothetical data:
p0 <- with(dat0, gg0(x = x, y = y))
p0

## Visualize missing data problem:
p1 <- with(yObs, gg0(x = x, y = y)) +
    geom_point(mapping = aes(x = x, y = y), data = dat0[rVec, ], color = "gray")
p1

## Plot the imputation model's best-fit line:
fit0 <- lm(y ~ x, data = yObs)
b0   <- coef(fit0)
s0   <- summary(fit0)$sigma

p2 <- p1 + geom_abline(intercept = b0[1], slope = b0[2], color = "blue", lwd = 1)
p2

## Plot the deterministic imputations:
imps <- predict(fit0, newdata = yMis)
p2 + geom_point(mapping = aes(x = yMis[, "x"], y = imps),  color = "red")

## Plot stochastic imputations:
imps <- imps + rnorm(length(imps), 0, s0)
p2 + geom_point(mapping = aes(x = yMis[, "x"], y = imps),  color = "red")

### Plot MI ###

## Estimate the imputation model:
nSams <- 100000
preds <- cbind(1, yObs$x)
df    <- nrow(yObs) - length(b0)

sigmaScale <- (1 / df) * crossprod(yObs$y - preds %*% b0)
sigmaSams  <- rinvchisq(nSams, df = df, scale = sigmaScale)

betaVarDenom <- solve(crossprod(preds))

betaSams <- matrix(NA, nSams, length(b0))
for(m in 1 : nSams) {
    betaSigma <- sigmaSams[m] * betaVarDenom
    betaSams[m, ] <- rmvnorm(1, mean = b0, sigma = betaSigma)
}

## Sample and plot imputations:
index <- sample(1 : nSams, 5)

i  <- 2
b1 <- betaSams[index[i], ]
s1 <- sigmaSams[index[i]]

p3 <- p1 + geom_abline(intercept = b1[1], slope = b1[2], color = "blue", lwd = 1)
p3

mPreds <- cbind(1, yMis$x)
imps   <- mPreds %*% b1 + rnorm(nrow(yMis), 0, sqrt(s1))

p3 + geom_point(mapping = aes(x = yMis$x, y = imps), color = "red")

## Visualize posterior distributions:
plot(density(sigmaSams))
plot(density(betaSams[ , 1]))
plot(density(betaSams[ , 2]))

dS <- density(sigmaSams)
dB0 <- density(betaSams[ , 1])
dB1 <- density(betaSams[ , 2])

gg0(x = dS$x, y = dS$y, points = FALSE) + geom_line() + xlab("Residual Variance") + ylab("Density")
gg0(x = dB0$x, y = dB0$y, points = FALSE) + geom_line() + xlab("Intercept") + ylab("Density")
gg0(x = dB1$x, y = dB1$y, points = FALSE) + geom_line() + xlab("Slope") + ylab("Density")
