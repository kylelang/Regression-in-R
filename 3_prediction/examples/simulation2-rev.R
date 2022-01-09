### Title:    Missing Data Course: Lecture 4 Simulation
### Author:   Kyle M. Lang
### Created:  2015-SEP-14
### Modified: 2015-SEP-14

            #install.packages("geoR", repos = "http://rweb.quant.ku.edu/cran")

rm(list = ls(all = TRUE))
                            
set.seed(235711)

library(mvtnorm)
library(mice)
library(pscl)
library(geoR)
library(mitools)

nObs <- 1000
pm <- 0.3
sigma <- matrix(c(1.0, 0.5, 0.0,
                  0.5, 1.0, 0.3,
                  0.0, 0.3, 1.0),
                ncol = 3)

nReps <- 500
nImps <- nSams <- 100
tVec <- tVec1 <- tVec2 <-
  corVec <- corVec1 <- corVec2 <- rep(NA, nReps)

## Run a small simulation to examine correlation bias
## and Type I error rates between different imputation methods:
for(rp in 1 : nReps) {
  cat(paste0("Doing replication number: ",
             rp,
             ".\n")
      )
  simData <- as.data.frame(rmvnorm(nObs, c(0, 0, 0), sigma))
  colnames(simData) <- c("y", "x", "z")

### Complete Data Fit ###
  fit <- lm(y ~ z, data = simData)
  tVec[rp] <- sqrt(summary(fit)$fstat[1])
  corVec[rp] <- with(simData, cor(x, y))
  
  ## Simulate MAR data:
  missData <- simData
  rVec <- pnorm(missData$x,
                mean = mean(missData$x),
                sd = sd(missData$x)) < pm
  missData[rVec, "y"] <- NA
  
  
### PN-Type MI ###
  mice1 <- mice(data      = missData,
                m         = nImps,
                maxit     = 1,
                printFlag = FALSE,
                method    = "norm.nob")

  imps1 <- list()
  for(m in 1 : mice1$m) imps1[[m]] <- complete(mice1, m)

### PNE-Type MI ###
  mice2 <- mice(data      = missData,
                m         = nImps,
                maxit     = 1,
                printFlag = FALSE,
                method    = "norm")

  imps2 <- list()
  for(m in 1 : mice2$m) imps2[[m]] <- complete(mice2, m)

### Parameter Estimation & Pooling ###
  fitList1 <-lapply(imps1, FUN = function(dat) lm(y ~ z, dat))
  corList1 <-lapply(imps1, FUN = function(dat) with(dat, cor(x, y)))
  
  pool1    <- MIcombine(fitList1)
  t1[rp]   <- (coef(pool1) / sqrt(diag(vcov(pool1))))[2]
  cor1[rp] <- mean(unlist(corList1))
  
  fitList2 <-lapply(imps2, FUN = function(dat) lm(y ~ z, dat))
  corList2 <-lapply(imps2, FUN = function(dat) with(dat, cor(x, y)))
  
  pool2    <- MIcombine(fitList2)
  t2[rp]   <- (coef(pool2) / sqrt(diag(vcov(pool2))))[2]
  cor2[rp] <- mean(unlist(corList2))
}

tMat <- cbind(tVec, tVec1, tVec2)
corMat <- cbind(corVec, corVec1, corVec2)
colnames(tMat) <- colnames(corMat) <-
  c("Complete Data", "PN-Type", "PNE-Type")

sigMat <- abs(tMat) > 1.96
errorRates <- colMeans(sigMat)
corVals <- colMeans(corMat)

outMat <- rbind(corVals, errorRates)
rownames(outMat) <- c("cor(X, Y)", "Type I Error")
outMat

saveRDS(outMat, "simResMat2.rds")
