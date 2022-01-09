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
  
  nMissing <- sum(rVec)
  nObserved <- sum(!rVec)
  yMiss <- missData[rVec, ]
  yObs <- missData[!rVec, ]
  missPredData <- as.matrix(cbind(1, yMiss[ , -1]))
  obsPredData <- as.matrix(cbind(1, yObs[ , -1]))
  
  ## Get the imputation model moments:
  lsFit <- lm(y ~ x + z, data = missData, na.action = "na.omit")
  betaHat <- matrix(coef(lsFit))
  sigma2Hat <- var(resid(lsFit))
  
### PN-Type MI ###
  impList1 <- list()
  for(m in 1 : nImps) {
    impList1[[m]] <- missData
    impList1[[m]][rVec, "y"] <-
      missPredData %*% betaHat +
        rnorm(nMissing, 0, sqrt(sigma2Hat))
  }
  
  miFitList1 <-lapply(impList1,
                      FUN = function(impDat){
                        lm(y ~ z, impDat)
                      }
                      )
  
  miCorList1 <-lapply(impList1,
                      FUN = function(impDat){
                        with(impDat, cor(x, y))
                      }
                      )
  
  miPooled1 <- MIcombine(miFitList1)

  tVec1[rp] <- (coef(miPooled1) / sqrt(diag(vcov(miPooled1))))[2]
  corVec1[rp] <- mean(unlist(miCorList1))
  
### PNE-Type MI ###
  df <- nObserved - length(betaHat)
  sigmaScale <- (1 / df) * crossprod(yObs$y - obsPredData %*% betaHat)
  
  sigmaSams <- rinvchisq(nSams, df = df, scale = sigmaScale)
  
  betaVarDenom <- solve(crossprod(obsPredData))
  
  betaSams <- matrix(NA, nSams, length(betaHat))
  for(m in 1 : nSams) {
    betaSigma <- sigmaSams[m] * betaVarDenom
    betaSams[m, ] <- rmvnorm(1, mean = betaHat, sigma = betaSigma)
  }
  
  impList2 <- list()
  for(m in 1 : nImps) {
    impList2[[m]] <- missData
    impList2[[m]][rVec, "y"] <-
      missPredData %*% matrix(betaSams[m, ]) +
        rnorm(nMissing, 0, sqrt(sigmaSams[m]))
  }
  
  miFitList2 <-lapply(impList2,
                      FUN = function(impDat){
                        lm(y ~ z, impDat)
                      }
                      )
  
  miCorList2 <-lapply(impList2,
                      FUN = function(impDat){
                        with(impDat, cor(x, y))
                      }
                      )
  
  miPooled2 <- MIcombine(miFitList2)

  tVec2[rp] <- (coef(miPooled2) / sqrt(diag(vcov(miPooled2))))[2]
  corVec2[rp] <- mean(unlist(miCorList2))
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
