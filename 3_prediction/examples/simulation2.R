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

myMI <- TRUE # Do hand-coded imputation vs. using mice()

nObs <- 1000
pm <- 0.3
sigma <- matrix(c(1.0, 0.5, 0.0,
                  0.5, 1.0, 0.3,
                  0.0, 0.3, 1.0),
                ncol = 3)

nReps <- 500
nSams <- nImps <- 100
tVec <- tVec1 <- tVec2 <- tVec3 <-
  corVec <- corVec1 <- corVec2 <- corVec3 <- rep(NA, nReps)

## Run a small simulation to examine correlation bias
## and Type I error rates between different imputation methods:
for(rp in 1 : nReps) {
  cat(paste0("Doing replication number: ",
             rp,
             ".\n")
      )
  simData <- as.data.frame(rmvnorm(nObs, c(0, 0, 0), sigma))
  colnames(simData) <- c("y", "x", "z")
  
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

  if(myMI) {
    ## Get deterministic imputations:
    imp1 <- missPredData %*% betaHat
    
    ## Fill missing cells in Y:
    impData1 <- missData
    impData1[rVec, "y"] <- imp1
  } else {
    miceOut1 <- mice(missData,
                     m = 1,
                     maxit = 1,
                     method = c("norm.predict", "", ""),
                     printFlag = FALSE)
    impData1 <- complete(miceOut1)
  }
                  
  fit1 <- lm(y ~ z, impData1)
  tVec1[rp] <- sqrt(summary(fit1)$fstat[1])
  corVec1[rp] <- with(impData1, cor(x, y))
  
  ## Get stochastic imputations:
  if(myMI) {
    imp2 <-
      missPredData %*% betaHat +
        rnorm(nMissing, 0, sqrt(sigma2Hat))
                         
    ## Fill missing cells in Y:
    impData2 <- missData
    impData2[rVec, "y"] <- imp2
  } else {
    miceOut2 <- mice(missData,
                     m = 1,
                     maxit = 1,
                     method = c("norm.nob", "", ""),
                     printFlag = FALSE)
    impData2 <- complete(miceOut2)
  }
  
  fit2 <- lm(y ~ z, impData2)
  tVec2[rp] <- sqrt(summary(fit2)$fstat[1])
  corVec2[rp] <- with(impData2, cor(x, y))
  
  if(myMI) { # Do MI by hand
    df <- nObserved - length(betaHat)
    sigmaScale <- (1 / df) * crossprod(yObs$y - obsPredData %*% betaHat)
    
    sigmaSams <- rinvchisq(nSams, df = df, scale = sigmaScale)
    
    betaVarDenom <- solve(crossprod(obsPredData))
    
    betaSams <- matrix(NA, nSams, length(betaHat))
    for(m in 1 : nSams) {
      betaSigma <- sigmaSams[m] * betaVarDenom
      betaSams[m, ] <- rmvnorm(1, mean = betaHat, sigma = betaSigma)
    }
    
    impMat <- matrix(NA, nMissing, nImps)
    for(m in 1 : nImps) {
      impMat[ , m] <-
        missPredData %*% matrix(betaSams[m, ]) +
          rnorm(nMissing, 0, sqrt(sigmaSams[m]))
    }

    impList <- list()
    impData3 <- missData
    for(m in 1 : nImps) {
      impData3[rVec, "y"] <- impMat[ , m]
      impList[[m]] <- impData3
    }
  } else {# Use mice() to do MI:
    miceOut1 <- mice(missData,
                     m = nImps,
                     maxit = 1,
                     method = c("norm", "", ""),
                     printFlag = FALSE)

    impList <- list()
    for(m in 1 : nImps) impList[[m]] <- complete(miceOut1, m)
  }
  
  miFitList <-lapply(impList,
                     FUN = function(impDat){
                       lm(y ~ z, impDat)
                     }
                     )
  
  miCorList <-lapply(impList,
                     FUN = function(impDat){
                       with(impDat, cor(x, y))
                     }
                     )
  
  miPooled <- MIcombine(miFitList)

  tVec3[rp] <- (coef(miPooled) / sqrt(diag(vcov(miPooled))))[2]
  corVec3[rp] <- mean(unlist(miCorList))
}

tMat <- cbind(tVec, tVec1, tVec2, tVec3)
corMat <- cbind(corVec, corVec1, corVec2, corVec3)
colnames(tMat) <- colnames(corMat) <-
  c("Complete Data", "Conditional Mean", "Stochastic", "MI")

sigMat <- abs(tMat) > 1.96
errorRates <- colMeans(sigMat)
corVals <- colMeans(corMat)

outMat <- rbind(corVals, errorRates)
rownames(outMat) <- c("cor(X, Y)", "Type I Error")
outMat

saveRDS(outMat, "simResMat.rds")
