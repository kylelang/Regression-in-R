### Title:    Lecture 7 Examples
### Author:   Kyle M. Lang
### Created:  2017-AUG-24
### Modified: 2018-AUG-22

rm(list = ls(all = TRUE))

install.packages("wec", repos = "http://cloud.r-project.org")

plotDir <- "../latexStuff/figures/"
dataDir <- "../data/"

library(wec)
source("../../../code/supportFunctions.R")

##### Example models

cDat <- readRDS(paste0(dataDir, "cars_data.rds"))

atOnly <- abs(cDat$mtOpt - 1)
all    <- as.numeric(with(cDat, front + rear == 0))

cDat <- data.frame(cDat[ , 1 : 2],
                   atOnly = atOnly,
                   cDat[ , 3 : 5],
                   all = all,
                   cDat[ , 6 : 7])

saveRDS(cDat, paste0(dataDir, "cars_data2.rds"))

rowSums(cDat[ , c("front", "rear", "all")])

table(atOnly, cDat$mtOpt)

data(mtcars)

tmp  <- factor(mtcars$gear)

cmCon <- diag(3)
colnames(cmCon) <- rownames(cmCon) <- c(3, 4, 5)
contrasts(tmp) <- cmCon


summary(lm(mtcars$hp ~ tmp))
summary(lm(mtcars$hp ~ tmp - 1))

tmp1 <- model.matrix(~ tmp - 1)

head(tmp)

y <- scale(mtcars$hp, scale = FALSE)

out1 <- lm(mtcars$hp ~ tmp1 - 1)
out2 <- lm(hp ~ factor(gear), data = mtcars)

cf1 <- coef(out1)
cf2 <- coef(out2)

cf1 - cf2

av1 <- anova(out1)

ls(av1)

ssm0 <- av1[1, "Sum Sq"]
msm0 <- ssm0 / 3

ssm1 <- crossprod(out1$fitted.values - mean(mtcars$hp))
msm1 <- ssm1 / 2
f1   <- msm1 / av1[2, "Mean Sq"]
r2   <- ssm1 / crossprod(scale(mtcars$hp, scale = FALSE))

s1 <- summary(out1)
s1$r.squared <- r2
s1$fstatistic[c(1, 2)] <- c(f1, 2)

s1

summary(out2)
ls(out1)

out1$rank

obj <- out1

summary.cellMeans <- function(obj) {
    ## Get broken summaries:
    s0  <- summary.lm(obj)
    av0 <- anova(obj)

    ## Extract model info:
    y  <- obj$model[ , 1]
    df <- obj$rank - 1

    ## Compute correct measures of variability:
    ss <- crossprod(obj$fitted.values - mean(y))
    ms <- ss / df 

    ## Compute correct stats:
    r2  <- ss / crossprod(y - mean(y))
    r2a <- 1 - (1 - r2) * ((length(y) - 1) / obj$df.residual)
    f   <- ms / av0["Residuals", "Mean Sq"]

    ## Replace broken stats:
    s0$r.squared           <- r2
    s0$adj.r.squared       <- r2a
    s0$fstatistic[c(1, 2)] <- c(f, df)

    s0 # Return corrected summary
}

cDat <- readRDS(paste0(dataDir, "cars_data.rds"))

out1 <- lm(price ~ atOnly + mtOpt - 1, data = cDat)
out2 <- lm(price ~ mtOpt, data = cDat)

summary(out1)
summary(out2)

class(out1) <- c("cellMeans", class(out1))

summary(out1)

summary.cellMeans(out1)
summary(out2)

with(mtcars, crossprod(fitted(out1) - mean(hp)) / crossprod(scale(hp, scale = FALSE)))

summary(out2)$r.squared

## Generate weighted effects codes:
mt <- factor(cDat$mtOpt, levels = c(0, 1), labels = c("no", "yes"))

contrasts(mt) <- contr.wec(mt, "no")
mtOpt.wec     <- model.matrix(~mt)[ , -1]

dr                  <- rep("all", nrow(cDat))
dr[cDat$front == 1] <- "front"
dr[cDat$rear == 1]  <- "rear"
dr                  <- factor(dr)

contrasts(dr)    <- contr.wec(dr, "all")
dr.wec           <- model.matrix(~dr)[ , -1]
colnames(dr.wec) <- c("front.wec", "rear.wec")

## Revert back to default contrasts:
contrasts(dr) <- contr.treatment(n = levels(dr))
contrasts(mt) <- contr.treatment(n = levels(mt))

cDat <-
    data.frame(cDat[ , 1 : 3], mtOpt.wec, cDat[ , -c(1 : 3)], dr.wec, mt, dr)

head(cDat)

saveRDS(cDat, paste0(dataDir, "cars_data2.rds"))

out1 <- lm(price ~ front + rear, data = cDat)
out2 <- lm(price ~ front.ec + rear.ec, data = cDat)
out3 <- lm(price ~ front.wec + rear.wec, data = cDat)

grpMeans <- tapply(cDat$price, cDat$dr, mean)

coef(out1)[1] - grpMeans["all"]
coef(out2)[1] - mean(grpMeans)
coef(out3)[1] - mean(cDat$price)

tmp <- summary(out1)

ls(tmp)

head(mtcars)

carb <- mtcars$carb

c1 <- rep(-0.5, length(carb))
c1[carb >= 4] <- 0.5

c2 <- rep(0, length(carb))
c2[carb == 1] <- -0.5
c2[carb == 2] <- 0.5

c3 <- rep(0, length(carb))
c3[carb == 1 | carb == 2] <- -0.5
c3[carb == 3 | carb == 4] <- 0.5

c4 <- rep(0, length(carb))
c4[carb < 4]  <- -0.25
c4[carb == 4] <- 0.75

c5 <- rep(0, length(carb))
c5[carb == 1]            <- -0.75
c5[carb > 1 & carb <= 4] <- 0.25

codes  <- data.frame(carb, c1, c2, c3, c4, c5)
desMat <- aggregate(codes, by = list(carb), FUN = mean)

colMeans(desMat[ , -c(1, 2)])

crossprod(desMat$c1, desMat$c2)
crossprod(desMat$c1, desMat$c3)
crossprod(desMat$c1, desMat$c4)
crossprod(desMat$c1, desMat$c5)

crossprod(desMat$c2, desMat$c3)
crossprod(desMat$c2, desMat$c4)
crossprod(desMat$c2, desMat$c5)

crossprod(desMat$c3, desMat$c4)
crossprod(desMat$c3, desMat$c5)

crossprod(desMat$c4, desMat$c5)
