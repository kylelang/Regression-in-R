### Title:    Lecture 5 Examples
### Author:   Kyle M. Lang
### Created:  2017-AUG-24
### Modified: 2017-OCT-03

rm(list = ls(all = TRUE))

install.packages("sandwich", repos = "http://cloud.r-project.org")

plotDir <- "../latexStuff/figures/"
dataDir <- "../data/"

library(rgl)
library(visreg)
library(xtable)
library(lattice)
library(colorspace)
library(plotly)
library(ppcor)
library(ggplot2)
library(DAAG)
library(MASS)
library(caret)
library(car)
library(mvtnorm)
library(lmtest)
library(sandwich)

source("../../../code/supportFunctions.R")

data(Cars93)

x  <- Cars93$Horsepower
y1 <- Cars93$Price
y2 <- log(y1)
y3 <- sqrt(y1)

plot(x = x, y = y1)
plot(x = x, y = y2)
plot(x = x, y = y3)

with(Cars93, plot(x = Horsepower, y = Price))
with(Cars93, plot(x = Horsepower, y = MPG.highway))

out1 <- lm(Price ~ Horsepower, data = Cars93)
plot(y = resid(out1), x = fitted(out1))

qqnorm(resid(out1))
qqline(resid(out1))


trans = function(x, e, d)
    dnorm(sinh(d * asinh(x) - e)) * d * cosh(d * asinh(x) - e) / sqrt(1 + x^2)


x <- seq(-2.5, 6.5, length.out = 1000)
plot(x = x, y = trans(x, e = 0.5, d = 1), type = "l")

gg0(x = x, y = trans(x, e = 0.5, d = 1), points = FALSE) +
    geom_line() +
    xlab("X") +
    ylab("Density")

x <- seq(-6.5, 2.5, length.out = 1000)
plot(x = x, y = trans(x, e = -0.5, d = 1), type = "l")

x <- seq(-2.5, 2.5, length.out = 1000)
plot(x = x, y = trans(x, e = 0, d = 1.5), type = "l")

x <- seq(-8.5, 8.5, length.out = 1000)
plot(x = x, y = trans(x, e = 0, d = 0.5), type = "l")


lines(x = x, y = dnorm(x), col = "red")

?qqplot

coeftest(out1, vcov = vcovHC(out1))
summary(out1)$coefficients

out2 <- lm(MPG.highway ~ Horsepower, data = Cars93)
plot(y = resid(out2), x = fitted(out2))

Cars93.2 <- Cars93[-39, ]

out2.2 <- update(out2, data = Cars93.2)

resettest(out2.2)

resettest(out2)

X <- rmvnorm(1000, c(0, 0), matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2))

x <- X[ , 1]
z <- X[ , 2]
w <- apply(X, 1, prod)

x <- rnorm(1000)
z <- rnorm(1000)

y <- x + z + w + rnorm(1000)

out1 <- lm(y ~ x + z)
out2 <- lm(y ~ x*z)

library(rockchalk)

x <- c(seq(-10, -8, length.out = 500),
       seq(8, 10, length.out = 500)
       )
x <- seq(-10, 10, length.out = 1000)

y <- rnorm(1000, 0, 1 / (2 + abs(x)))

plot(y)

plot(x, y)

plotSlopes(out2, plotx = x, modx = z)

plot(y = resid(out2), x = fitted(out2))

out3 <- update(out2, ". ~ . + I(Horsepower^2)")
summary(out3)
resettest(out3)

X <- rmvnorm(1000, c(0, 0), matrix(c(1.0, 0.73, 0.73, 1.0), 2, 2))
y <- X %*% matrix(c(0.5, 0.5)) + rnorm(1000)

out1 <- lm(y ~ X)
out2 <- lm(y ~ X[ , 1])

summary(out1)
summary(out2)

plot(y = resid(out1), x = fitted(out1))
plot(y = resid(out2), x = fitted(out2))

tmp <- data.frame(y = resid(out2), x = fitted(out2))

ggplot(data = tmp, aes(x = x, y = y)) +
    geom_point() +
    theme_classic() +
    geom_smooth(aes(x = x, y = y), method = "loess")

ls(tmp)

tmp$method
tmp$parameter
tmp$statistic
Cars93$yh2 <- fitted(out3)

tmp <- lm(MPG.highway ~ Horsepower + I(Horsepower^2) + I(yh2^2) + I(yh2^3), data = Cars93)

anova(out3, tmp)

out4 <- update(out3, ". ~ . + I(Horsepower^3)")
resettest(out4)

r <- sqrt(abs(resid(out1)))
w <- 1 / fitted(lm(r ~ Cars93$Horsepower))
w <- 1 / Cars93$Horsepower

out1.2 <- lm(Price ~ Horsepower, weights = w, data = Cars93)

coef(out1)
coef(out1.2)

?lm

plot(y = resid(out1.2), x = fitted(out1.2))

plot(out1.2, 3)

out2 <- lm(Price ~ MPG.highway, data = Cars93)

?lm


tmp <- BoxCoxTrans(Cars93$Price)

p2 <- predict(tmp, Cars93$Price)

out1.2 <- lm(p2 ~ Cars93$Horsepower)



ls(tmp)

out2   <- lm(MPG.highway ~ Horsepower, data = Cars93)
out2.1 <- lm(MPG.highway ~ Horsepower + I(Horsepower^2), data = Cars93)
out2.2 <- lm(MPG.highway ~ Horsepower + I(Horsepower^2) + I(Horsepower^3),
             data = Cars93)
rstudent(out1)

tmp <- scale(Cars93$Price)

p1 <- ggplot(mapping = aes(sample = tmp)) + stat_qq()
p1 + ylim(c(-4.5, 4.5))

?qqplot

qqnorm(resid(out1))
qqline(resid(out1), probs = c(0.25, 0.95))

flag <- abs(rstudent(out1)) > 3
outs <- which(flag)

tmp <- resid(out1)[-outs]

qqnorm(tmp)
qqline(tmp)

sum(tmp)

summary(out1)
summary(out1.2)

dat1 <- data.frame(yHat1 = predict(out1),
                   res1  = resid(out1),
                   yHat2 = predict(out2),
                   res2  = resid(out2)
                   )

p1 <- gg0(x = predict(out1.2), y = resid(out1.2))

p1 + geom_hline(yintercept = 0, colour = "gray") +
    geom_hline(yintercept = c(-3, 3), colour = "red") +
    xlab("Predicted MPG") +
    ylab("Residual MPG")


p1 + geom_smooth(method = "loess", se = FALSE) +
    xlab("Predicted MPG") +
    ylab("Residual MPG")

p2 <- gg0(x = predict(out1), y = rstudent(out1))

p2 + xlab("Predicted Price") +
    ylab("Residual Price")


X <- as.matrix(Cars93[ , c("Horsepower", "MPG.city")])
X <- matrix(Cars93[ , "Horsepower"])
            
hMat <- X %*% solve(t(X) %*% X) %*% t(X)

hMat

yHat <- hMat %*% Cars93$Price

yHat

out1 <- lm(Price ~ Horsepower - 1, data = Cars93)

yHat2 <- predict(out1)

yHat - yHat2

y <- matrix(c(2, 2))
x <- matrix(c(1, 0.5, 1, 0.5), 2, 2)

x
y
x %*% y

a <- matrix(rnorm(4), 2, 2)
b <- matrix(rnorm(4), 2, 2)
c <- matrix(rnorm(4), 2, 2)

tmp1 <- a %*% b %*% c
tmp2 <- c %*% b %*% a

tmp1 == tmp2

yHat2 <- sort(


    

    

    plot(y = resid(out1), x = predict(out1))
plot(y = resid(out2), x = predict(out2))

lOut1 <- loess(resid(out2) ~ predict(out2))

points(y = predict(lOut1), x = predict(out2), col = "red")

lines(y = sort(predict(lOut1)), x = sort(predict(out2)))

ls(lOut1)

plot(lOut1$trace.hat)

?loess

x

cd <- cooks.distance(out1)
plot(cd)

dff <- dffits(out1)
plot(dff)

dfb <- dfbetas(out1)
plot(dfb[ , 1])
plot(dfb[ , 2])

im <- influence.measures(out1)

ls(im)

im$is.inf

cv <- covratio(out1)
plot(cv)

sr  <- rstudent(out1)
lev <- hatvalues(out1)
dff <- dffits(out1)
dfb <- dfbetas(out1)
cd  <- cooks.distance(out1)

index <- c(1 : nrow(Cars93))

sr2  <- cbind(index, sr)
lev2 <- cbind(index, lev)
dff2 <- cbind(index, dff)
dfb2 <- cbind(index, dfb)
cd2  <- cbind(index, cd)

sr3 <- sr2[order(abs(sr)), ]
sr3

lev3 <- lev2[order(lev), ]
lev3

dff3 <- dff2[order(abs(dff)), ]
dff3

cd3 <- cd2[order(cd), ]
cd3

dfb3.1 <- dfb2[order(abs(dfb[ , 1])), -3]
dfb3.1

dfb3.2 <- dfb2[order(abs(dfb[ , 2])), -2]
dfb3.2

dat2 <- Cars93
dat2 <- dat2[-c(59, 28), ]

out2 <- lm(Price ~ Horsepower, dat2)

x <- Cars93$Horsepower
y <- Cars93$Price

x2 <- x[-c(59, 28)]
y2 <- y[-c(59, 28)]

plot(x = x, y = y)
points(x = x[c(59, 28)], y = y[c(59, 28)], col = "red")
abline(a = coef(out1)[1], b = coef(out1)[2], col = "red")
abline(a = coef(out2)[1], b = coef(out2)[2], col = "blue")

summary(out1)
summary(out2)

plot(lev)

colVec <- rep("black", nrow(Cars93))
colVec[c(59, 28)] <- "red"

p1 <- gg0(x = x, y = y, points = FALSE) +
    geom_point(aes(colour = colVec), show.legend = FALSE, size = 2) +
    scale_colour_manual(values = c("black", "red")) +
    xlab("Horsepower") +
    ylab("Price")

p1 + geom_smooth(method = "lm", se = FALSE) +
geom_smooth(mapping = aes(x = x2, y = y2),
            method = "lm",
            se = FALSE,
            color = "red") +
scale_colour_manual(values = c("black", "red"))


?colour_scale_manual

plot(x = lev, y = rstudent(out1))


?covratio

AIC(out1)
BIC(out1)

?AIC

?extractAIC

?rstudent

lm.influence(out1)

?lm.influence


out1 <- lm(y ~ x)
out2 <- lm(y2 ~ x2)

summary(out1)$coefficients
summary(out2)$coefficients

summary(out1)[c("sigma", "r.squared", "fstatistic")]
summary(out2)[c("sigma", "r.squared", "fstatistic")]

nObs <- 10000

x <- rnorm(nObs)
z <- rnorm(nObs)

w <- x + z

plot(w, z)
plot(w, x)

cor(data.frame(w, x, z))

y <- 0.5 * w + 0.75 * x + 0.5 * z + rnorm(nObs, 0, 1.5)

out1 <- lm(y ~ x + w)
summary(out1)

vif(out1)

x <- rnorm(nObs)
w <- rnorm(nObs)

z <- x + w + rnorm(nObs, 0, 0.4)

y <- 0.5 * w + 0.75 * x + rnorm(nObs, 0, 1.5)

cor(data.frame(x, w, z))

dat1 <- data.frame(y, x, w, z)

ind <- sample(c(1 : nObs), replace = TRUE)
tmp <- dat1[ind, ]

out1 <- lm(y ~ x + w + z, data = tmp)
summary(out1)

vif(out1)

r2 <- 0.5

## Collinearity simulation:
clVec <- seq(0.05, 0.95, 0.1)
nReps <- 100

clList <- list()
for(cl in clVec) {
    rpList <- list()
    for(rp in 1 : nReps) {
        X <- rmvnorm(nObs, c(0, 0), matrix(c(1.0, cl, cl, 1.0), 2, 2))
        
        eta   <- X %*% matrix(c(0.5, 0.75))
        sigma <- (var(eta) / r2) - var(eta)
        
        y <- eta + rnorm(nObs, 0, sqrt(sigma))
        
        dat1 <- data.frame(y, X)
        colnames(dat1) <- c("y", "x1", "x2")
        
        out1 <- lm(y ~ x1 + x2, data = dat1)
        rpList[[rp]] <- coef(out1)
    }
    clList[[as.character(cl)]] <- do.call(rbind, rpList)
}

clMat <- do.call(rbind, clList)

iMat  <- matrix(clMat[ , 1], nrow = 100)
x1Mat <- matrix(clMat[ , 2], nrow = 100)
x2Mat <- matrix(clMat[ , 3], nrow = 100)

x2Frame <- data.frame(b2 = clMat[ , 2], cl = as.factor(rep(clVec, each = 100)))

p1 <- gg0(x = x2Frame$cl, y = x2Frame$b2, points = FALSE)
p1 + geom_boxplot() + xlab("Cor(X1, X2)") + ylab("Estimated Beta2")

## Multicollinearity simulation:
clVec <- seq(0.05, 0.95, 0.1)
nReps <- 100

clList <- list()
for(cl in clVec) {
    rpList <- list()
    for(rp in 1 : nReps) {
        X <- rmvnorm(nObs, c(0, 0), matrix(c(1.0, 0, 0, 1.0), 2, 2))
        
        etaZ   <- X %*% matrix(c(1, 1))
        sigmaZ <- (var(etaZ) / cl) - var(etaZ)
        
        z <- etaZ + rnorm(nObs, 0, sqrt(sigmaZ))
        
        XZ <- cbind(X, z)
        
        etaY   <- XZ %*% matrix(c(0.25, 0.5, 0.75))
        sigmaY <- (var(etaY) / r2) - var(etaY)

        y <- etaY + rnorm(nObs, 0, sqrt(sigmaY))

        dat1           <- data.frame(y, XZ)
        colnames(dat1) <- c("y", "x1", "x2", "z")
        
        out1 <- lm(y ~ x1 + x2 + z, data = dat1)
        rpList[[rp]] <- coef(out1)
    }
    clList[[as.character(cl)]] <- do.call(rbind, rpList)
}

clMat <- do.call(rbind, clList)

iMat  <- matrix(clMat[ , 1], nrow = 100)
x1Mat <- matrix(clMat[ , 2], nrow = 100)
x2Mat <- matrix(clMat[ , 3], nrow = 100)
zMat  <- matrix(clMat[ , 4], nrow = nReps)

boxplot(iMat)
boxplot(x1Mat)
boxplot(x2Mat)
boxplot(zMat)

zFrame <- data.frame(b3 = clMat[ , 4], cl = as.factor(rep(clVec, each = nReps)))

p1 <- gg0(x = zFrame$cl, y = zFrame$b3, points = FALSE)
p1 + geom_boxplot() + xlab("R Squared Z.{X1,X2}") + ylab("Estimated Beta3")


## Outlier checks:
out1 <- lm(MPG.highway ~ Horsepower + I(Horsepower^2), data = Cars93)
out2 <- lm(MPG.highway ~ Horsepower + I(Horsepower^2) + I(Horsepower^3),
           data = Cars93)

cd1 <- cooks.distance(out1)
rs1 <- rstudent(out1)
lv1 <- hatvalues(out1)

cd2 <- cooks.distance(out2)
rs2 <- rstudent(out2)
lv2 <- hatvalues(out2)

plot(cd1)
plot(rs1)
plot(lv1)

plot(cd2)
plot(rs2)
plot(lv2)

which.max(cd1)

dat1 <- Cars93[-39, ]

out2 <- lm(MPG.highway ~ Horsepower + I(Horsepower^2), data = dat1)

p1 <- gg0(x = predict(out2), y = resid(out2)) + geom_smooth(se = FALSE)
p1


out3 <- lm(MPG.highway ~ Horsepower + I(Horsepower^2) + I(Horsepower^3),
           data = dat1)

p1 <- gg0(x = predict(out3), y = resid(out3)) + geom_smooth(se = FALSE)
p1



colnames(iMat) <- colnames(x1Mat) <- colnames(x2Mat) <- names(clList)

boxplot(iMat)
boxplot(x1Mat)
boxplot(x2Mat)

vif(out1)

out2 <- lm(y ~ x + z)
summary(out2)

vif(out2)


cor(data.frame(x, w, z))

summary(lm(z ~ x + y))

























##### Example models
dDat <- readRDS("../data/diabetes.rds")

m1 <- mean(dDat$ldl)
s1 <- sd(dDat$ldl)

dDat$ldlLo  <- dDat$ldl - (m1 - s1)
dDat$ldlMid <- dDat$ldl - m1
dDat$ldlHi  <- dDat$ldl - (m1 + s1)

outLo  <- lm(bp ~ bmi*ldlLo, data = dDat)
outMid <- lm(bp ~ bmi*ldlMid, data = dDat)
outHi  <- lm(bp ~ bmi*ldlHi, data = dDat)

b0Lo <- coef(outLo)[1]
b1Lo <- coef(outLo)["bmi"]

b0Mid <- coef(outMid)[1]
b1Mid <- coef(outMid)["bmi"]

b0Hi <- coef(outHi)[1]
b1Hi <- coef(outHi)["bmi"]

x    <- seq(min(dDat$bmi), max(dDat$bmi), 0.1)
dat1 <- data.frame(x    = x,
                   yLo  = b0Lo + b1Lo * x,
                   yMid = b0Mid + b1Mid * x,
                   yHi  = b0Hi + b1Hi * x)

p1 <- ggplot(data = dDat, aes(x = bmi, y = bp)) +
    theme_classic() +
    theme(text = element_text(family = "Courier", size = 16))
p2 <- p1 + geom_point(colour = "gray") +
    geom_line(mapping = aes(x = x, y = yLo, colour = "Mean LDL - 1 SD"),
              data    = dat1,
              size    = 1.5) +
    geom_line(mapping = aes(x = x, y = yMid, colour = "Mean LDL"),
              data    = dat1,
              size    = 1.5) +
    geom_line(mapping = aes(x = x, y = yHi, colour = "Mean LDL + 1 SD"),
              data    = dat1,
              size    = 1.5) +
    xlab("BMI") +
    ylab("BP")

p2 + scale_colour_manual(name = "", values = c("Mean LDL" = "black",
                                               "Mean LDL - 1 SD" = "red",
                                               "Mean LDL + 1 SD" = "blue")
                         ) +
    theme(legend.justification = c(1, 0), legend.position = c(0.975, 0.025))

p2

p2 + guides()


p2 + guides(guide_legend())

?guides
?guide_legend

?geom_abline

out12 <- lm(bp ~ bmi * ldl, data = dDat)

data(socsupport)

out0 <- lm(BDI ~ tangiblesat, data = socsupport)
summary(out0)

out1 <- lm(BDI ~ tangiblesat * gender, data = socsupport)
summary(out1)

socsupport$gender2 <- relevel(socsupport$gender, ref = "male")
levels(socsupport$gender2)

out2 <- lm(BDI ~ tangiblesat * gender2, data = socsupport)
summary(out2)

out3 <- lm(BDI ~ tangiblesat + gender, data = socsupport)
summary(out3)

round(coef(out3), 2)

coef(out1)
coef(out3)

##### Plots:

p1 <- ggplot(data = socsupport, mapping = aes(x = tangiblesat, y = BDI, colour = gender)) +
    theme_classic() +
    theme(text = element_text(family = "courier", size = 16))

?scale_color_discrete

cols <- rep("blue", nrow(socsupport))
cols[socsupport$gender == "female"] <- "red"

p1 + geom_jitter(aes(colour = gender)) + scale_color_manual(values = c("red", "blue"))


?scale_colour_manual

p2 <- p1 + geom_jitter() +
    scale_colour_manual(name = "gender", values = c("red", "blue"))

p2 + geom_abline(slope     = coef(out1)["tangiblesat"],
                 intercept = coef(out1)[1],
                 colour    = "red",
                 size      = 1.5) +
    geom_abline(slope     = coef(out2)["tangiblesat"],
                intercept = coef(out2)[1],
                colour    = "blue",
                size      = 1.5)

p2 + geom_abline(slope     = coef(out3)["tangiblesat"],
                 intercept = coef(out3)[1],
                 colour    = "red",
                 size      = 1.5) +
    geom_abline(slope     = coef(out3)["tangiblesat"],
                intercept = (coef(out3)[1] + coef(out3)["gendermale"]),
                colour    = "blue",
                size      = 1.5)


dat1 <- readRDS("../data/diabetes.rds") 

dat1$age30  <- dat1$age - 30
dat1$ldl100 <- dat1$ldl - 100
dat1$hdl60  <- dat1$hdl - 60
dat1$bmi25  <- dat1$bmi - 25

out0 <- lm(bp ~ ldl + bmi, data = dat1)
summary(out0)

tmp0 <- visreg2d(fit       = out0,
                 x         = "bmi",
                 y         = "ldl",
                 plot.type = "persp",
                 col       = c("blue", "red"),
                 xlab      = "BMI",
                 ylab      = "LDL",
                 zlab      = "BP")

out1 <- lm(bp ~ ldl * bmi, data = dat1)
summary(out1)

tmp1 <- visreg2d(fit       = out1,
                 x         = "bmi",
                 y         = "ldl",
                 plot.type = "persp",
                 col       = c("blue", "red"),
                 xlab      = "BMI",
                 ylab      = "LDL",
                 zlab      = "BP")

z1 <- tmp1$z
x1 <- rep(tmp1$x, each = length(tmp1$y))
y1 <- rep(tmp1$y, length(tmp1$x))

ldlNorm <- with(dat1, (ldl - min(ldl)) / diff(range(ldl)))
bmiNorm <- with(dat1, (bmi - min(bmi)) / diff(range(bmi)))

ramp    <- colorRampPalette(c("red", "blue"))
n       <- 100
colsLdl <- ramp(n)[ldlNorm * (n - 1) + 1]
colsBmi <- ramp(n)[bmiNorm * (n - 1) + 1]

visFlag <- resid(out1) > 0

z2 <- z3 <- dat1$bp
z2[!visFlag] <- NA
z3[visFlag]  <- NA

rz   <- range(z1, dat1$bp)
m    <- 0.05
zLim <- c(rz[1] - m * diff(rz), rz[2] + m * diff(rz))

pdf(paste0(plotDir, "3d_data_plot.pdf"), family = "Courier")

cloud(x            = bp ~ ldl * bmi,
      data         = dat1,
      screen       = c(x = 290, y = 30, z = 10),
      scales       = list(col = "black", arrows = FALSE),
      xlab         = "LDL",
      ylab         = "BMI",
      zlab         = "BP",
      par.settings = list(axis.line = list(col = 0)),
      pch          = 20,
      col.point    = colsBmi,
      cex          = 2,
      zlim         = zLim)

dev.off()

myPanel <- function(x1, y1, z1, x2, y2, z2, z3, ...) {
    panel.cloud(x2, y2, z3, ...)
    panel.wireframe(x1, y1, z1, ...)
    panel.cloud(x2, y2, z2, ...)
}

pdf(paste0(plotDir, "response_surface_plot.pdf"), family = "Courier")

wireframe(x            = z1 ~ y1 * x1,
          drape        = FALSE,
          screen       = c(x = 290, y = 30, z = 10),
          scales       = list(col = "black", arrows = FALSE),
          xlab         = "LDL",
          ylab         = "BMI",
          zlab         = "BP",
          par.settings = list(axis.line = list(col = 0)),
          col.regions  = colors(), # Exploit bug to make wireframe transparent
          panel        = myPanel,
          x2           = dat1$ldl,
          y2           = dat1$bmi,
          z2           = z2,
          z3           = z3,
          pch          = 20,
          col.point    = colsBmi,
          cex          = 2,
          zlim         = zLim)

dev.off()


z1 <- tmp0$z
x1 <- rep(tmp0$x, each = length(tmp0$y))
y1 <- rep(tmp0$y, length(tmp0$x))

rz   <- range(z1, dat1$bp)
m    <- 0.05
zLim <- c(rz[1] - m * diff(rz), rz[2] + m * diff(rz))

pdf(paste0(plotDir, "response_surface_plot0.pdf"), family = "Courier")

wireframe(x            = z1 ~ y1 * x1,
          drape        = FALSE,
          screen       = c(x = 290, y = 30, z = 10),
          scales       = list(col = "black", arrows = FALSE),
          xlab         = "LDL",
          ylab         = "BMI",
          zlab         = "BP",
          par.settings = list(axis.line = list(col = 0)),
          col.regions  = colors(), # Exploit bug to make wireframe transparent
          panel        = myPanel,
          x2           = dat1$ldl,
          y2           = dat1$bmi,
          z2           = z2,
          z3           = z3,
          pch          = 20,
          col.point    = colsBmi,
          cex          = 2,
          zlim         = zLim)

dev.off()



dat2 <- dat1[ , c("ldl", "age", "bmi")]
    
colMeans(dat1)
apply(dat1, 2, sd)
apply(dat1, 2, range)

mean(dat1$bp)
sd(dat1$bp)

out0 <- lm(age ~ sexF, data = dat1)
summary(out0)

out1 <- lm(bp ~ age, data = dat1)
out2 <- lm(bp ~ age + bmi, data = dat1)

anova(out1, out2)

out3 <- lm(bp ~ age + ldl + hdl, data = dat1)
out4 <- lm(bp ~ age + ldl + hdl + bmi, data = dat1)

tab1 <- rbind(coef(out3)[-c(1, 2)], coef(out4)[-c(1, 2, 5)])
tab1

rownames(tab1) <- c("Chol. Only", "With BMI")
colnames(tab1) <- c("LDL", "HDL")
xTab1 <- xtable(tab1, align = c("r|", "c|", "c|"), digits = 2)
print(xTab1)


s0 <- summary(out0)
s1 <- summary(out1)
s2 <- summary(out2)

f0 <- s0$fstat

pf(f0[1], f0[2], f0[3], lower = FALSE)

fStat <- s1$fstatistic["value"]
tStat <- s1$coefficients["age", "t value"]

colnames(dat1)

summary(dat1)

ls(s1)

tStat - sqrt(fStat)

summary(out2)$r.squared - summary(out1)$r.squared
anova(out1, out2)

ls(summary(out2))

out0 <- lm(bp ~ age, data = dat1)
summary(out0)

out1 <- lm(bp ~ age + ldl + hdl, data = dat1)
summary(out1)

out2 <- lm(bp ~ age + ldl + hdl + bmi, data = dat1)
summary(out2)

anova(out1, out2)

out3 <- lm(bp ~ age + bmi, data = dat1)
summary(out3)

anova(out3, out2)

with(dat1, cor(ldl, hdl))

out2 <- lm(bp ~ bmi + age, data = dat1)
summary(out2)

out3 <- lm(bp ~ age + bmi + ldl + hdl, data = dat1)
summary(out3)

with(dat1, mean(bmi[age == 30]))

with(dat1, tapply(bmi, age, mean))

summary(lm(bp ~ bmi*ldl, data = dat1))


cor(dat1[ , c("ldl", "age", "bmi")])

anova(out3, out4)




#### F plots

??fd

?FDist

x <- seq(0, 5, 0.01)
fDat <- data.frame(x = x,
                   y = df(x, 5, 500)
                   )

p3 <- ggplot(data = fDat, mapping = aes(x = x, y = y)) +
    theme_classic() +
    theme(text = element_text(size = 16, family = "Courier")) +
    labs(x = "F", y = "Density", title = "Sampling distribution of F(5, 500)") +
    theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

p3 + geom_line()

plot(x = x, y = y, type = "l")

#### PPCOR Examples

with(mtcars, cor.test(hp, mpg))
with(mtcars, pcor.test(hp, mpg, wt))
with(mtcars, spcor.test(mpg, hp, wt))

out1 <- lm(mpg ~ hp, data = mtcars)
out2 <- lm(mpg ~ hp + wt, data = mtcars)

s1 <- summary(out1)
s2 <- summary(out2)

s2$r.sq - s1$r.sq

with(mtcars, spcor.test(mpg, wt, hp))["estimate"]^2




data(iris)

mDat              <- iris[ , c("Petal.Length", "Species")]
colnames(mDat)    <- c("sat", "majF")
levels(mDat$majF) <- c("law", "econ", "ds")
mDat$majN         <- as.numeric(mDat$maj)

saveRDS(mDat, paste0(dataDir, "major_data.rds"))

summary(lm(sat ~ majN, data = mDat))

mDat <- readRDS(paste0(dataDir, "major_data.rds"))


gender <- factor(sample(c("male", "female"), 10, TRUE))
male   <- as.numeric(model.matrix(~gender)[ , -1])

tab1 <- data.frame(gender, male)

drink <- factor(sample(c("coffee", "tea", "juice"), 10, TRUE))
codes <- model.matrix(~drink)[ , -1]
colnames(codes) <- c("juice", "tea")

drink
codes

xTab3 <- xtable(data.frame(drink, codes), digits = 0)
print(xTab3, booktabs = TRUE)

library(psych)
data(bfi)

colnames(bfi)

bfi$gender

codes[drink == "coffee", ] <- -1
codes

data(mtcars)
data(Cars93)

?mtcars
?Cars93

mtOpt <- model.matrix(~Cars93$Man.trans.avail)[ , -1]
drive <- model.matrix(~Cars93$DriveTrain)[ , -1]

colnames(drive) <- c("front", "rear")

mtOpt.ec <- mtOpt
mtOpt.ec[mtOpt == 0] <- -1

drive.ec <- drive
drive.ec[rowSums(drive) == 0, ] <- -1
colnames(drive.ec) <- c("front.ec", "rear.ec")

dat1 <- data.frame(price = Cars93$Price,
                   mtOpt,
                   mtOpt.ec,
                   drive,
                   drive.ec)

saveRDS(dat1, paste0(dataDir, "cars_data.rds"))

head(dat1)

out1 <- lm(Price ~ Man.trans.avail, data = Cars93)
summary(out1)

out1.1 <- lm(price ~ mtOpt, data = dat1)
summary(out1.1)

out2 <- lm(Price ~ DriveTrain, data = Cars93)
summary(out2)

out2.2 <- lm(price ~ front + rear, data = dat1)
summary(out2.2)

out3 <- lm(price ~ mtOpt.ec, data = dat1)
summary(out3)

y <- dat1$price
x <- dat1$mtOpt.ec
x[x == 1] <- 0.5
x[x == -1] <- -0.5

out3.2 <- lm(y ~ x)
summary(out3.2)

coef(out3)[1] - mean(dat1$price)

mean(tapply(dat1$price, dat1$mtOpt, mean))

colnames(Cars93)

with(Cars93, plot(Horsepower, MPG.city))

p1 <- ggplot(data = Cars93, mapping = aes(x = Horsepower, y = MPG.city)) +
    theme_classic()
p2 <- p1 + geom_point() +
    theme(text = element_text(family = "Courier", size = 16))

p2 + geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, colour = "red") +
    xlim(c(0, 350)) +
    theme(axis.line.y = element_line(colour = "white")) +
    geom_vline(xintercept = 0)

p2 + geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), se = FALSE)

p2 + geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4), se = FALSE)


Cars93$hpC <- with(Cars93, Horsepower - mean(Horsepower))

p1 <- ggplot(data = Cars93, mapping = aes(x = hpC, y = MPG.city)) +
    theme_classic()
p2 <- p1 + geom_point() +
    theme(text = element_text(family = "Courier", size = 16))

p22 <- p2 + geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, colour = "red") +
    geom_vline(xintercept = 0) +
    xlab("Horsepower") +
    ylab("MPG") +
    xlim(c(-100, 200))


p33 <- p22 + theme(axis.line.y = element_line(colour = "white"))

tmp <- lm(MPG.city ~ hpC + I(hpC^2), data = Cars93)

p33 + geom_abline(slope = coef(tmp)[2], intercept = coef(tmp)[1], colour = "purple") 


p2 + geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), se = FALSE)

p2 + geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4), se = FALSE)
