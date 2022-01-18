### Title:    Regression in R: Lab 4 Demonstration Script
### Author:   Kyle M. Lang
### Created:  2016-04-04
### Modified: 2022-01-18

rm(list = ls(all = TRUE))

## Install the new packages we'll need:
install.packages("rockchalk", repos = "http://cloud.r-project.org")

library(dplyr)     # For data manipulation
library(magrittr)  # For special pipes
library(rockchalk) # For interaction probing

## Load data:
dataDir <- "../../data/"

ginz <- readRDS(paste0(dataDir, "ginzberg.rds"))
bfi  <- readRDS(paste0(dataDir, "bfi_scored.rds"))

data(iris)


###-Contiuous Variable Moderation--------------------------------------------###

## Focal effect:
out0 <- lm(depression ~ fatalism, data = ginz)
summary(out0)

## Additive model:
out1 <- lm(depression ~ fatalism + simplicity, data = ginz)
summary(out1)

## Moderated model:
out2 <- lm(depression ~ fatalism * simplicity, data = ginz)
summary(out2)


###-Probing via Centering----------------------------------------------------###

## Center 'simplicity' on Mean & Mean +/- 1SD
m <- mean(ginz$simplicity)
s <- sd(ginz$simplicity)

ginz %<>% mutate(simpLo = simplicity - (m - s),
                 simpMid = simplicity - m,
                 simpHi = simplicity - (m + s)
                 )

## Check the results:
head(ginz)

## Test SS at Mean - 1SD:
out2.1 <- lm(depression ~ fatalism * simpLo, data = ginz)
summary(out2.1)

## Test SS at Mean:
out2.2 <- lm(depression ~ fatalism * simpMid, data = ginz)
summary(out2.2)

## Test SS for Mean + 1SD:
out2.3 <- lm(depression ~ fatalism * simpHi, data = ginz)
summary(out2.3)


###-Probing via the 'rockchalk' Package--------------------------------------###

## First we use 'plotSlopes' to estimate the simple slopes:
plotOut1 <- plotSlopes(out2,
                       plotx      = "fatalism",
                       modx       = "simplicity",
                       modxVals   = "std.dev",
                       plotPoints = TRUE)

## We can also get simple slopes at the quartiles of simplicity's distribution:
plotOut2 <- plotSlopes(out2,
                       plotx      = "fatalism",
                       modx       = "simplicity",
                       modxVals   = "quantile",
                       plotPoints = TRUE)

## Or we can manually pick some values:
range(ginz$simplicity)
plotOut3 <- plotSlopes(out2,
                       plotx      = "fatalism",
                       modx       = "simplicity",
                       modxVals   = seq(0.5, 2.5, 0.5),
                       plotPoints = TRUE)

## Test the simple slopes via the 'testSlopes' function:
testOut1 <- testSlopes(plotOut1)
ls(testOut1)
testOut1$hypotests

testOut2 <- testSlopes(plotOut2)
testOut2$hypotests

testOut3 <- testSlopes(plotOut3)
testOut3$hypotests


###-Binary Categorical Moderators--------------------------------------------###

## Focal effect:
out0 <- lm(neuro ~ agree, data = bfi)
summary(out0)

## Additive model:
out1 <- lm(neuro ~ agree + gender, data = bfi)
summary(out1)

## Moderated model:
out2 <- lm(neuro ~ agree * gender, data = bfi)
summary(out2)

## Test 'female' simple slope by changing reference group:
bfi$gender2 <- relevel(bfi$gender, ref = "female")

out2.1 <- lm(neuro ~ agree * gender2, data = bfi)
summary(out2.1)


###-Nominal Categorical Moderators (G > 2)-----------------------------------###

## Moderated model:
out1 <- lm(Petal.Width ~ Sepal.Width * Species, data = iris)
summary(out1)

## Test for significant moderation:
out0 <- lm(Petal.Width ~ Sepal.Width + Species, data = iris)
summary(out0)

anova(out0, out1)

## Test different simple slopes by changing reference group:
iris$Species2 <- relevel(iris$Species, ref = "virginica")
iris$Species3 <- relevel(iris$Species, ref = "versicolor")

out1.1 <- lm(Petal.Width ~ Sepal.Width * Species2, data = iris)
out1.2 <- lm(Petal.Width ~ Sepal.Width * Species3, data = iris)

summary(out1)
summary(out1.1)
summary(out1.2)

## Do the same test using 'rockchalk':
plotOut1 <- plotSlopes(model      = out1,
                       plotx      = "Sepal.Width",
                       modx       = "Species",
                       plotPoints = FALSE)

testOut1 <- testSlopes(plotOut1)
testOut1$hypotests


###-END----------------------------------------------------------------------###
