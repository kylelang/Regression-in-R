### Title:    Regression in R 3: Moderation
### Author:   Kyle M. Lang
### Created:  2016-04-04
### Modified: 2024-01-24

rm(list = ls(all = TRUE))


###-Preliminaries------------------------------------------------------------###

library(dplyr)     # For data manipulation
library(magrittr)  # For special pipes
library(rockchalk) # For interaction probing

## Load data:
dataDir <- "data/"

ginz <- readRDS(paste0(dataDir, "ginzberg.rds"))
bfi  <- readRDS(paste0(dataDir, "bfi_scored.rds"))

data(iris)

################################################################################
## PRACTICE PROBLEM 3.1
##
## a) Use the readRDS() function to load the "msq2.rds" dataset.
## b) Use the data() function to load the "cps3" and "leafshape" datasets from
##    the DAAG package.
##
################################################################################


###-Continuous Variable Moderation--------------------------------------------###

## Focal effect:
out0 <- lm(depression ~ fatalism, data = ginz)
summary(out0)

## Additive model:
out1 <- lm(depression ~ fatalism + simplicity, data = ginz)
summary(out1)

## Moderated model:
out2 <- lm(depression ~ fatalism * simplicity, data = ginz)
summary(out2)

################################################################################
## PRACTICE PROBLEM 3.2
##
## Use the "msq2" data to estimate the following model.
##
## Estimate a model that tests if the effect of Energetic Arousal (EA) on Tense
## Arousal (TA) varies as a function of Negative Affect (NegAff), after
## controlling for Positive Affect (PA).
##
################################################################################

################################################################################
## PRACTICE PROBLEM 3.3
##
## Use the results from the model you estimated in PP 3.2 to answer the
## following questions.
##
## a) What is the value of the parameter estimate that quantifies the effect of
##    Negative Affect on the Energetic Arousal -> Tense Arousal effect, after
##    controlling for Positive Affect?
## b) Does Negative Affect significantly moderate (at the alpha = 0.05 level)
##    the relationship between Energetic Arousal and Tense Arousal, after
##    controlling for Positive Affect?
## c) After controlling for Positive Affect, how does Negative Affect impact
##    the relationship between Energetic Arousal and Tense Arousal? Provide a
##    sentence interpreting the appropriate effect.
##
################################################################################


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

################################################################################
## PRACTICE PROBLEM 3.4
##
## a) Use the centering method to test the simple slopes of the model you
##    estimated in PP 3.2 at Negative Affect values of 0, 10, and 20.
## b) After controlling for Positive Affect, what is the simple slope of
##    Energetic Arousal on Tense Arousal when Negative Affect is 0.
## c) Is the simple slope you estimated in (b) statistically significant at
##    the alpha = 0.05 level?
## d) After controlling for Positive Affect, what is the simple slope of
##    Energetic Arousal on Tense Arousal when Negative Affect is 10.
## e) Is the simple slope you estimated in (d) statistically significant at
##    the alpha = 0.05 level?
## f) After controlling for Positive Affect, what is the simple slope of
##    Energetic Arousal on Tense Arousal when Negative Affect is 20.
## g) Is the simple slope you estimated in (f) statistically significant at
##    the alpha = 0.05 level?
##
################################################################################


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

################################################################################
## PRACTICE PROBLEM 3.5
##
## a) Use the 'rockchalk' package to test the same simple slopes you estimated
##    in PP 3.4.
## b) Do the results of the centering approach agree with the results from
##    'rockchalk'?
##
################################################################################

## When the moderator is continuous, the testSlopes() function also calculates
## the Johnson-Neyman region of significance:
testOut1$jn$roots
summary(out2)$coefficients

################################################################################
## PRACTICE PROBLEM 3.6
##
## Run a Johnson-Neyman analysis on the model you estimated in PP 3.2.
## - What values of Negative Affect produce significant simple slopes of
##   Energetic Arousal on Tense Arousal, after controlling for Positive Affect?
##
################################################################################


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
bfi %>%
    mutate(gender = relevel(gender, ref = "female")) %>%
    update(out2, data = .) %>%
    summary()

################################################################################
## PRACTICE PROBLEM 3.7
##
## Use the "cps3" data to estimate the following model.
##
## Estimate a model that tests if the effect of Years of Education on Real
## Earnings in 1975 is significantly moderated by being Hispanic, after
## controlling for Real Earnings in 1973.
##
## HINT: The Hispanic variable is not a factor. You may want to change that.
##
################################################################################

################################################################################
## PRACTICE PROBLEM 3.8
##
## Use the results of the model you estimated in PP 3.7 to answer the following
## questions.
##
## b) After controlling for 1974 Earnings, does being Hispanic significantly
##    affect the relationship between Years of Education and 1975 Earnings at
##    the alpha = 0.05 level
## c) After controlling for 1974 Earnings, does being Hispanic significantly
##    affect the relationship between Years of Education and 1975 Earnings at
##    the alpha = 0.01 level?
##
################################################################################

################################################################################
## PRACTICE PROBLEM 3.9
##
## Answer the following questions w.r.t. the model you estimated in PP 3.7.
##
## a) What is the simple slope of Years of Education on 1975 Earnings
##    (controlling for 1974 Earnings) for Non-Hispanic people?
## b) Is the simple slope from (a) statistically significant at the alpha = 0.05
##    level?
## c) What is the simple slope of Years of Education on 1975 Earnings
##    (controlling for 1974 Earnings) for Hispanic people?
## d) Is the simple slope from (c) statistically significant at the alpha = 0.05
##    level?
##
################################################################################

################################################################################
## PRACTICE PROBLEM 3.10
##
## Visualize the simple slopes from PP 3.9 in an appropriate way.
##
################################################################################


###-Nominal Categorical Moderators (G > 2)-----------------------------------###

## Moderated model:
out1 <- lm(Petal.Width ~ Sepal.Width * Species, data = iris)
summary(out1)

## Test for significant moderation:
out0 <- lm(Petal.Width ~ Sepal.Width + Species, data = iris)
summary(out0)

anova(out0, out1)

## Test different simple slopes by changing reference group:
out1.1 <- iris %>%
    mutate(Species = relevel(Species, ref = "virginica")) %>%
    update(out1, data = .)
out1.2 <- iris %>%
    mutate(Species = relevel(Species, ref = "versicolor")) %>%
    update(out1, data = .)

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

################################################################################
## PRACTICE PROBLEM 3.11
##
## Use the "leafshape" data to answer the following questions.
##
## a) What are the levels of the "location" factor?
## b) What are the group sizes for the "location" factor?
##
################################################################################

################################################################################
## PRACTICE PROBLEM 3.12
##
## Use the "leafshape" data to estimate the following model.
##
## Estimate a model that tests if the effect of Leaf Width on Leaf Length
## differs significantly between Locations.
##
################################################################################

################################################################################
## PRACTICE PROBLEM 3.13
##
## Use the results of the model you estimated in PP 3.12 to answer the following
## questions.
##
## a) Does the effect of Leaf Width on Leaf Length differ significantly
##    (alpha = 0.05) between Locations?
## b) What is the value of the test statistic that you used to answer (a)?
##
################################################################################

################################################################################
## PRACTICE PROBLEM 3.14
##
## Answer the following questions w.r.t. the model you estimated in PP 3.12.
##
## a) What is the simple slope of Leaf Width on Leaf Length in Sabah?
## b) Is the simple slope you reported in (a) significant at the alpha = 0.05
##    level?
## c) What is the simple slope of Leaf Width on Leaf Length in Panama?
## d) Is the simple slope you reported in (c) significant at the alpha = 0.05
##    level?
## e) What is the simple slope of Leaf Width on Leaf Length in South Queensland?
## f) Is the simple slope you reported in (e) significant at the alpha = 0.05
##    level?
## g) In which Location is the effect of Leaf Width on Leaf Length strongest?
## h) What caveat might you want to place on the conclusion reported in (g)?
##    - HINT: Consider your answers for PP 3.11.
##
################################################################################


###-END----------------------------------------------------------------------###
