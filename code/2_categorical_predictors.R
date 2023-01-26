### Title:    Regression in R 2: Categorical Predictors
### Author:   Kyle M. Lang
### Created:  2017-09-08
### Modified: 2023-01-26

rm(list = ls(all = TRUE))


###-Preliminaries------------------------------------------------------------###

library(wec) # For weighted effects codes

## Define some helper functions
source("code/support/helper_functions.R")

## Load the iris data
data(iris)

set.seed(235711)

## Sample 100 rows to unbalance group sizes:
iris <- iris[sample(1:nrow(iris), 100), ]

################################################################################
## PRACTICE PROBLEM 2.1
##
## Use the data() function to load the "bfi" dataset from the psychTools 
## and the "BMI" dataset from the wec package.
##
################################################################################


###-Factor Variables---------------------------------------------------------###

## Look at the 'Species' factor:
iris$Species
is.factor(iris$Species)

str(iris$Species)
summary(iris$Species)

## Factors have special attributes:
attributes(iris$Species)
attributes(iris$Petal.Length)
attributes(iris)

## Factors have labeled levels:
levels(iris$Species)
nlevels(iris$Species)

## Factors are not numeric variables:
mean(iris$Species)
var(iris$Species)
iris$Species - iris$Species

################################################################################
## PRACTICE PROBLEM 2.2
##
## Use the "bfi" data to complete the following tasks.
##
## a) Refer to the help file of the "bfi" dataset to find the correct levels for
##    the "gender" and "education" variables.
## b) Create factors for the "gender" and "education" variables with sensible
##    sets of labels for the levels.
##
################################################################################

################################################################################
## PRACTICE PROBLEM 2.3
##
## Use the "bfi" data to complete the following tasks.
##
## a) How many women in this sample have graduate degrees?
## b) What is the most frequently reported level of educational attainment among
##    men in this sample?
##
################################################################################


###-Dummy Codes--------------------------------------------------------------###

## Use a factor variable as a predictor:
out1 <- lm(Petal.Length ~ Species, data = iris)
summary(out1)

## Check the contrasts:
contrasts(iris$Species)

## Change the reference group:
iris$Species2 <- relevel(iris$Species, ref = "virginica")

levels(iris$Species)
levels(iris$Species2)

## How are the contrasts affected?
contrasts(iris$Species)
contrasts(iris$Species2)

## Which carries through to the models:
out2 <- lm(Petal.Length ~ Species2, data = iris)
summary(out1)
summary(out2)

################################################################################
## PRACTICE PROBLEM 2.4
##
## Use the "BMI" data to complete the following tasks.
##
## a) How many levels does the "education" factor have?
## b) What is the reference level of the "sex" factor?
## c) What is the reference level of the "education" factor?
##
################################################################################

################################################################################
## PRACTICE PROBLEM 2.5
##
## Use the "BMI" data to complete the following tasks.
##
## a) Run a linear regression model wherein "BMI" is predicted by dummy-coded
##    versions of "sex" and "education".
##    - Set the reference group to "male" for "sex".
##    - Set the reference group to "highest" for "education".
## b) Is there a significant effect (at alpha = 0.05) of "sex" on "BMI" after
##    controlling for "education"?
## c) According to the model from (a), what is the expected BMI for males in
##    the highest education group?
##
################################################################################


###-Unweighted Effects Codes-------------------------------------------------###

## Use the contr.sum() function to create contrasts for unweighted effects codes
?contr.sum

iris$Species3            <- iris$Species
contrasts(iris$Species3) <- contr.sum(levels(iris$Species3))
contrasts(iris$Species3)

## Use the fancy-pants Species factor:
out3 <- lm(Petal.Length ~ Species3, data = iris)
summary(out3)
contrasts(iris$Species3)

## How about some better names?
colnames(contrasts(iris$Species3)) <- c("setosa", "versicolor")
contrasts(iris$Species3)

## Or use the fixEcNames() function defined in helper_functions.R to
## automatically achieve the same effect:
iris$Species3 <- fixEcNames(iris$Species3)
contrasts(iris$Species3)

out4 <- lm(Petal.Length ~ Species3, data = iris)
summary(out4)

## Change the omitted group:
iris$Species4 <- iris$Species
levels(iris$Species4)
iris$Species4 <- relevel(iris$Species4, ref = "virginica")

levels(iris$Species)
levels(iris$Species4)

## This won't work:
tmp <- relevel(iris$Species, ref = "versicolor")

levels(iris$Species)
levels(tmp)

## The above procedure is implemented in the changeOmitted() function defined
## in the helper_functions.R script:
tmp <- changeOmitted(iris$Species)

levels(iris$Species)
levels(tmp)

## Update the contrasts attribute:
contrasts(iris$Species4)
contrasts(iris$Species4) <- contr.sum(nlevels(iris$Species4))
contrasts(iris$Species4)

## Give some good names:
iris$Species4 <- fixEcNames(iris$Species4)
contrasts(iris$Species4)

## Use the new factor:
out5 <- lm(Petal.Length ~ Species4, data = iris)
summary(out4)
summary(out5)

## To summarize:
iris$Species4            <- iris$Species
iris$Species4            <- changeOmitted(iris$Species4)
contrasts(iris$Species4) <- contr.sum(nlevels(iris$Species4))
iris$Species4            <- fixEcNames(iris$Species4)

contrasts(iris$Species)
contrasts(iris$Species3)
contrasts(iris$Species4)

################################################################################
## PRACTICE PROBLEM 2.6
##
## Use the "BMI" data to estimate the following model.
##
## Regress "BMI" onto an unweighted effects-coded representation of "education"
## and a dummy-coded representation of "childless".
## - Adjust the contrasts attribute of the "education" factor to implement the
##   unweighted effects coding.
##
################################################################################

################################################################################
## PRACTICE PROBLEM 2.7
##
## Change the reference group (i.e., the omitted group) for the unweighted
## effects codes that you implemented in PP 2.6, and rerun the model regressing
## "BMI" onto "education" and "childless".
##
################################################################################

################################################################################
## PRACTICE PROBLEM 2.8
##
## Use the results from the models you estimated in PP 2.6 and PP 2.7 to answer
## the following questions.
##
## a) What is the expected BMI (averaged across education groups) for people
##    with children?
## b) What is the expected difference in BMI between the most highly educated
##    group and the average BMI across education groups, after controlling for
##    childlessness?
## c) Is the difference you reported in (b) significantly different from zero,
##    at the alpha = 0.05 level?
## d) What is the expected difference in BMI between the middle education group
##    and the average BMI across education groups, after controlling for
##    childlessness?
## e) Is the difference you reported in (d) significantly different from zero,
##    at the alpha = 0.05 level?
##
################################################################################


###-Weighted Effects Codes---------------------------------------------------###

## Use the wec::contr.wec() function to create weighted effects-coded contrasts:
iris$Species5            <- iris$Species
contrasts(iris$Species5) <- contr.wec(iris$Species, omitted = "virginica")
contrasts(iris$Species5)

out6 <- lm(Petal.Length ~ Species5, data = iris)
summary(out6)

## Create contrast with a different reference level:
iris$Species6            <- iris$Species
contrasts(iris$Species6) <- contr.wec(iris$Species, omitted = "setosa")
contrasts(iris$Species6)

out7 <- lm(Petal.Length ~ Species6, data = iris)
summary(out7)
summary(out6)

################################################################################
## PRACTICE PROBLEM 2.9
##
## Use the "BMI" data to estimate the following model.
##
## Regress "BMI" onto a weighted effects-coded representation of "education" and
## a dummy-coded representation of "sex".
## - Adjust the contrasts attribute of the "education" factor to implement the
##   weighted effects coding.
##
################################################################################

################################################################################
## PRACTICE PROBLEM 2.10
##
## Change the reference group (i.e., the omitted group) for the weighted effects
## codes that you implemented in PP 2.9, and rerun the model regressing "BMI"
## onto "education" and "sex".
##
################################################################################

################################################################################
## PRACTICE PROBLEM 2.11
##
## Use the results from the models you estimated in PP 2.9 and PP 2.10 to answer
## the following questions.

## a) What is the average BMI for females?
## b) What is the expected difference in BMI between the least educated group
##    and the average BMI, after controlling for sex?
## c) Is the difference you reported in (b) significantly different from zero,
##    at the alpha = 0.01 level?
## d) What is the expected difference in BMI between the most highly educated
##    group and the average BMI, after controlling for sex?
## e) Is the difference you reported in (d) significantly different from zero,
##    at the alpha = 0.01 level?
##
################################################################################


###-Reverting to Default "Treatment" Contrasts-------------------------------###

contrasts(iris$Species6)

## Use the contr.treatment() function to get back to dummy codes (default):
contrasts(iris$Species6) <- contr.treatment(levels(iris$Species6))

contrasts(iris$Species6)
contrasts(iris$Species)


###-Testing the Effects of Grouping Factors----------------------------------###

summary(out1)

## Test the effect of Species:
out0 <- lm(Petal.Length ~ 1, data = iris)

summary(out0)
anova(out0, out1)

## Test the partial effect of Species:
out8.1 <- lm(Petal.Length ~ Petal.Width, data = iris)
out8.2 <- update(out8.1, ". ~ . + Species")

summary(out8.1)
summary(out8.2)

anova(out8.1, out8.2)

################################################################################
## PRACTICE PROBLEM 2.12
##
## Use the "BMI" data to answer the following questions.
##
## a) Does education level explain a significant proportion of variance in BMI,
##    above and beyond sex?
## b) What is the value of the test statistic that you used to answer (a)?
##
################################################################################


###-END----------------------------------------------------------------------###

