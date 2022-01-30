### Title:    Regression in R: Lab 2 Demonstration Script
### Author:   Kyle M. Lang
### Created:  2017-09-08
### Modified: 2022-01-30

rm(list = ls(all = TRUE))

library(wec) # For weighted effects codes

## Load the iris data
data(iris)

set.seed(235711)

## Sample 100 rows to unbalance group sizes:
iris <- iris[sample(1 : nrow(iris), 100), ]


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

## OR ##

## Define a function to automatically fix EC names:
fixEcNames <- function(x) {
    tmp                    <- contrasts(x)
    colnames(contrasts(x)) <- rownames(tmp)[rowSums(tmp) > 0]
    x
}

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

## Define a function to automatically change the omitted group:
changeOmitted <- function(x) relevel(x, ref = levels(x)[nlevels(x)])

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


###-END----------------------------------------------------------------------###
