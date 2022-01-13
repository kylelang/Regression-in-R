### Title:    Stats & Methods Lab 4 Demonstration Script
### Author:   Kyle M. Lang
### Created:  2017-09-08
### Modified: 2020-09-24

rm(list = ls(all = TRUE))

install.packages("wec", repos = "http://cloud.r-project.org")

library(wec)                 # For weighted effects codes
source("studentFunctions.R") # For summary.cellMeans()

## Load the iris data
data(iris)

set.seed(235711)

## Sample 100 rows to unbalance group sizes:
iris <- iris[sample(1 : nrow(iris), 100), ]

###--------------------------------------------------------------------------###

### Factor Variables ###

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

###--------------------------------------------------------------------------###

### Creating Factor Variables ###

x <- sample(c(1, 2, 3), size = 100, replace = TRUE)
x
class(x)

## Don't use any labels:
y <- factor(x)
y
class(y)
levels(y)

## Arithmetic still doesn't work:
mean(y)

## Match labels to default levels:
z <- factor(x, labels = c("dog", "cat", "badger"))
z
table(x, z)

## Use a different labels -> levels mapping:
z2 <- factor(x, levels = c(2, 1, 3), labels = c("dog", "cat", "badger"))
z3 <- factor(x, labels = c("cat", "dog", "badger"))

## Compare:
all(z2 == z3)
all.equal(z2, z3)

levels(z2)
levels(z3)

table(z2, z3)
table(x, z2)
table(x, z3)

## Reorder factor levels:
z4 <- factor(x, levels = c(2, 1, 3))
levels(z4)
table(x, z4)

## We can also create empty factor levels:
z5 <- factor(x, levels = c(1, 2, 3, 4))
levels(z5)
table(x, z5)

## Be careful with 'numeric' factor labels:
z6 <- factor(x, labels = c(3, 2, 1))
levels(z6)
table(x, z6)
table(cast = as.numeric(z6), factor = z6)
table(cast = as.numeric(z6), raw = x)

## Safe way to cast factor with 'numeric' labels to a numeric vector:
x2 <- as.numeric(as.character(z6))
table(x, x2)
class(x2)

###--------------------------------------------------------------------------###

### Dummy Codes ###

## Use a factor variable as a predictor:
out1 <- lm(Petal.Length ~ Species, data = iris)
summary(out1)

## Check the contrasts:
contrasts(iris$Species)

## Change the reference group:
iris$Species2 <- relevel(iris$Species, ref = "virginica")

levels(iris$Species)
levels(iris$Species2)

## How are the contrasts affected:
contrasts(iris$Species)
contrasts(iris$Species2)

## Which carries through to the models:
out2 <- lm(Petal.Length ~ Species2, data = iris)
summary(out1)
summary(out2)

###--------------------------------------------------------------------------###

### Cell-Means Codes ###

out3 <- lm(Petal.Length ~ Species - 1, data = iris)
summary(out3)
summary.cellMeans(out3)

###--------------------------------------------------------------------------###

### Unweighted Effects Codes ###

## Use the 'contr.sum' function to create unweighted effects-coded contrasts:
?contr.sum

iris$Species3            <- iris$Species
contrasts(iris$Species3) <- contr.sum(levels(iris$Species3))
contrasts(iris$Species3)

## Use the fancy-pants Species factor:
out4 <- lm(Petal.Length ~ Species3, data = iris)
summary(out4)
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

out5 <- lm(Petal.Length ~ Species3, data = iris)
summary(out5)

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
out6 <- lm(Petal.Length ~ Species4, data = iris)
summary(out5)
summary(out6)

## To summarize:
iris$Species4            <- iris$Species
iris$Species4            <- changeOmitted(iris$Species4)
contrasts(iris$Species4) <- contr.sum(nlevels(iris$Species4))
iris$Species4            <- fixEcNames(iris$Species4)

contrasts(iris$Species)
contrasts(iris$Species3)
contrasts(iris$Species4)

###--------------------------------------------------------------------------###

### Weighted Effects Codes ###

## Use the 'contr.wec' function to create weighted effects-coded contrasts:
iris$Species5            <- iris$Species
contrasts(iris$Species5) <- contr.wec(iris$Species, omitted = "virginica")
contrasts(iris$Species5)

out7 <- lm(Petal.Length ~ Species5, data = iris)
summary(out7)

## Create contrast with a different reference level:
iris$Species6            <- iris$Species
contrasts(iris$Species6) <- contr.wec(iris$Species, omitted = "setosa")
contrasts(iris$Species6)

out8 <- lm(Petal.Length ~ Species6, data = iris)
summary(out8)
summary(out7)

###--------------------------------------------------------------------------###

### Reverting to Default "Treatment" Contrasts ###

tmp <- iris$Species6
contrasts(tmp)

## Dummy codes without names:
contrasts(tmp) <- contr.treatment(nlevels(tmp))
contrasts(tmp)

## Named dummy codes (default):
tmp            <- iris$Species6
contrasts(tmp) <- contr.treatment(levels(tmp))
contrasts(tmp)
contrasts(iris$Species)

###--------------------------------------------------------------------------###

### Testing the Effects of Grouping Factors ###

summary(out1)

## Test the effect of Species:
out0 <- lm(Petal.Length ~ 1, data = iris)
summary(out0)
anova(out0, out1)

## Test the partial effect of Species:
out9.1 <- lm(Petal.Length ~ Petal.Width, data = iris)
out9.2 <- update(out9.1, ". ~ . + Species")

summary(out9.1)
summary(out9.2)

anova(out9.1, out9.2)
