### Title:    Suggested Solutions 2: Categorical Predictors
### Author:   Kyle M. Lang
### Created:  2018-09-24
### Modified: 2023-01-26

library(wec)
library(magrittr)
library(dplyr)

source("code/support/helper_functions.R")


###-Preliminaries------------------------------------------------------------###

## 2.1) Use the data() function to load the "bfi" dataset from the psychTools 
##      and the "BMI" dataset from the wec package.

data(bfi, package = "psychTools")
data(BMI)


###-Factors------------------------------------------------------------------###

## 2.2a) Refer to the help file of the "bfi" dataset to find the correct levels
##       for the "gender" and "education" variables.

?psychTools::bfi

## 2.2b) Create factors for the "gender" and "education" variables with sensible
##       sets of labels for the levels.


bfi %<>% mutate(gender = factor(gender,
                                levels = c(1, 2),
                                labels = c("male", "female")
                                ),
                education = factor(education,
                                   levels = 1:5,
                                   labels = c("some_hs",
                                              "hs_grad",
                                              "some_college",
                                              "college_grad",
                                              "graduate_degree")
                                   )
                )

###--------------------------------------------------------------------------###

## 2.3a) How many women in this sample have graduate degrees?

bfi %$% table(gender, education)["female", "graduate_degree"]

## 2.3b) What is the most frequently reported level of educational attainment
##       among men in this sample?

bfi %$% table(gender, education)["male", ] %>% which.max() %>% names()


###-Dummy Codes--------------------------------------------------------------###

## 2.4a) How many levels does the "education" factor have?

nlevels(BMI$education)

## 2.4b) What is the reference level of the "sex" factor?

levels(BMI$sex)[1]

## 2.4c) What is the reference level of the "education" factor?

levels(BMI$education)[1]

###--------------------------------------------------------------------------###

## 2.5a) Run a linear regression model wherein "BMI" is predicted by dummy-coded
##       versions of "sex" and "education".
##       - Set the reference group to "male" for "sex".
##       - Set the reference group to "highest" for "education".

out1 <- BMI %>%
    mutate(education = relevel(education, ref = "highest")) %$%
    lm(BMI ~ sex + education)
summary(out1)

## 2.5b) Is there a significant effect (at alpha = 0.05) of "sex" on "BMI" after
##       controlling for "education"?

isSig(out1, "sexfemale")

## 2.5c) According to the model from (a), what is the expected BMI for males in
##       the highest education group?

coef(out1)["(Intercept)"]


###-Unweighted Effects Codes-------------------------------------------------###

## 2.6) Regress "BMI" onto an unweighted effects-coded representation of
##      "education" and a dummy-coded representation of "childless".
##      - Adjust the contrasts attribute of the "education" factor to implement
##        the unweighted effects coding.

contrasts(BMI$education) <- contr.sum(levels(BMI$education))
BMI$education            <- fixEcNames(BMI$education)

out3 <- lm(BMI ~ education + childless, data = BMI)
summary(out3)

###--------------------------------------------------------------------------###

## 2.7) Change the reference group (i.e., the omitted group) for the unweighted
##      effects codes that you implemented in (1), and rerun the model
##      regressing "BMI" onto "education" and "childless".

BMI$education            <- changeOmitted(BMI$education)
contrasts(BMI$education) <- contr.sum(levels(BMI$education))
BMI$education            <- fixEcNames(BMI$education)

out4 <- lm(BMI ~ education + childless, data = BMI)
summary(out4)

###--------------------------------------------------------------------------###

## 2.8a) What is the expected BMI (averaged across education groups) for people
##       with children?

coef(out4)["(Intercept)"]

## 2.8b) What is the expected difference in BMI between the most highly educated
##       group and the average BMI across education groups, after controlling
##       for childlessness?

coef(out4)["educationhighest"]

## 2.8c) Is the difference you reported in (b) significantly different from
##       zero, at the alpha = 0.05 level?

isSig(out4, "educationhighest")

## 2.8d) What is the expected difference in BMI between the middle education
##       group and the average BMI across education groups, after controlling
##       for childlessness?

coef(out3)["educationmiddle"]

## 2.8e) Is the difference you reported in (d) significantly different from
##       zero, at the alpha = 0.05 level?

isSig(out3, "educationmiddle")


###-Weighted Effects Codes---------------------------------------------------###

## 2.9) Regress "BMI" onto a weighted effects-coded representation of
##      "education" and a dummy-coded representation of "sex".
##      - Adjust the contrasts attribute of the "education" factor to implement
##        the weighted effects coding.

BMI$sex                  <- relevel(BMI$sex, ref = "female")
contrasts(BMI$education) <- contr.wec(BMI$education, omitted = "lowest")

out5 <- lm(BMI ~ sex + education, data = BMI)
summary(out5)

###--------------------------------------------------------------------------###

## 2.10) Change the reference group (i.e., the omitted group) for the weighted
##       effects codes that you implemented in (1), and rerun the model
##       regressing "BMI" onto "education" and "sex".

contrasts(BMI$education) <- contr.wec(BMI$education, omitted = "highest")

out6 <- lm(BMI ~ sex + education, data = BMI)
summary(out6)

###--------------------------------------------------------------------------###

## 2.11a) What is the average BMI for females?

coef(out5)["(Intercept)"]

## 2.11b) What is the expected difference in BMI between the least educated
##        group and the average BMI, after controlling for sex?

coef(out6)["educationlowest"]

## 2.11c) Is the difference you reported in (b) significantly different from
##        zero, at the alpha = 0.01 level?

isSig(out6, "educationlowest", alpha = 0.01)

## 2.11d) What is the expected difference in BMI between the most highly
##        educated group and the average BMI, after controlling for sex?

coef(out5)["educationhighest"]

## 2.11e) Is the difference you reported in (d) significantly different from
##        zero, at the alpha = 0.01 level?

isSig(out5, "educationhighest", alpha = 0.01)


###-Testing the Effects of Grouping Factors----------------------------------###

## 2.12a) Does education level explain a significant proportion of variance in
##        BMI, above and beyond sex?

out7 <- update(out6, ". ~ . - education")
summary(out7)

av7  <- anova(out7, out6)
av7

isSig(av7)

## 2.12b) What is the value of the test statistic that you used to answer (a)?

av7[2, "F"]


###-END----------------------------------------------------------------------###
