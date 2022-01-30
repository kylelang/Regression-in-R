### Title:    Regression in R: Lab 2 Practice Script
### Author:   Kyle M. Lang
### Created:  2018-09-24
### Modified: 2022-01-30


###-Overview-----------------------------------------------------------------###

## You will practice fitting MLR models with categorical predictor variables.

## You will need the built-in R datasets "bfi" (from the psychTools package) and
## "BMI" (from the wec) package.


###-Preliminaries------------------------------------------------------------###

## 1) Use the library() function to load the "wec" package.

## 2) Use the data() function to load the "bfi" and "BMI" datasets.


###-Factors------------------------------------------------------------------###

### Use the "bfi" data to complete the following:
### -- You may ignore any missing data, for the purposes of these exercises
###    (although you should never do so in a real data analysis).

## 1) Refer to the help file of the "bfi" dataset to find the correct levels for
##    the "gender" and "education" variables.

## 2) Create factors for the "gender" and "education" variables with sensible
##    sets of labels for the levels.

## 3) How many women in this sample have graduate degrees?

## 4) What is the most frequently reported level of educational attainment among
##    men in this sample?


###-Dummy Codes--------------------------------------------------------------###

### Use the "BMI" data to complete the following:

## 1) How many levels does the "education" factor have?

## 2a) What is the reference level of the "sex" factor?
## 2b) What is the reference level of the "education" factor?

## 3a) Run a linear regression model wherein "BMI" is predicted by dummy-coded
##     "sex" and "education".
##     -- Set the reference group to "male" for the "sex" factor
##     -- Set the reference group to "highest" for the "education" factor
## 3b) Is there a significant effect (at alpha = 0.05) of "sex" on "BMI" after
##     controlling for "education"?
## 3c) According to the model from (3a), what is the expected BMI for males in
##     the highest education group?


###-Unweighted Effects Codes-------------------------------------------------###

### Use the "BMI" data to complete the following:

## 1) Regress "BMI" onto an unweighted effects-coded representation of
##    "education" and a dummy-coded representation of "childless".
##    -- Adjust the contrasts attribute of the "education" factor to implement
##       the unweighted effects coding.

## 2) Change the reference group (i.e., the omitted group) for the unweighted
##    effects codes that you implemented in (1), and rerun the model regressing
##    "BMI" onto "education" and "childless".

### Use the results of the models you estimated in (1) and (2) to answer the
### following questions.

## 3a) What is the expected BMI (averaged across education groups) for people
##     with children?
## 3b) What is the expected difference in BMI between the most highly educated
##     group and the average BMI across education groups, after controlling for
##     childlessness?
## 3c) Is the difference you reported in (3b) significantly different from zero,
##     at the alpha = 0.05 level?
## 3d) What is the expected difference in BMI between the middle education
##     group and the average BMI across education groups, after controlling for
##     childlessness?
## 3e) Is the difference you reported in (3d) significantly different from zero,
##     at the alpha = 0.05 level?


###-Weighted Effects Codes---------------------------------------------------###

### Use the "BMI" data to complete the following:

## 1) Regress "BMI" onto a weighted effects-coded representation of "education"
##    and a dummy-coded representation of "sex".
##    -- Adjust the contrasts attribute of the "education" factor to implement
##       the weighted effects coding.

## 2) Change the reference group (i.e., the omitted group) for the weighted
##    effects codes that you implemented in (1), and rerun the model regressing
##    "BMI" onto "education" and "sex".

### Use the results of the models you estimated in (1) and (2) to answer the
### following questions.

## 3a) What is the average BMI for females?
## 3b) What is the expected difference in BMI between the least educated group
##     and the average BMI, after controlling for sex?
## 3c) Is the difference you reported in (3b) significantly different from zero,
##     at the alpha = 0.01 level?
## 3d) What is the expected difference in BMI between the most highly educated
##     group and the average BMI, after controlling for sex?
## 3e) Is the difference you reported in (3d) significantly different from zero,
##     at the alpha = 0.01 level?

## 4a) Does education level explain a significant proportion of variance in BMI,
##     above and beyond sex?
## 4b) What is the value of the test statistic that you used to answer (4a)?


###-END----------------------------------------------------------------------###
