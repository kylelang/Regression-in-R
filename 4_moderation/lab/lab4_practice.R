### Title:    Regression in R: Lab 4 Practice Script
### Author:   Kyle M. Lang
### Created:  2018-09-24
### Modified: 2022-01-18


###-Overview-----------------------------------------------------------------###

## You will practice using MLR models for moderation analysis.

## You will need the "msq2.rds" data and the built-in R datasets "cps3" and
## "leafshape" (from the DAAG package). The "msq2.rds" dataset is available in
## the "data" directory for this course.


###-Preliminaries------------------------------------------------------------###

## 1) Use the "install.packages" function to install the "rockchalk" and "DAAG"
##    packages.

## 2) Use the "library" function to load the "rockchalk" and "DAAG" packages.

## 3) Use the "readRDS" function to load the "msq2.rds" dataset into your
##    workspace.

## 4) Use the "data" function to load the "cps3" and "leafshape" datasets into
##    your workspace.


###-Continuous Variable Moderation-------------------------------------------###

### Use the "msq2" data to complete the following:

## 1a) Estimate a model that tests if the effect of Energetic Arousal (EA) on
##     Tense Arousal (TA) varies as a function of Negative Affect (NegAff),
##     after controlling for Positive Affect (PA).
## 1b) What is the value of the parameter estimate that quantifies the effect of
##     Negative Affect on the Energetic Arousal -> Tense Arousal effect, after
##     controlling for Positive Affect?
## 1c) Does Negative Affect significantly moderate (at the alpha = 0.05 level)
##     the relationship between Energetic Arousal and Tense Arousal, after
##     controlling for Positive Affect?
## 1d) After controlling for Positive Affect, how does Negative Affect impact
##     the relationship between Energetic Arousal and Tense Arousal? Provide a
##     sentence interpreting the appropriate effect.

## 2a) Use the centering method to test the simple slopes of the model you
##     estimated in (1a) at Negative Affect values of 0, 10, and 20.
## 2b) After controlling for Positive Affect, what is the simple slope of
##     Energetic Arousal on Tense Arousal when Negative Affect is 0.
## 2c) Is the simple slope you estimated in (2b) statistically significant at
##     the alpha = 0.05 level?
## 2d) After controlling for Positive Affect, what is the simple slope of
##     Energetic Arousal on Tense Arousal when Negative Affect is 10.
## 2e) Is the simple slope you estimated in (2d) statistically significant at
##     the alpha = 0.05 level?
## 2f) After controlling for Positive Affect, what is the simple slope of
##     Energetic Arousal on Tense Arousal when Negative Affect is 20.
## 2g) Is the simple slope you estimated in (2f) statistically significant at
##     the alpha = 0.05 level?

## 3a) Use the 'rockchalk' package to test the same simple slopes you estimated
##     in (2a).
## 3b) Do the results of the centering approach agree with the results from
##     'rockchalk'?


###-Binary Categorical Moderators--------------------------------------------###

### Use the "cps3" data to complete the following:

## 1a) Estimate a model that tests if the effect of Years of Education on Real
##     Earnings in 1975 is significantly moderated by being Hispanic, after
##     controlling for Real Earnings in 1974.
##     HINT: The Hispanic variable is not a factor. You may want to change that.
## 1b) After controlling for 1974 Earnings, does being Hispanic significantly
##     affect the relationship between Years of Education and 1975 Earnings at
##     the alpha = 0.05 level
## 1c) After controlling for 1974 Earnings, does being Hispanic significantly
##     affect the relationship between Years of Education and 1975 Earnings at
##     the alpha = 0.01 level?

## 2a) What is the simple slope of Years of Education on 1975 Earnings
##     (controlling for 1974 Earnings) for Non-Hispanic people?
## 2b) Is the simple slope from (2a) statistically significant at the
##     alpha = 0.05 level?
## 2c) What is the simple slope of Years of Education on 1975 Earnings
##     (controlling for 1974 Earnings) for Hispanic people?
## 2d) Is the simple slope from (2c) statistically significant at the
##     alpha = 0.05 level?
## 2e) Visualize the simple slopes compute above in an appropriate way.


###-Nominal Categorical Moderators-------------------------------------------###

### Use the "leafshape" data to complete the following:

## 1a) What are the levels of the "location" factor?
## 1b) What are the group sizes for the "location" factor?

## 2a) Estimate a model that tests if the effect of Leaf Width on Leaf Length
##     differs significantly between Locations.
## 2b) Does the effect of Leaf Width on Leaf Length differ significantly
##     (alpha = 0.05) between Locations?
## 2c) What is the value of the test statistic that you used to answer (2b)?

## 3a) What is the simple slope of Leaf Width on Leaf Length in Sabah?
## 3b) Is the simple slope you reported in (3a) significant at the alpha = 0.05
##     level?
## 3c) What is the simple slope of Leaf Width on Leaf Length in Panama?
## 3d) Is the simple slope you reported in (3c) significant at the alpha = 0.05
##     level?
## 3e) What is the simple slope of Leaf Width on Leaf Length in South
##     Queensland?
## 3f) Is the simple slope you reported in (3e) significant at the alpha = 0.05
##     level?

## 4a) In which Location is the effect of Leaf Width on Leaf Length strongest?
## 4b) What caveat might you want to place on the conclusion reported in (4a)?
##     HINT: Look at the answers to Question 1 of this section.


###-END----------------------------------------------------------------------###
