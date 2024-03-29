### Title:    Suggested Solutions 3: Moderation
### Author:   Kyle M. Lang
### Created:  2018-09-24
### Modified: 2024-01-24

library(dplyr)
library(magrittr)
library(rockchalk)

## The isSig() function is defined here:
source("code/support/helper_functions.R")


###-Preliminaries------------------------------------------------------------###

## 3.1a) Use the readRDS() function to load the "msq2.rds" dataset.

dataDir <- "data/"
msq2    <- readRDS(paste0(dataDir, "msq2.rds"))

## 3.1b) Use the data() function to load the "cps3" and "leafshape" datasets
##       from the DAAG package.

data(cps3, package = "DAAG")
data(leafshape, package = "DAAG")


###-Continuous Variable Moderation-------------------------------------------###

## 3.2a) Estimate a model that tests if the effect of Energetic Arousal (EA) on
##       Tense Arousal (TA) varies as a function of Negative Affect (NegAff),
##       after controlling for Positive Affect (PA).

out1 <- lm(TA ~ PA + EA * NegAff, data = msq2)
summary(out1)

###--------------------------------------------------------------------------###

## 3.3a) What is the value of the parameter estimate that quantifies the effect
##       of Negative Affect on the Energetic Arousal -> Tense Arousal effect,
##       after controlling for Positive Affect?

coef(out1)["EA:NegAff"]

## 3.3b) Does Negative Affect significantly moderate (at the alpha = 0.05 level)
##       the relationship between Energetic Arousal and Tense Arousal, after
##       controlling for Positive Affect?

isSig(out1, "EA:NegAff")

## 3.3c) After controlling for Positive Affect, how does Negative Affect impact
##       the relationship between Energetic Arousal and Tense Arousal? Provide a
##       sentence interpreting the appropriate effect.

### For a unit increase in Negative Affect, the effect of Energetic Arousal on
### Tense Arousal is expected to increase by 0.0196, after controlling for
### Positive Affect.


###-Probing via Centering----------------------------------------------------###

## 3.4a) Use the centering method to test the simple slopes of the model you
##       estimated in PP 3.2 at Negative Affect values of 0, 10, and 20.

msq2 %<>% mutate(na10 = NegAff - 10, na20 = NegAff - 20)

out1.10 <- lm(TA ~ PA + EA * na10, data = msq2)
summary(out1.10)

out1.20 <- lm(TA ~ PA + EA * na20, data = msq2)
summary(out1.20)

## 3.4b) After controlling for Positive Affect, what is the simple slope of
##       Energetic Arousal on Tense Arousal when Negative Affect is 0.

coef(out1)["EA"]

## 3.4c) Is the simple slope you estimated in (b) statistically significant at
##       the alpha = 0.05 level?

isSig(out1, "EA")

## 3.4d) After controlling for Positive Affect, what is the simple slope of
##       Energetic Arousal on Tense Arousal when Negative Affect is 10.

coef(out1.10)["EA"]

## 3.4e) Is the simple slope you estimated in (d) statistically significant at
##       the alpha = 0.05 level?

isSig(out1.10, "EA")

## 3.4f) After controlling for Positive Affect, what is the simple slope of
##       Energetic Arousal on Tense Arousal when Negative Affect is 20.

coef(out1.20)["EA"]

## 3.4g) Is the simple slope you estimated in (f) statistically significant at
##       the alpha = 0.05 level?

isSig(out1.20, "EA")


###-Probing via the 'rockchalk' Package--------------------------------------###

## 3.5a) Use the 'rockchalk' package to test the same simple slopes you
##       estimated in PP 3.4.

psOut1 <-
    plotSlopes(out1, plotx = "EA", modx = "NegAff", modxVals = c(0, 10, 20))
tsOut1 <- testSlopes(psOut1)
tsOut1$hypotests

## 3.5b) Do the results of the centering approach agree with the results from
##       'rockchalk'?

### Calculate the differences between the two approaches:
(
   dif <- tsOut1$hypotests[ , -1] - rbind(summary(out1)$coefficients["EA", ],
                                         summary(out1.10)$coefficients["EA", ],
                                         summary(out1.20)$coefficients["EA", ]
                                         )
)

### Are all differences lower than machine precision?
all(dif < .Machine$double.eps) %>% ifelse("YES", "NO")

###--------------------------------------------------------------------------###

## 3.6) Run a Johnson-Neyman analysis on the model you estimated in PP 4.2.
##     - What values of Negative Affect produce significant simple slopes of
##       Energetic Arousal on Tense Arousal, after controlling for Positive 
##       Affect?

tsOut1$jn$roots

summary(out1)

### The effect of EA on TA is significant for all values of NegAff.


###-Binary Categorical Moderators--------------------------------------------###

## 3.7) Estimate a model that tests if the effect of Years of Education on Real
##      Earnings in 1975 is significantly moderated by being Hispanic, after
##      controlling for Real Earnings in 1973.

cps3 %<>% mutate(hisp = factor(hisp,
                               levels = c(0, 1),
                               labels = c("non-hispanic", "hispanic")
                               )
                 )

out2 <- lm(re75 ~ re74 + educ * hisp, data = cps3)
summary(out2)

###--------------------------------------------------------------------------###

## 3.8a) After controlling for 1974 Earnings, does being Hispanic significantly
##       affect the relationship between Years of Education and 1975 Earnings at
##       the alpha = 0.05 level

isSig(out2, "educ:hisphispanic")

## 3.8b) After controlling for 1974 Earnings, does being Hispanic significantly
##       affect the relationship between Years of Education and 1975 Earnings at
##       the alpha = 0.01 level?

isSig(out2, "educ:hisphispanic", alpha = 0.01)

###--------------------------------------------------------------------------###

## 3.9a) What is the simple slope of Years of Education on 1975 Earnings
##       (controlling for 1974 Earnings) for Non-Hispanic people?

coef(out2)["educ"]

## 3.9b) Is the simple slope from (a) statistically significant at the
##       alpha = 0.05 level?

isSig(out2, "educ")

## 3.9c) What is the simple slope of Years of Education on 1975 Earnings
##       (controlling for 1974 Earnings) for Hispanic people?

cps3 %>%
    mutate(hisp = relevel(hisp, ref = "hispanic")) %$%
    lm(re75 ~ re74 + educ * hisp) ->
    out2.2

summary(out2.2)

coef(out2.2)["educ"]

## 3.9d) Is the simple slope from (c) statistically significant at the
##       alpha = 0.05 level?

isSig(out2.2, "educ")

###--------------------------------------------------------------------------###

## 3.10) Visualize the simple slopes from PP 4.9 in an appropriate way.

plotSlopes(out2, plotx = "educ", modx = "hisp")


###-Nominal Categorical Moderators-------------------------------------------###

## 3.11a) What are the levels of the "location" factor?

levels(leafshape$location)

## 3.11b) What are the group sizes for the "location" factor?

table(leafshape$location)

###--------------------------------------------------------------------------###

## 3.12) Estimate a model that tests if the effect of Leaf Width on Leaf Length
##       differs significantly between Locations.

out3 <- lm(bladelen ~ bladewid * location, data = leafshape)
summary(out3)

###--------------------------------------------------------------------------###

## 3.13a) Does the effect of Leaf Width on Leaf Length differ significantly
##        (alpha = 0.05) between Locations?

av3 <- lm(bladelen ~ bladewid + location, data = leafshape) %>%
    anova(out3)

isSig(av3)

## 3.13b) What is the value of the test statistic that you used to answer (a)?

av3[2, "F"]

###--------------------------------------------------------------------------###

## 3.14a) What is the simple slope of Leaf Width on Leaf Length in Sabah?

coef(out3)["bladewid"]

## 3.14b) Is the simple slope you reported in (a) significant at the
##        alpha = 0.05 level?

isSig(out3, "bladewid")

## 3.14c) What is the simple slope of Leaf Width on Leaf Length in Panama?

leafshape %>%
    mutate(location = relevel(location, ref = "Panama")) %>%
    update(out3, data = .) ->
    out3.1

summary(out3.1)

coef(out3.1)["bladewid"]

## 3.14d) Is the simple slope you reported in (c) significant at the
##        alpha = 0.05 level?

isSig(out3.1, "bladewid")

## 3.14e) What is the simple slope of Leaf Width on Leaf Length in South
##     Queensland?

leafshape %>%
    mutate(location = relevel(location, ref = "S Queensland")) %>%
    update(out3, data = .) ->
    out3.2

summary(out3.2)

coef(out3.2)["bladewid"]

## 3.14f) Is the simple slope you reported in (e) significant at the
##        alpha = 0.05 level?

isSig(out3.2, "bladewid")

## 3.14g) In which Location is the effect of Leaf Width on Leaf Length strongest?

cf  <- coef(out3)
tmp <- cf[grep("bladewid", names(cf))]

best <- levels(leafshape$location)[which.max(c(tmp[1], tmp[1] + tmp[-1]))]
best

## 3.14h) What caveat might you want to place on the conclusion reported in (g)?

table(leafshape$location)[best]

### We only have 9 observations from Tasmania. The estimated simple slope for
### Tasmania is going to be very sensitive to those particular 9 observations.

###--------------------------------------------------------------------------###

## Alternatively, we can get all of the statistics we need to answer 3.14 in one
## shot via rockchalk routines.

ss3 <- plotSlopes(out3, plotx = "bladewid", modx = "location") %>%
    testSlopes()

ss3$hypotests


###-END----------------------------------------------------------------------###
