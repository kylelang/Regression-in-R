### Title:    Helper Functions for Practice Problems
### Author:   Kyle M. Lang
### Created:  2023-01-25
### Modified: 2023-01-25

## Extract p-values from a fitted lm object:
## - obj  = a fitted lm object
## - what = name of the coefficient for which to extract the p-value
getP <- function(obj, what) summary(obj)$coef[what, "Pr(>|t|)"]

## Answer yes/no significance questions:
## - obj   = a fitted lm object
## - what  = name of the coefficient for which to extract the p-value
## - alpha = desired statistical significance level
isSig <- function(obj, what, alpha = 0.05)
    ifelse(getP(obj, what) < alpha, "YES", "NO")

## Automatically fix effects code names for the factor x:
fixEcNames <- function(x) {
    tmp                    <- contrasts(x)
    colnames(contrasts(x)) <- rownames(tmp)[rowSums(tmp) > 0]
    x
}

## Automatically change which level will represent the "omitted group" when
## effects-coding the factor x:
changeOmitted <- function(x) relevel(x, ref = tail(levels(x), 1))
