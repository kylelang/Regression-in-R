### Title:    Helper Functions for Practice Problems
### Author:   Kyle M. Lang
### Created:  2023-01-25
### Modified: 2023-01-26

###--------------------------------------------------------------------------###

## Extract p-values from a fitted lm object:
## - obj  = a fitted 'lm' or 'anova' object
## - what = name of the coefficient (lm) or model number (anova) for which to 
##          extract the p-value
getP <- function(obj, what) 
{
  ## lm() object:
  if(class(obj)[1] == "lm")
    summary(obj)$coef[what, "Pr(>|t|)"]
  
  ## anova() object:
  else if(class(obj)[1] == "anova") 
    obj[what, "Pr(>F)"]
  
  ## illegal object class:
  else
    stop("I don't know what to do with an object of class '", class(obj), "'.")
  
}

###--------------------------------------------------------------------------###

## Answer yes/no significance questions:
## - obj   = a fitted lm object
## - what  = name of the coefficient (lm) or model number (anova) for which to 
##           extract the p-value
## - alpha = desired statistical significance level
isSig <- function(obj, what = NA, alpha = 0.05, logical = FALSE) 
{
  ## For two-model comparisons, we always want the second model:
  check <- is.na(what) && class(obj)[1] == "anova" && nrow(obj) == 2
  if(check) what <- 2
  
  ## Is 'what' significant at the 'alpha' level?
  check <- getP(obj, what) < alpha
  
  ## Return the logical answer, if 'logical = TRUE':
  if(logical) return(check)
  
  ## Return a character answer if 'logical = FALSE':
  ifelse(check, "YES", "NO")
}

###--------------------------------------------------------------------------###

## Automatically fix effects code names for the factor x:
fixEcNames <- function(x) {
    tmp                    <- contrasts(x)
    colnames(contrasts(x)) <- rownames(tmp)[rowSums(tmp) > 0]
    x
}

###--------------------------------------------------------------------------###

## Automatically change which level will represent the "omitted group" when
## effects-coding the factor x:
changeOmitted <- function(x) relevel(x, ref = tail(levels(x), 1))

###--------------------------------------------------------------------------###
