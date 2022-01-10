### Title:    Process Cars93 Data into cars_data.rds
### Author:   Kyle M. Lang
### Created:  2017-AUG-24
### Modified: 2022-01-10

                                        #install.packages("wec", repos = "http://cloud.r-project.org")

rm(list = ls(all = TRUE))

dataDir <- "../data/"

library(MASS)
library(wec)

data(Cars93)

## Create dummy/cell-means codes:
mtCodes           <- model.matrix(~Man.trans.avail - 1, data = Cars93)
colnames(mtCodes) <- c("atOnly", "mtOpt")

drCodes           <- model.matrix(~DriveTrain - 1, data = Cars93)
colnames(drCodes) <- c("four", "front", "rear")

## Create unweighted effects codes:
mtOpt.ec                <- mtCodes[ , "mtOpt"]
mtOpt.ec[mtOpt.ec == 0] <- -1

dr.ec                        <- drCodes[ , c("front", "rear")]
dr.ec[rowSums(dr.ec) == 0, ] <- -1
colnames(dr.ec)              <- c("front.ec", "rear.ec")

## Generate weighted effects codes:
mt            <- Cars93$Man.trans.avail
contrasts(mt) <- contr.wec(mt, "No")
mtOpt.wec     <- model.matrix(~mt)[ , -1]

dr               <- Cars93$DriveTrain
contrasts(dr)    <- contr.wec(dr, "4WD")
dr.wec           <- model.matrix(~dr)[ , -1]
colnames(dr.wec) <- c("front.wec", "rear.wec")

cDat <- data.frame(price = Cars93$Price,
                   mt = Cars93$Man.trans.avail,
                   dr = Cars93$DriveTrain,
                   mtCodes,
                   mtOpt.ec,
                   mtOpt.wec,
                   drCodes,
                   dr.ec,
                   dr.wec)

saveRDS(cDat, paste0(dataDir, "cars_data.rds"))
