library(car)
library(dplyr)
library(magrittr)

data(Cars93, package = "MASS")

colnames(Cars93)

?MASS::Cars93

dat1 <- Cars93 %>% select(-Manufacturer, 
                          -Model, 
                          -Type, 
                          -AirBags, 
                          -DriveTrain, 
                          -Man.trans.avail, 
                          -Origin, 
                          -Make)


colnames(dat1)

plot(dat1)

Cars93 %$% cor(EngineSize, Turn.circle, use = "pairwise")

out0 <- lm(Horsepower ~ Turn.circle, data = Cars93)
summary(out0)
plot(out0, 1)

dev.off()

table(Cars93$Passengers)

colMeans(is.na(Cars93))

out1 <- lm(MPG.highway ~ Horsepower, data = Cars93)
out2 <- lm(MPG.highway ~ Turn.circle, data = Cars93)
out3 <- lm(MPG.highway ~ Horsepower + Turn.circle, data = Cars93)

plot(Cars93)

summary(out1)
summary(out2)
summary(out3)

plot(out1, 1)
plot(out2, 1)
plot(out3, 1)
crPlots(out3)

out4 <- update(out3, ". ~ . + poly(Horsepower, 2) - Horsepower")
out5 <- update(out3, ". ~ . + log(EngineSize) - EngineSize")

crPlots(out3)
crPlots(out4)

tmp <- Cars93 %$% 
  poly(EngineSize, 2) %>% 
  as.data.frame() %>% 
  rename(EngineSize1 = 1, EngineSize2 = 2)

dat2 <- data.frame(dat1, tmp)

out5 <- lm(MPG.highway ~ EngineSize1 + EngineSize2 + Turn.circle, data = dat2)

plot(out5, 1)
crPlots(out5)

crPlot(out4, "Turn.circle")
crPlot(out5, "Turn.circle")

crPlot(out5, "EngineSize1")
crPlot(out5, "EngineSize2")

crPlots(out4)

tmp1 <- coef(out5)[2] * dat2$EngineSize1 + coef(out5)[3] * dat2$EngineSize2
tmp2 <- predict(out4, type = "terms")[ , 2]

tmp1 - tmp2

?crPlot

?crPlots

out4 <- update(out3, ". ~ . + poly(EngineSize, 2) - EngineSize")

summary(out4)

dat2$EngineSize1 - (dat2$EngineSize - mean(dat2$EngineSize))

plot(out4, 1)
crPlots(out4)

crPlots(out3)

crPlot(out4, "poly(EngineSize, 2)")

dfb <- dfbetas(out3)

apply(dfb, 2, plot)

tmp <- lm(prestige ~ log2(income) + education + poly(women,2), data=Prestige)
summary(tmp)

ls(tmp)


tmp$terms
