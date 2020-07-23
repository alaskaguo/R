library(MASS)
biopsy$ID = NULL
names(biopsy) = c("thick", "u.size", "u.shape", "adhsn","s.size", "nucl", "chrom", "n.nuc", "mit", "class")
biopsy<- na.omit(biopsy)

set.seed(123) #random number generator
ind <- sample(2, nrow(biopsy), replace = TRUE, prob = c(0.7, 0.3))
train <- biopsy[ind==1, ] #the training data set
test <- biopsy[ind==2, ] #the test data set
str(train)

library(rms)

dd <- datadist(train)
option <- options(datadist = "dd")
fit1 <- lrm(class ~ thick+u.size+u.shape+nucl+chrom+n.nuc, data=train, x=T, y=T)
nom <- nomogram(fit1, fun = plogis, fun.at = c(.01, seq(.1,.9, by = .2), .999), lp = F, funlabel = "Probability of malignancy")
plot(nom,xfrac=0.4)

train$pred1 <- predict(fit1, train, type = "fitted")
train$linear.predictor <- predict(fit1, train, type = "lp")

test$pred2 <- predict(fit1, test, type = "fitted")
test$linear.predictor <- predict(fit1, test, type = "lp")

library(InformationValue)
train$actuals <- ifelse(train$class == "malignant", 1, 0)
misClassError(train$actuals, train$pred1)

plotROC(train$actuals, train$pred1)

test$actuals <- ifelse(test$class == "malignant", 1, 0)
misClassError(test$actuals, test$pred2)

plotROC(test$actuals, test$pred2)

library(PredictABEL)
plotCalibration(data=train, cOutcome=13, predRisk=train$pred1, groups=30, rangeaxis=c(0,1))

plotCalibration(data=test, cOutcome=13, predRisk=test$pred2, groups=30, rangeaxis=c(0,1))

library(nomogramFormula)
options(option)
results <- formula_rd(nomogram = nom)
train$points <- points_cal(formula = results$formula,rd=train)
test$points <- points_cal(formula = results$formula,rd=test)
biopsy$points <- points_cal(formula = results$formula,rd=biopsy)
head(biopsy$points)