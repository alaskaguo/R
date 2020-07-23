breast<-read.spss("BreastCancer.sav")
breast<-as.data.frame(breast)
breast<-na.omit(breast)
head(breast)

str(breast)
breast$Status<-ifelse(breast$Status == 'Dead',1,0)
breast$Pathologic_stage<-as.numeric(breast$Pathologic_stage)
breast$PgR <-as.numeric(ifelse(breast$PgR == "Positive", 1,0))
str(breast)

set.seed(123) #random number generator
ind <- sample(2, nrow(breast), replace = TRUE, prob = c(0.5, 0.5))
train <- breast[ind==1, ] #the training data set
test <- breast[ind==2, ] #the test data set
str(train)

str(test)

dd <- datadist(train)
option <- options(datadist = "dd")
coxm <- cph(Surv(Months,Status==1) ~ Age+Pathologic_stage+PgR,
            data = train, x = T, y = T, surv = T)
surv <- Survival(coxm)
nom <- nomogram(coxm,fun=list(function(x)surv(12, x), function(x)surv(36, x),
                              function(x)surv(60, x)),
                lp = T,funlabel = c('1-Yeas OS', '3-Year OS','5-YearOS'),
                maxscale = 100, fun.at = c('0.95','0.85','0.80','0.70','0.6','0.5','0.4','0.3','0.2','0.1'))
plot((nom), xfrac = .3)

options(option)
results <- formula_lp(nomogram = nom)
points <- points_cal(formula = results$formula, lp = coxm$linear.predictors)
head(points)

options(option)
results <- formula_rd(nomogram = nom)
train$points <- points_cal(formula = results$formula,rd=train)
test$points <- points_cal(formula = results$formula,rd=test)
breast$points <- points_cal(formula = results$formula,rd=breast)
head(breast$points)

train$linear.predictors<-coxm$linear.predictors
head(train$linear.predictors)

test$linear.predictors<-predict(coxm,type="lp",newdata=test)
breast$linear.predictors<-predict(coxm,type="lp",newdata=breast)
head(breast$linear.predictors)

train$survprob1 <- predictSurvProb(coxm,newdata=train,times=12)
head(train$survprob1)

test$survprob2 <- predictSurvProb(coxm,newdata=test,times=12)
head(test$survprob2)

breast$survprob <- predictSurvProb(coxm,newdata=breast,times=12)
head(breast$survprob)