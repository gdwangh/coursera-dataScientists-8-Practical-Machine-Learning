# Q1: 计算与可选答案相差很远
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y<-as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)

set.seed(33833)
library(caret)

fit1<-train(vowel.train$y~., mechod="rf", data=vowel.train[,-1])
fit2<-train(vowel.train$y~., mechod="gbm", data=vowel.train[,-1])

predTest1<-predict(fit1, newdata=vowel.test[,-1])
predTest2<-predict(fit2, newdata=vowel.test[,-1])

confusionMatrix(predTest1, vowel.test$y)  # Accuracy : 0.6061
confusionMatrix(predTest2, vowel.test$y)  # Accuracy : 0.6017

sum((predTest1==predTest2) & (predTest2==vowel.test$y))/length(vowel.test$y)  # 0.5757576
sum((predTest1==predTest2) & (predTest2==vowel.test$y))/sum((predTest1==predTest2)) # 0.6303318

# Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
fit1<-train(diagnosis~.,data=training,method="rf")
fit2<-train(diagnosis~.,data=training,method="gbm")
fit3<-train(diagnosis~.,data=training,method="lda")

pred1<-predict(fit1, newdata=training)
pred2<-predict(fit2, newdata=training)
pred3<-predict(fit3, newdata=training)

combDF<-data.frame(pred1,pred2,pred3, diagnosis=training$diagnosis)
combFit<-train(diagnosis ~ ., data=combDF, method="rf")

predTest1<-predict(fit1, newdata=testing)
predTest2<-predict(fit2, newdata=testing)
predTest3<-predict(fit3, newdata=testing)
combTestDF<-data.frame(pred1=predTest1,pred2=predTest2,pred3=predTest3)
combPred<-predict(combFit, combTestDF)

confusionMatrix(predTest1, testing$diagnosis)  # Accuracy : 0.7683
confusionMatrix(predTest2, testing$diagnosis)  # Accuracy : 0.7927
confusionMatrix(predTest3, testing$diagnosis)  # Accuracy : 0.7683
confusionMatrix(combPred, testing$diagnosis)  # Accuracy : 0.7927

accuracy(predTest1, testing$diagnosis)
# Q3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)

fit<-train(CompressiveStrength~., method="lasso",data=training)
# ?plot.enet
par(cex=.8,las=2, mai=c(par("mai")[1:3],2))
plot(fit$finalModel, xvar="penalty", use.color=TRUE)

fit$finalModel$beta.pure

# Q4
library(lubridate)  # For year() function below
# dat = read.csv("~/Desktop/gaData.csv")
dat = read.csv("./ex/gaData.csv")
training = dat[year(dat$date) < 2012,]  # training : year 2011
testing = dat[(year(dat$date)) > 2011,] # testing: year 2012
tstrain = ts(training$visitsTumblr)
tsTest = ts(testing$visitsTumblr)

library(forecast)
batModel<-bats(tstrain)
fcast<-forecast(batModel, h=length(tsTest), level=95)

sum((tsTest>=fcast$lower) & (tsTest<=fcast$upper))/length(tsTest)


# Q5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
library(e1071)
fit<-svm(CompressiveStrength~., data=training)
y<-predict(fit, newdata=testing)
n<-length(y)
RMSE=sqrt(sum((y-testing$CompressiveStrength)^2)/n)
