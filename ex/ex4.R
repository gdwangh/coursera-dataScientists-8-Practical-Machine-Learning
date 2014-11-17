# Q1
library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test) 

vowel.train$y<-as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)

set.seed(33833)
fit1<-train(y~., mechod="rf", data=vowel.train)
fit2<-train(y~., mechod="gbm", data=vowel.train, trControl=trainControl(method="cv"))

pred1<-predict(fit1, newdata=vowel.test)
pred2<-predict(fit2, newdata=vowel.test)

predDF<-data.frame(pred1, pred2, y=vowel.test$y)
combFit<-train(y~., method="gam", data=predDF)
combPred<-predict(combFit, predDF)

confusionMatrix(pred1, vowel.test$y)  # Accuracy : 0.6061
confusionMatrix(pred2, vowel.test$y)  # Accuracy : 0.5996
confusionMatrix(combPred, vowel.test$y) #Accuracy : 0.0173


