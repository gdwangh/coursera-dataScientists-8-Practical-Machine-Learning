# Q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

training = segmentationOriginal[segmentationOriginal$Case=="Train",]
testing = segmentationOriginal[segmentationOriginal$Case=="Test",]

set.seed(125)
fit<-train(Class~., method="rpart", data=training)
print(fit$finalModel)

a PS
b WS
c PS
d 无法判断

# Q3
library(pgmm)
data(olive)
olive = olive[,-1]
newdata = as.data.frame(t(colMeans(olive)))

fit<-train(Area~., method="rpart", data=olive)
pred<-predict(fit, newdata)


# Q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
fit<-train(chd~age+alcohol+obesity+tobacco+typea+ldl, method="glm", data=trainSA,  family="binomial")

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd, predict(fit, trainSA))
missClass(testSA$chd, predict(fit, testSA))

# Q5
library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test) 

vowel.train$y<-as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)

set.seed(33833)
fit<-train(y~., mechod="rf", data=vowel.train)
varImp(fit)
