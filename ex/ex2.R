# Q2

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]


summary(mixtures)

library(Hmisc)
plot(training$CompressiveStrength, col=cut2(training$Cement, g=10) )
plot(training$CompressiveStrength, col=cut2(training$BlastFurnaceSlag, g=10) )
plot(training$CompressiveStrength, col=cut2(training$FlyAsh, g=10) )
plot(training$CompressiveStrength, col=cut2(training$Water, g=10) )
plot(training$CompressiveStrength, col=cut2(training$Superplasticizer, g=10) )
plot(training$CompressiveStrength, col=cut2(training$CoarseAggregate, g=10) )
plot(training$CompressiveStrength, col=cut2(training$FineAggregate, g=10) )
plot(training$CompressiveStrength, col=cut2(training$Age, g=10) )


# Q3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(training$Superplasticizer)
hist(log(training$Superplasticizer+1))


# Q4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

p<-preProcess(training[,grep("IL", names(training))],method="pca", thresh=0.8)
trainPC<-predict(p, training[,grep("IL", names(training))])

names(trainPC)


# Q5: 下面的结果与答案不符合，不知道原因

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 0.75)[[1]]

training = adData[ inTrain, c(1,grep("IL", names(adData)))]
testing = adData[-inTrain,c(1,grep("IL", names(adData)))]

# No-PCA
fit1<-train(diagnosis~.,data=training, method="glm")
confusionMatrix(predict(fit1, newdata=testing), testing$diagnosis)


   # Accuracy : 0.7439
   # Balanced Accuracy : 0.6523

# PCA
fit2<-train(training$diagnosis~.,data=training, method="glm", preProcess="pca", 
            trControl=trainControl(preProcOptions=list(thresh = 0.8)))
confusionMatrix( predict(fit2, newdata=testing), testing$diagnosis)
   # Accuracy : 0.7439
   # Balanced Accuracy : 0.6379

# PCA 2 0.6707
preProc <- preProcess(training[,-1], thresh= 0.80, method="pca")
trainPC <- predict(preProc, training[,-1])
fit3<- train(training$diagnosis~., method="glm", data=trainPC)

testPC<- predict(preProc, testing[,-1])
confusionMatrix(testing$diagnosis, predict(fit3, testPC))

