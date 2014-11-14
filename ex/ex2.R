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

# p<-preProcess(training[,grep("IL", names(training))],method="pca", thresh=0.8)
# 错在 grep("IL",...)，正则表达式有错，匹配的不仅仅是IL开头的，还有中间是IL的
# 所以得到的列不对，导致下面答案错误
p<-preProcess(training[,grep("^IL", names(training), perl = TRUE, value=TRUE)],method="pca", thresh=0.8)


# trainPC<-predict(p, training[,grep("IL", names(training))])
trainPC<-predict(p, training[,grep("^IL", names(training), perl = TRUE, value=TRUE)])

names(trainPC)


# Q5: 原先错误在grep("IL"...)匹配的不仅仅是IL开头的，还有中间含IL的

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 0.75)[[1]]

# 错在 grep("IL",...)，正则表达式有错，匹配的不仅仅是IL开头的，还有中间是IL的
# 所以得到的列不对，导致下面答案错误
#training = adData[ inTrain, c(1,grep("IL", names(adData)))]
# testing = adData[-inTrain,c(1,grep("IL", names(adData)))]

training = adData[ inTrain, c(1,grep("^IL", names(adData), perl = TRUE))]
testing = adData[-inTrain,c(1,grep("^IL", names(adData), perl = TRUE))]

# No-PCA
fit1<-train(diagnosis~.,data=training, method="glm")
confusionMatrix(predict(fit1, newdata=testing), testing$diagnosis)

   # grep错误导致的答案：
   # Accuracy : 0.7439
   # Balanced Accuracy : 0.6523

# 修改正则表达式后：Accuracy : 0.6463

# PCA
fit2<-train(training$diagnosis~.,data=training, method="glm", preProcess="pca", 
            trControl=trainControl(preProcOptions=list(thresh = 0.8)))
confusionMatrix( predict(fit2, newdata=testing), testing$diagnosis)
  # grep错误导致的答案：
   # Accuracy : 0.7439
   # Balanced Accuracy : 0.6379

# 修改正则表达式后：Accuracy : 0.7195


# PCA 2 修改正则表达式后：Accuracy : 0.7195
preProc <- preProcess(training[,-1], thresh= 0.80, method="pca")
trainPC <- predict(preProc, training[,-1])
fit3<- train(training$diagnosis~., method="glm", data=trainPC)

testPC<- predict(preProc, testing[,-1])
confusionMatrix(testing$diagnosis, predict(fit3, testPC))

