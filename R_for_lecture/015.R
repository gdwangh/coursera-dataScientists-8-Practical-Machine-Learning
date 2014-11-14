# 在使用bs增加feature后，如何在test数据集上预测？

library(ISLR)
library(caret)
data(Wage)

inTrain<-createDataPartition(y=Wage$age,p=0.7, list=FALSE)

training<-Wage[inTrain,]
testing<-Wage[-inTrain,]

library(splines)

# 方法1：
lm1<-lm(wage~bs(age,df=3), data=training)
predict_test_wage1<- predict(lm1, data.frame(age=testing$age))

# 方法2：
lm1<-lm(wage~bs(age,df=3), data=training)
predict_test_wage2<- predict(lm1, newdata=testing)

# 方法3：
bsBasis<-bs(training$age, df=3)
lm1<-lm(wage~bsBasis, data=training)
bsBasis<-predict(bsBasis, testing$age)  # splines on test dataset
pred_test_wage<- predict(lm1, newdata=testing)


# 绘图
par(mfrow = c(1, 1))
pre_train_wage<-predict(lm1, newdata=training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, pre_train_wage, col="red",pch=19, cex=0.5)
points(testing$age, predict_test_wage1, col="blue",pch=2, cex=0.5)

par(mfrow = c(1, 1))
plot(testing$age, testing$wage,pch=19, cex=0.5)
points(testing$age, predict_test_wage1, col="red",pch=19, cex=0.5)
points(testing$age, predict_test_wage2, col="blue",pch=3, cex=0.5)





