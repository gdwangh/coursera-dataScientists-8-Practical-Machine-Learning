#setwd("D:/workspace/dataScientists/8-Practical Machine Learning/coursera-dataScientists-8-Practical-Machine-Learning/project")
setwd("./project")

library(caret)
tmp_ds<-read.csv("pml-training.csv")
# train_ds<-read.csv("pml-training.csv")
test_ds<-read.csv("pml-testing.csv")

all_ds<-read.csv("pml-all.csv")
sub_ds<-subset(all_ds, (user_name %in% test_ds$user_name) & (raw_timestamp_part_1 %in% test_ds$raw_timestamp_part_1) & (raw_timestamp_part_2 %in% test_ds$raw_timestamp_part_2), select=c("user_name","raw_timestamp_part_1","raw_timestamp_part_2","classe"))

# 调整顺序，以便比较test data set的结果
test_key=test_ds[,c(2,3,4)]
test_key$user_name=gsub("^ +", "", test_key$user_name)
test_key$user_name=gsub(" +$", "", test_key$user_name)
sorted_test_ds<-test_ds[do.call(order, test_key), ]

sub_key=sub_ds[,c(1,2,3)]
sub_key$user_name=gsub("^ +", "", sub_key$user_name)
sub_key$user_name=gsub(" +$", "", sub_key$user_name)
sorted_sub_ds<-sub_ds[ do.call(order, sub_key), ]


# split cross validate dataset
set.seed(5)
inTrain = createDataPartition(tmp_ds$classe, p = 3/4)[[1]]
train_ds = tmp_ds[ inTrain,]
valid_ds = tmp_ds[-inTrain,]

summary(train_ds)

# 发现train set 中有很多字段的值NA 或 空的数量很多，删除那些字段
n<-ncol(train_ds)
th<-nrow(train_ds)/5   # NA 太多, 有效记录数小于阀值，就不能要这个列
clist<-c()
for (i in 8:(n-1))  {   # 跳过x 和user_name, new_window, num_window, classe
  # 统计本列不是 NA 和 “” 的记录数量
  NA_num<- sum(!is.na(train_ds[,i]) & (train_ds[,i]!=""))
  
  # 检查是否是timestamp字段
  if (grepl("timestamp", names(train_ds)[i],ignore.case=TRUE)) next
  
  if (NA_num > th) clist<-c(clist, i)
}

summary(train_ds[,clist])
names(train_ds)[clist]

qplot(train_ds$classe, train_ds$pitch_belt,geom="boxplot")
qplot(train_ds$classe, train_ds$total_accel_forearm)



fit<-train(classe~., data=train_ds[,clist], method="glm")

nearZeroVar(train_ds[,clist], saveMetrics=TRUE)



featurePlot(x=train_ds[,c(6,7,9)], y=train_ds[,160], plot="pairs")
classe

# 检查NA值很多的那些列是什么回事
# max_roll_belt等多NA字段，与num_window，new_window的关系 
qplot(train_ds$num_window,train_ds$new_window, col=is.na(train_ds$max_roll_belt))
# 观察结果显示：
# new_window = yes --- max，min等计算属性有值
#            = no --- max，min等计算属性=NA

t<-table(train_ds$num_window,train_ds$new_window)  

# 有些num_windows没有new_windows, 有些num_windows有1个new_windows=yes，不会有2、3、4个

# new_window 与 classe的关系,貌似没有直接关系
qplot(train_ds$num_window,train_ds$classe, col=train_ds$new_window)

#  belt sensors相关各参数之间的关系
M<-abs(cor(train_ds[,c(8:11,37:45)]))
diag(M)<-0
which(M>0.8,arr.ind=T)

# 测试 belt
preProc_belt<-preProcess(train_ds[,c(8:11,37:45)], method="pca", thresh=0.8)
trainPC_belt<-predict(preProc_belt, train_ds[,c(8:11,37:45)])

fit1<-train(train_ds$classe~., method="rpart",data=trainPC_belt)

validPC_belt<-predict(preProc_belt, valid_ds[,c(8:11,37:45)])

y<-predict(fit1, validPC_belt)
confusionMatrix(valid_ds$classe, y)

library(rpart)
library(rpart.plot)
library(rattle)
fancyRpartPlot(fit2$finalModel)
print(fit2$finalModel)

# 测试所有的
preProc<-preProcess(train_ds[clist], method="pca")
trainPC<-predict(preProc, train_ds[clist])

fit1<-train(train_ds$classe~., method="rf",data=trainPC)
confusionMatrix(train_ds$classe, predict(fit1, trainPC))  # Accuracy : 1 

validPC<-predict(preProc, valid_ds[clist])

y<-predict(fit1, validPC)
confusionMatrix(valid_ds$classe, y)  # Accuracy : 0.977

testPC<-predict(preProc, sorted_test_ds[clist])
y2<-predict(fit1, testPC)
confusionMatrix(sorted_sub_ds$classe, y2)  # Accuracy : 0.95

#14718 samples
#11 predictor
#5 classes: 'A', 'B', 'C', 'D', 'E' 


# 不用预处理后的，直接用原始的
fit2<-train(train_ds$classe~., method="rf",data=train_ds[clist]) 
confusionMatrix(train_ds$classe, predict(fit2, train_ds[clist])) # Accuracy : 1
confusionMatrix(valid_ds$classe, predict(fit2, valid_ds[clist]))# Accuracy : 1
confusionMatrix(sorted_sub_ds$classe, predict(fit2, sorted_test_ds[clist]))  # Accuracy : 0.9947

# 14718 samples
# 51 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E'


# booting, 试了几个，除了gbm，其他出错，都是用在2-level factor的
fit3<-train(train_ds$classe~., method="gbm",data=train_ds[clist]) 
confusionMatrix(train_ds$classe, predict(fit3, train_ds[clist])) # Accuracy : gbm=0.9736
confusionMatrix(valid_ds$classe, predict(fit3, valid_ds[clist]))# Accuracy : gbm=0.9598
confusionMatrix(sorted_sub_ds$classe, predict(fit3, sorted_test_ds[clist]))  # gbm=Accuracy : 0.9947

# combine 2 models
fit4_1<-train(train_ds$classe~., method="gbm",data=train_ds[clist]) 
fit4_2<-train(train_ds$classe~., method="rf",data=train_ds[clist],
              trControl=trainControl(method="cv")) 

confusionMatrix(valid_ds$classe, predict(fit4_1, valid_ds[clist]))  # Accuracy : 0.9602
confusionMatrix(valid_ds$classe, predict(fit4_2, valid_ds[clist]))  # Accuracy : 0.9945

pred1<-predict(fit4_1, valid_ds[clist])
pred2<-predict(fit4_2, valid_ds[clist])
predDF<-data.frame(pred1, pred2, classe=valid_ds$classe)
combFit<-train(classe~., method="gam", data=predDF)

pred1test<-predict(fit4_1, sorted_test_ds[clist])
pred2test<-predict(fit4_2, sorted_test_ds[clist])
predDF_test<-data.frame(pred1=pred1test, pred2=pred2test)
combPred_test<-predict(combFit, predDF_test)

confusionMatrix(valid_ds$classe, predict(combFit, predDF))  # Accuracy : 
confusionMatrix(sorted_sub_ds$classe, combPred_test)        # Accuracy : 

