#setwd("D:/workspace/dataScientists/8-Practical Machine Learning/coursera-dataScientists-8-Practical-Machine-Learning/project")
setwd("./project")

library(caret)
# tmp_ds<-read.csv("pml-training.csv")
train_ds<-read.csv("pml-training.csv")
test_ds<-read.csv("pml-testing.csv")

# split cross validate dataset
set.seed(5)
# inTrain = createDataPartition(tmp_ds$classe, p = 3/4)[[1]]
# train_ds = tmp_ds[ inTrain,]
# valid_ds = tmp_ds[-inTrain,]

summary(train_ds)

# 发现train set 中有很多字段的值NA 或 空的数量很多，删除那些字段
n<-ncol(train_ds)
th<-nrow(train_ds)/5   # NA 太多, 有效记录数小于阀值，就不能要这个列
clist<-c()
for (i in 3:n)  {   # 跳过x 和user_name, 以及输出项 classe
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

# train 执行出错，因为 glm只能针对2-level factor
preProc_belt<-preProcess(train_ds[,c(8:11,37:45)], method="pca", thresh=0.8)
trainPC_belt<-predict(preProc_belt, train_ds[,c(8:11,37:45)])
fit1<-train(train_ds$classe~., method="glm",data=trainPC_belt,family="binomial")

fit1<-t(train_ds$classe ~ ., data=trainPC_belt, family="binomial")
dim(trainPC_belt)

