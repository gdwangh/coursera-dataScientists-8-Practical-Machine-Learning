setwd("D:/workspace/dataScientists/8-Practical Machine Learning/coursera-dataScientists-8-Practical-Machine-Learning/project")

library(caret)
tmp_ds<-read.csv("pml-training.csv")
test_ds<-read.csv("pml-testing.csv")

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
for (i in 3:(n-1))  {   # 跳过x 和user_name, 以及输出项 classe
  # 统计本列不是 NA 和 “” 的记录数量
  NA_num<- sum(!is.na(train_ds[,i]) & (train_ds[,i]!=""))
  
  # 检查是否是timestamp字段
  if (grepl("timestamp", names(train_ds)[i],ignore.case=TRUE)) next
  
  if (NA_num > th) clist<-c(clist, i)
}

summary(train_ds[,clist])

qplot(train_ds$classe, train_ds$num_window,geom="boxplot")
qplot(train_ds$classe, train_ds$num_window)


nearZeroVar(train_ds[,clist], saveMetrics=TRUE)



featurePlot(x=train_ds[,c(6,7,9)], y=train_ds[,160], plot="pairs")
classe

