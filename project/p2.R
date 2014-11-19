setwd("./project")

# set parall compute
library(doMC) # install.packages('doMC')
registerDoMC(4) #adjust to your number of cores

library(caret)
tmp_ds<-read.csv("pml-training.csv", na.strings=c("NA","","#DIV/0!"))

# train_ds<-read.csv("pml-training.csv")
test_ds<-read.csv("pml-testing.csv", na.strings=c("NA","","#DIV/0!"))

# for each of the four sensors, get out the Euler angles and the raw accelerometer, gyroscope and
# magnetometer readings
var_list<-c(8:11,37:45,46:49,60:68,84:86,102,113:121,122:124,140,151:159)

# test
# 调整顺序，以便比较test data set的结果
test_key=test_ds[,c(2,3,4)]
test_key$user_name=gsub("^ +", "", test_key$user_name)
test_key$user_name=gsub(" +$", "", test_key$user_name)
sorted_test_ds<-test_ds[do.call(order, test_key), var_list]

all_ds<-read.csv("pml-all.csv", na.strings=c("NA","","#DIV/0!"))
sub_ds<-subset(all_ds, (user_name %in% test_ds$user_name) & (raw_timestamp_part_1 %in% test_ds$raw_timestamp_part_1) & (raw_timestamp_part_2 %in% test_ds$raw_timestamp_part_2), select=c("user_name","raw_timestamp_part_1","raw_timestamp_part_2","classe"))

sub_key=sub_ds[,c(1,2,3)]
sub_key$user_name=gsub("^ +", "", sub_key$user_name)
sub_key$user_name=gsub(" +$", "", sub_key$user_name)
sorted_test_classe<-sub_ds[ do.call(order, sub_key), "classe"]



# split data from pml-training.csv into training and cross validation dataset
set.seed(12345)
inTrain = createDataPartition(tmp_ds$classe, p = 0.75)[[1]]
train_ds = tmp_ds[ inTrain, c(var_list,160)]
valid_ds = tmp_ds[-inTrain,c(var_list,160)]

#lapply(train_ds, class)
nsv<-nearZeroVar(train_ds, saveMetrics=TRUE)
nsv   # no TRUE to delete

# Random Forest
ModFit1<-train(classe ~ ., train_ds, method="rf")
# ModFit1<-randomForest(classe ~ ., train_ds)
vImp<-varImp(ModFit1)
vImp<-data.frame(varname=rownames(vImp), Overall=vImp$Overall, row.names=rownames(vImp))
vImp[order(vImp$Overall, decreasing=TRUE), ]

pred<-predict(ModFit1, valid_ds)  
confusionMatrix(valid_ds$classe, pred)  # Accuracy : 0.9939

pred_test<-predict(ModFit1, sorted_test_ds)
confusionMatrix(sorted_test_classe, pred_test)  # Accuracy : 1

# Random forest with pca
# ModFit2<-train(classe ~ ., train_ds, method="rf", preProcess="pca", 
#               trControl=trainControl(preProcOptions=list(thresh = 0.9)))

preProc<-preProcess(train_ds[,-53], method="pca",thresh=0.9)  # 53 means classe
trainPC<-predict(preProc, train_ds[,-53])
ModFit2<-randomForest(train_ds$classe~.,data=trainPC)

validPC<-predict(preProc, valid_ds[,-53])
y<-predict(ModFit2, validPC)
confusionMatrix(valid_ds$classe, y)   # Accuracy : 0.9689

testPC<-predict(preProc, sorted_test_ds)
y<-predict(ModFit2, testPC)
confusionMatrix(sorted_test_classe, y)  # Accuracy : 1 

# write 20 files for prediction 

answers = rep("A", 20)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)

pred_test<-predict(ModFit1, test_ds)
pml_write_files(pred_test)
