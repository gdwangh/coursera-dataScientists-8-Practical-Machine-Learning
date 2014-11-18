setwd("./project")

library(caret)
tmp_ds<-read.csv("pml-training.csv", na.strings=c("NA","","#DIV/0!"))

# train_ds<-read.csv("pml-training.csv")
test_ds<-read.csv("pml-testing.csv", na.strings=c("NA","","#DIV/0!"))

# for each of the four sensors, get out the Euler angles and the raw accelerometer, gyroscope and
# magnetometer readings
var_list<-c(8:11,37:45,46:49,60:68,84:86,102,113:121,122:124,140,151:159)
# split data from pml-training.csv into training and cross validation dataset

set.seed(12345)
inTrain = createDataPartition(tmp_ds$classe, p = 3/4)[[1]]
train_ds = tmp_ds[ inTrain, c(var_list,160)]
valid_ds = tmp_ds[-inTrain,c(var_list,160)]

#lapply(train_ds, class)

# Random Forest
ModFit<-train(classe ~ ., train_ds, method="rf")
varImp(ModFit)
pred<-predict(ModFit, valid_ds[,-160])

confusionMatrix(valid_ds$classe, pred)  # Accuracy : 0.9925

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
sorted_sub_ds<-sub_ds[ do.call(order, sub_key), "classe"]

pred_test<-predict(ModFit, sorted_test_ds)
confusionMatrix(sorted_sub_ds, pred_test)  # Accuracy : 1

