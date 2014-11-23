library(caret)
library(randomForest)

# setwd("./project")
tmp_ds<-read.csv("pml-training.csv", na.strings=c("NA","","#DIV/0!"))
test_ds<-read.csv("pml-testing.csv", na.strings=c("NA","","#DIV/0!"))

set.seed(12345)
inTrain = createDataPartition(tmp_ds$classe, p = 0.1)[[1]]
train_ds = tmp_ds[ inTrain, ]
valid_ds = tmp_ds[-inTrain, ]

var_list<-c(8:11,37:45,46:49,60:68,84:86,102,113:121,122:124,140,151:159)
train_classe<-train_ds[,160]
train_ds<-train_ds[,var_list]

valid_classe<-valid_ds[,160]
valid_ds<-valid_ds[,var_list]

# 先删去近似于常量的变量
zerovar <- nearZeroVar(x1)

# 再删去相关度过高的自变量
highCorr<-findCorrelation(cor(train_ds), 0.90)
train_ds<-train_ds[,-highCorr]

# 数据预处理步骤（标准化，缺失值处理）
Process <- preProcess(train_ds)
trainPC <- predict(Process, train_ds)

# tuning the parameter
trCtrl=trainControl(method = "cv", returnResamp="all", repeats=3)
fit<-train(train_ds, train_classe, method = "rf", trControl=trCtrl)

# cross-valaid, 10 folder which is default
ctrl<-rfeControl(functions=rfFuncs, returnResamp="all", method="cv")
rfProfile <- rfe(trainPC, train_classe, sizes=c(1:ncol(trainPC)), rfeControl=ctrl)

rfProfile
# get a text string of variable names that were picked in the final model
predictors(rfProfile)

# predict 
pred<-predict(rfProfile, valid_ds)

# Calculates performance
postResample(pred, valid_classe)

# produces the performance profile across different subset sizes
trellis.par.set(caretTheme())
plot(rfProfile,type=c('o','g'))
xyplot(rfProfile, type = c("g", "p", "smooth"))


http://topepo.github.io/caret/featureselection.html

