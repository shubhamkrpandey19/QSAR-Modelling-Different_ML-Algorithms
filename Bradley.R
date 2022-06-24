library("Boruta")
require("Boruta")
library("caret")
library("dplyr")

#Data Collection

# Validation set
val1<-read.csv(file.choose(), header=TRUE)
val2<-read.csv(file.choose(), header=TRUE)
val3<-read.csv(file.choose(), header=TRUE)

Dataset<-read.csv(file.choose(), header=TRUE)
#Remove Names and target variables from Dataset
Dataset1<-Dataset[,-c(1:2)]
anyNA(Dataset)

#Check if any NA values, if yes then remove the column
Dataset1%>%select(where(~!any(is.na(.))))

dim(Dataset1)
View(Dataset)

#Preprocessing

#Variable having zero variance are removed from the dataset
Dataset1<-Dataset1[,-which(sapply(Dataset1[,1:16092],var)==0)]
dim(Dataset1)
#Near zero variance variables are removed
preProcess<-preProcess(Dataset1[,1:10625],method<-c("nzv"))
Dataset1<-predict(preProcess, Dataset1[,1:10625])
dim(Dataset1)
##Highly correlated variables are removed by setting correlation cutoff 70%
correlation<-cor(Dataset1[,1:2700])
highlycorrelated<-findCorrelation(correlation,cutoff = 0.9)
Dataset1<-Dataset1[,-highlycorrelated]
dim(Dataset1)

Dataset1<-cbind(Dataset$MP.Measured,Dataset1)
names(Dataset1)[names(Dataset1)=="Dataset$MP.Measured"]<-"MP"

#Data Splitting (Trainig & Test sets)
totaldata<-createDataPartition(Dataset1$MP, p=0.8,list = FALSE)
train<-Dataset1[totaldata,]
dim(train)
test<-Dataset1[-totaldata,]
dim(test)

#Features Selction
##Boruta algorithm based on wrappping mwthod was used as feature selection
All.Features<-Boruta(MP~.,train, pValue = 0.01, maxRuns = 100, num.threads=4)
Mdfy.Features<-TentativeRoughFix(All.Features)
getSelectedAttributes(Mdfy.Features)
getConfirmedFormula(Mdfy.Features)
train<-train[,c("MP",getSelectedAttributes(Mdfy.Features))]
dim(train)

#RF Model for multivariate regression
##10-fold cross validation is used to ensure unbaised system
trainControl<-trainControl(method = "cv", number = 10)
RF1<-train(train[,-1], train$MP, method="rf",trControl=trainControl, importance= T,
           tuneLength = 10, num.trees = 100)

#Performance evaluation of developed model
RF1
plot(RF1)

predic1<-predict(RF1, train)
RMSE(predic1,train$MP)
postResample(predic1,train$MP)

plot(train$MP, type = "l",lty=1.8, col="green")
lines(predic1, type = "l", col="blue")

predic2<-predict(RF1, test)
RMSE(predic2,test$MP)
postResample(predic2,test$MP)

val1RF1<-predict(RF1, val1)
RMSE(val1RF1,val1$MP)
postResample(val1RF1,val1$MP)

val2RF1<-predict(RF1, val2)
RMSE(val2RF1,val2$MP)
postResample(val2RF1,val2$MP)

val3RF1<-predict(RF1, val3)
RMSE(val3RF1,val3$MP)
postResample(val3RF1,val3$MP)

ctrl <- trainControl(
  method = "cv",
  number = 10,
)



#KNN Regression
KNN<- train(train[-1], train$MP, method='knn', preProcess=c("center","scale"), trControl=ctrl)

KNN
plot(KNN)

predic3<-predict(KNN, train)
RMSE(predic3,train$MP)
postResample(predic3,train$MP)

predic4<-predict(KNN, test)
RMSE(predic4,test$MP)
postResample(predic4,test$MP)

val1KNN<-predict(KNN, val1)
RMSE(val1KNN,val1$MP)
postResample(val1KNN,val1$MP)

val2KNN<-predict(KNN, val2)
RMSE(val2KNN,val2$MP)
postResample(val2KNN,val2$MP)

val3KNN<-predict(KNN, val3)
RMSE(val3KNN,val3$MP)
postResample(val3KNN,val3$MP)


library("hydroGOF")
t(gof(sim = predic3, obs = train$MP))

#Train SVM
# Grid search to fine tune SVM
grid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25))
SVM<-train(MP~., data=train, 
           method='svmRadial', 
           trControl=ctrl,  
           tunegrid=grid,
           preProcess = NULL)


#Performance evaluation of developed model
SVM
plot(SVM)
anyNA(test)

set.seed(1) 
predic5<-predict(SVM, train[,-1])
RMSE(predic5,train$MP)
postResample(predic5,train$MP)

predic6<-predict(SVM, test[,-1])
RMSE(predic6,test$MP)
postResample(predic6,test$MP)

val1SVM<-predict(SVM, val1[,-1])
RMSE(val1SVM,val1$MP)
postResample(val1SVM,val1$MP)

val2SVM<-predict(SVM, val2)
RMSE(val2SVM,val2$MP)
postResample(val2SVM,val2$MP)

val3SVM<-predict(SVM, val3)
RMSE(val3SVM,val3$MP)
postResample(val3SVM,val3$MP)

#Linear Regression
LR<- train(train[-1], train$MP, method='lm', preProcess=c("center","scale"), trControl=ctrl)

LR
plot(LR)

predic7<-predict(LR, train)
RMSE(predic7,train$MP)
postResample(predic7,train$MP)

predic8<-predict(LR, test)
RMSE(predic8,test$MP)
postResample(predic8,test$MP)

val1LR<-predict(LR, val1)
RMSE(val1LR,val1$MP)
postResample(val1LR,val1$MP)

val2LR<-predict(LR, val2)
RMSE(val2LR,val2$MP)
postResample(val2LR,val2$MP)

val3LR<-predict(LR, val3)
RMSE(val3LR,val3$MP)
postResample(val3LR,val3$MP)

#Ridge Regression
Ridge<- train(train[-1], train$MP, method='ridge', preProcess=c("center","scale"), trControl=ctrl)

Ridge
plot(Ridge)

predic9<-predict(Ridge, train)
RMSE(predic9,train$MP)
postResample(predic9,train$MP)

predic10<-predict(Ridge, test)
RMSE(predic10,test$MP)
postResample(predic10,test$MP)

val1Ridge<-predict(Ridge, val1)
RMSE(val1Ridge,val1$MP)
postResample(val1Ridge,val1$MP)

val2Ridge<-predict(Ridge, val2)
RMSE(val2Ridge,val2$MP)
postResample(val2Ridge,val2$MP)

val3Ridge<-predict(Ridge, val3)
RMSE(val3Ridge,val3$MP)
postResample(val3Ridge,val3$MP)

#Lasso Regression
Lasso<- train(train[-1], train$MP, method='lasso', preProcess=c("center","scale"), trControl=ctrl)

Lasso
plot(Lasso)

predic11<-predict(Lasso, train)
RMSE(predic11,train$MP)
postResample(predic11,train$MP)

predic12<-predict(Lasso, test)
RMSE(predic12,test$MP)
postResample(predic12,test$MP)

val1Lasso<-predict(Lasso, val1)
RMSE(val1Lasso,val1$MP)
postResample(val1Lasso,val1$MP)

val2Lasso<-predict(Lasso, val2)
RMSE(val2Lasso,val2$MP)
postResample(val2Lasso,val2$MP)

val3Lasso<-predict(Lasso, val3)
RMSE(val3Lasso,val3$MP)
postResample(val3Lasso,val3$MP)

#variable importance: to check which variables offering high predictive value in RF regression model
varImp(RF1, top=500)
impvariable<-varImp(RF1, top=500)$importance
print(impvariable)
write.csv(impvariable, file = "Best features.csv")


##building another model with top 500 features
a<-read.csv(file.choose()) ##Best features file
a<-a %>% arrange(desc(Overall))
name<-a$X
name<-as.character(name)
newdata<-train[,c("MP", name)]
newtraindata<-newdata[,-(502:952)]

newtestdata<-test[,c("MP", name)]
newtestdata<-newtestdata[,-(502:952)]

# #Model build up with 20 best features
# RF2<-train(newtraindata[,-1], newtraindata$MP, method="rf",trControl=trainControl, importance= T,
#            tuneLength = 5, num.trees = 100)
# Grid search to fine tune SVM
grid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25))
#Train SVM
ctrl <- trainControl(
  method = "cv",
  number = 10,
)
SVM<- train(newtraindata[,-1], newtraindata$MP,
            method = "svmRadial",
            tuneGrid = grid,
            trControl = ctrl)

#Performance evaluation of developed model
SVM
plot(SVM)

predic3<-predict(SVM, newtraindata)
RMSE(predic3,newtraindata$MP)
postResample(predic3,newtraindata$MP)

predic4<-predict(SVM, newtestdata)
RMSE(predic4,newtestdata$MP)
postResample(predic4,newtestdata$MP)

#validation with ademola data
validationset<-read.csv(file.choose(), header=TRUE)
validationdata<-validationset[,c("MP", name)]
validationdata<-validationdata[,-(502:952)]
predict5 = predict(SVM, newdata = validationdata[,-1])
RMSE(predict5,validationdata$MP)
postResample(predict5,validationdata$MP)
