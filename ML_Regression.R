library("Boruta")
require("Boruta")
library("caret")
library("dplyr")

# Validation set
#Validation set list
#val1=23common solvent
#val2=LC updated list 415 datapoints
#val3=LC Mw<500 178 datapoints
#val4=LC Mw>500 237 datapoints
#val5=LC Nitrile present 121 datapoints
#val6=LC Nitrile absent 294 datapoints


#Remove NA values from the validation set using na.omit
val1<-na.omit(read.csv("C:\\Users\\skpandey1\\OneDrive - The University of Alabama\\Machine Learning Project\\Melting Point Point Prediction\\Validation set\\SE descriptors\\val1_23commonsolvents.csv" , header=TRUE))
val2<-na.omit(read.csv("C:\\Users\\skpandey1\\OneDrive - The University of Alabama\\Machine Learning Project\\Melting Point Point Prediction\\Validation set\\SE descriptors\\val3_new.csv" , header=TRUE))
val3<-na.omit(read.csv("C:\\Users\\skpandey1\\OneDrive - The University of Alabama\\Machine Learning Project\\Melting Point Point Prediction\\Validation set\\SE descriptors\\LC-1.csv" , header=TRUE))
val4<-na.omit(read.csv("C:\\Users\\skpandey1\\OneDrive - The University of Alabama\\Machine Learning Project\\Melting Point Point Prediction\\Validation set\\SE descriptors\\LC-2.csv" , header=TRUE))
val5<-na.omit(read.csv("C:\\Users\\skpandey1\\OneDrive - The University of Alabama\\Machine Learning Project\\Melting Point Point Prediction\\Validation set\\SE descriptors\\LCwithNitrile.csv" , header=TRUE))
val6<-na.omit(read.csv("C:\\Users\\skpandey1\\OneDrive - The University of Alabama\\Machine Learning Project\\Melting Point Point Prediction\\Validation set\\SE descriptors\\LCwithoutNitrile.csv" , header=TRUE))



#Data Collection

# Label<-read.csv("C:\\Users\\skpandey1\\OneDrive - The University of Alabama\\Machine Learning Project\\Melting Point Point Prediction\\Datasets\\Nitrile-BULK.csv", header=TRUE)
Dataset<-na.omit(read.csv("C:\\Users\\skpandey1\\OneDrive - The University of Alabama\\Machine Learning Project\\Melting Point Point Prediction\\Datasets\\SE_descriptor_Nitrile_dataset\\Nitrile_SE.csv", header=TRUE))

#Remove Names and target variables from Dataset
Dataset1<-Dataset[,-1]
anyNA(Dataset1)

#Preprocessing

# #Variable having zero variance are removed from the dataset
# Dataset1<-Dataset1[,-which(sapply(Dataset1[,1:ncol(Dataset1)],var)==0)]
# dim(Dataset1)
# #Near zero variance variables are removed
# preProcess<-preProcess(Dataset1[,1:ncol(Dataset1)],method<-c("nzv"))
# Dataset1<-predict(preProcess, Dataset1[,1:ncol(Dataset1)])
# dim(Dataset1)
# #Highly correlated variables are removed by setting correlation cutoff 90%
# correlation<-cor(Dataset1[,1:ncol(Dataset1)])
# highlycorrelated<-findCorrelation(correlation,cutoff = 0.9)
# Dataset1<-Dataset1[,-highlycorrelated]
# dim(Dataset1)

#Data normalisation using Mind 
preProcess<-preProcess(Dataset1[,1:ncol(Dataset1)],method<-c("range"))
dim(Dataset1)

transformed<-predict(preProcess, Dataset1[,1:ncol(Dataset1)])
Dataset1<-transformed
dim(Dataset1)



Dataset1<-cbind(Dataset$MP,Dataset1)
names(Dataset1)[names(Dataset1)=="Dataset$MP"]<-"MP"

#Data Splitting (Trainig & Test sets[80:20])
totaldata<-createDataPartition(Dataset1$MP, p=0.9,list = FALSE)
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


ctrl <- trainControl(
  method = "cv",
  number = 10,
)



#KNN Regression
KNN<- train(train[-1], train$MP, method='knn', preProcess=c("center","scale"), trControl=ctrl)

KNN
plot(KNN)


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



#Linear Regression
LR<- train(train[-1], train$MP, method='lm', preProcess=c("center","scale"), trControl=ctrl)

LR
plot(LR)



#Ridge Regression
Ridge<- train(train[-1], train$MP, method='ridge', preProcess=c("center","scale"), trControl=ctrl)

Ridge
plot(Ridge)


#Lasso Regression
Lasso<- train(train[-1], train$MP, method='lasso', preProcess=c("center","scale"), trControl=ctrl)

Lasso
plot(Lasso)


#end




#Results
library("ggplot2")
#list1 algorithms that i used
list1 <- list(RF1, SVM, KNN, LR, Ridge, Lasso)
list11 <- c("RF", "SVM", "KNN", "LR", "Ridge", "Lasso")
#list2 validation sets
list2 <- list(train, test, val1, val2, val3, val4, val5, val6)
list22 <- c("train", "test" ,"val1", "val2", "val3", "val4", "val5", "val6")
store=NULL
for (i in 1:length(list1)) {
  for (j in 1:length(list2)) {
    train_col=colnames(train)
    temp=list2[[j]] ##consider only those columns that are in train set (best features)
    ##Labeled those results as modified
    temp=temp[,which(colnames(temp) %in% train_col)]
    x <- predict(list1[[i]],temp)
    
    store=rbind(store,data.frame(Model=list11[i],Set=list22[j],t(postResample(x, temp$MP))))
    data1=data.frame(x,y=temp$MP)
    
    ggplot(data =data1 ,aes(x=y,y=x))+geom_point(color='black')+ylab('Observed')+xlab("Predicted")+xlim(min(data1$x,data1$y),max(data1$x,data1$y))+
      ylim(min(data1$x,data1$y),max(data1$x,data1$y))+ggtitle(paste0(list11[i],' ',list22[j]))+
      theme(plot.title = element_text(hjust = 0.5))+geom_abline(slope=1, intercept = 0)+
      geom_smooth(method = "lm", se = FALSE, color='red')
    ggsave(paste0('C:/Users/skpandey1/OneDrive - The University of Alabama/Machine Learning Project/Melting Point Point Prediction/Nitrile_SE/Nitrile90-10_SE',list11[i],'_',list22[j],'.jpeg'), width = 5, height = 5, units = "in")     
    
  }
}
openxlsx::write.xlsx(store,'C:/Users/skpandey1/OneDrive - The University of Alabama/Machine Learning Project/Melting Point Point Prediction/Nitrile_SE/Nitrile90-10_SE_results.xlsx',overwrite=T)

#XGBoost

install.packages('xgboost') 
library(xgboost)
train_x = data.matrix(train[, -1])
test_x = data.matrix(test[, -1])
train_y = train[,1]
test_y = test[,1]


m1_xgb <-
  xgboost(
    data = train_x,
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .25
  )


pred_xgb <-predict(m1_xgb, train_x)
RMSE(pred_xgb,train_y)
postResample(pred_xgb,train_y)



#Results
library("ggplot2")
#list1 algorithms that i used
list1 <- list(m1_xgb)
list11 <- c("m1_xgb")
#list2 validation sets
list2 <- list(train, test, val1, val2, val3, val4, val5, val6)
list22 <- c("train", "test", "val1", "val2", "val3", "val4", "val5", "val6")
store=NULL
for (i in 1:length(list1)) {
  for (j in 1:length(list2)) {
    
    train_col=colnames(train)
    temp=list2[[j]] ##consider only those columns that are in train set (best features)
    temp=temp[,which(colnames(temp) %in% train_col)]
    
    x <- predict(list1[[i]],data.matrix(temp[, -1]))
    store=rbind(store,data.frame(Model=list11[i],Set=list22[j],t(postResample(x, temp$MP))))
    data1=data.frame(x,y=temp$MP)
    
    ggplot(data =data1 ,aes(x=y,y=x))+geom_point(color='black')+ylab('Observed')+xlab("Predicted")+xlim(min(data1$x,data1$y),max(data1$x,data1$y))+
      ylim(min(data1$x,data1$y),max(data1$x,data1$y))+ggtitle(paste0(list11[i],' ',list22[j]))+
      theme(plot.title = element_text(hjust = 0.5))+geom_abline(slope=1, intercept = 0)+
      geom_smooth(method = "lm", se = FALSE, color='red')
    ggsave(paste0('C:/Users/skpandey1/OneDrive - The University of Alabama/Machine Learning Project/Melting Point Point Prediction/Nitrile_SE/XGB_Nitrile90-10_SE',list11[i],'_',list22[j],'.jpeg'), width = 5, height = 5, units = "in")     
    
  }
}
openxlsx::write.xlsx(store,'C:/Users/skpandey1/OneDrive - The University of Alabama/Machine Learning Project/Melting Point Point Prediction/Nitrile_SE/XGB_Nitrile90-10_SE.xlsx',overwrite=T)
