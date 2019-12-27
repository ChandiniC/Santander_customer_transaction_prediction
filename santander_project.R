#import libraries and set working directory
rm(list=ls())
setwd("C:/Users/chandini c/Desktop")
getwd()
x = c("ggplot2","glmnet","pROC", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#Load Libraries
lapply(x, require, character.only = TRUE)
rm(x)

#Load Data Sets
train = read.csv("train.csv")
test=read.csv("test.csv")
head(train)

#Dimension of data sets
dim(train)
dim(test)

#Summary of the dataset
str(train)
str(test)

#convert to factor
train$target=as.factor(train$target)

require(gridExtra)
#Count of target classes
table(train$target)
#Percenatge counts of target classes
table(train$target)/length(train$target)*100
#Bar plot for count of target classes
plot1=ggplot(train,aes(target))+theme_bw()+geom_bar(stat='count',fill='lightgreen')
grid.arrange(plot1)

###MISSING VALUE ANALYSIS
#Finding the missing values in train data
missing_val=data.frame(missing_val=apply(train,2,function(x){sum(is.na(x))}))
missing_val=sum(missing_val)
missing_val

#Finding the missing values in test data
missing_val=data.frame(missing_val=apply(test,2,function(x){sum(is.na(x))}))
missing_val=sum(missing_val)
missing_val


##OUTLIER ANALYSIS
#Outlier Analysis in train dataset
numeric_index=sapply(train,is.numeric)
numeric_data=train[,numeric_index]
numerical_features=colnames(numeric_data)

#Removal of outliers in train data:
for(i in numerical_features){
  val=train[,i][train[,i]%in%boxplot.stats(train[,i])$out]
  train=train[which(!train[,i]%in%val),]
}

#Outlier Analysis in test dataset
numeric_index=sapply(test,is.numeric)
numeric_data=test[,numeric_index]
numerical_features=colnames(numeric_data)

#Removal of outliers in test data:
for(i in numerical_features){
  val=test[,i][test[,i]%in%boxplot.stats(test[,i])$out]
  test=test[which(!test[,i]%in%val),]
}


##CORRELATION ANALYSIS
#Correlations in train data
#convert factor to int
train$target=as.numeric(train$target)
train_correlations=cor(train[,c(2:202)])
train_correlations

#Correlations in test data
test_correlations=cor(test[,c(2:201)])
test_correlations


##VISUALIZATIONS 
#Distribution of train attributes
for (var in names(train)[c(3:202)]){
  target=train$target
  plot=ggplot(train, aes(x=train[[var]],fill=target)) +
    geom_density(kernel='gaussian') + ggtitle(var)+theme_classic()
  print(plot)
}
#Distribution of test attributes
for (var in names(test)[c(2:201)]){
  target=test$target
  plot=ggplot(test, aes(x=test[[var]])) +
    geom_density(kernel='gaussian') + ggtitle(var)+theme_classic()
  print(plot)
}


#Applying the function to find mean values per column in train and test data.
train_mean=apply(train[,-c(1,2)],MARGIN=2,FUN=mean)
test_mean=apply(test[,-c(1)],MARGIN=2,FUN=mean)
ggplot()+
  
#Distribution of mean values per column in train data
geom_density(aes(x=train_mean),kernel='gaussian',show.legend=TRUE,color='blue')+theme_classic()+

#Distribution of mean values per column in test data
geom_density(aes(x=test_mean),kernel='gaussian',show.legend=TRUE,color='green')+
labs(x='mean values per column',title="Distribution of mean values per row in train and test dataset")


#Applying the function to find sd values per column in train and test data.
train_sd=apply(train[,-c(1,2)],MARGIN=2,FUN=sd)
test_sd=apply(test[,-c(1)],MARGIN=2,FUN=sd)
ggplot()+

#Distribution of sd values per column in train data
geom_density(aes(x=train_sd),kernel='gaussian',show.legend=TRUE,color='red')+theme_classic()+
  #Distribution of sd values per column in test data
geom_density(aes(x=test_sd),kernel='gaussian',show.legend=TRUE,color='blue')+
labs(x='sd values per column',title="Distribution of std values per column in train and test dataset")


#Applying the function to find skewness values per column in train and test data.
train_skew=apply(train[,-c(1,2)],MARGIN=2,FUN=skewness)
test_skew=apply(test[,-c(1)],MARGIN=2,FUN=skewness)
ggplot()+

#Distribution of skewness values per column in train data
  geom_density(aes(x=train_skew),kernel='gaussian',show.legend=TRUE,color='green')+theme_classic()+
  
#Distribution of skewness values per column in test data
  geom_density(aes(x=test_skew),kernel='gaussian',show.legend=TRUE,color='blue')+
  labs(x='skewness values per column',title="Distribution of skewness values per column in train and test dataset")

##MODELING##
#splitting the data using stratified sampling method:
rm(list=c('numeric_data','test_correlations','train_correlations','i','numeric_index','numerical_features','val'))
set.seed(1234)

#convert to factor
train$target=as.factor(train$target)
train_index=createDataPartition(train$target,p=.70,list=FALSE)
train_data=train[train_index,]

#validation data
valid_data=train[-train_index,]

#dimension of train and validation data
dim(train_data)
dim(valid_data)

##RANDOM FOREST:
#convert to int to factor
train_data$target=as.factor(train_data$target)

#Random Forest Model on train data
RF_model=randomForest(target~.,train_data[,-c(1)],importance=TRUE,ntree=10)

#Model performance on validation data set
RF_pred=predict(RF_model,valid_data[,-2])

#confusion matrix
confmatrix_RF=table(valid_data$target,RF_pred)
confusionMatrix(confmatrix_RF)

#Accuracy=90.22
#FNR=(FN*100)/(FN+TP)=50.86
(147*100)/(147+142)
#Recall=49.13
(142*100)/(142+147)

#ROC_AUC score and curve
set.seed(843)
#convert to numeric
RF_pred=as.numeric(RF_pred)
roc(valid_data$target,predictor=RF_pred,auc=TRUE,plot=TRUE)
#AUC=51.23

#LOGISTIC REGRESSION:
#Training dataset
X_t=as.matrix(train_data[,-c(1,2)])
y_t=as.matrix(train_data$target)

#validation dataset
X_v=as.matrix(valid_data[,-c(1,2)])
y_v=as.matrix(valid_data$target)

#test dataset
test_df=as.matrix(test[,-c(1)])

#logistic regression model
library(glmnet)
set.seed(667) # to reproduce results
lr_model=glmnet(X_t,y_t, family = "binomial")
summary(lr_model)

#cross validation:
cv_lr=cv.glmnet(X_t,y_t,family = "binomial", type.measure = "class")

#Model performance on validation dataset
set.seed(5363)
cv_predict=predict(cv_lr,X_v,type = "class")

#Confusion matrix
set.seed(689)

#actual target variable
target=valid_data$target

#convert to factor
target=as.factor(target)

#predicted target variable
#convert to factor
cv_predict=as.factor(cv_predict)
confMat=table(valid_data$target,cv_predict)
confusionMatrix(confMat)

#Accuracy=91.56%
#Recall=74.87
#FNR=(FN*100)/(FN+TP)=25.12
(353*100)/(353+1052)
#Recall=(TP*100)/(TP+FN)=74.87
(1052*100)/(1052+353)
#ROC_AUC score and curve
set.seed(843)
#convert to numeric
cv_predict=as.numeric(cv_predict)
roc(valid_data$target,predictor=cv_predict,auc=TRUE,plot=TRUE)
#AUC=59.55

#Naive bayes
library(e1071)
#Develop model
NB_model=naiveBayes(target~.,data=train_data)
#Predict on test cases
NB_pred=predict(NB_model,X_v,type="class")
#Confusion matrix
confmatrix_NB=table(observed=y_v,predicted=NB_pred)
confusionMatrix(confmatrix_NB)

#Accuracy=92.33%
#FNR=24.5
#Recall=71.77
(713*100)/(713+1813)
(1813*100)/(713+1813)

#ROC_AUC score and curve
set.seed(843)
#convert to numeric
NB_pred=as.numeric(NB_pred)
roc(valid_data$target,predictor=NB_pred,auc=TRUE,plot=TRUE)
#AUC=66.92

#Random Oversampling Examples(ROSE) for Logistic Reg
library(ROSE)
set.seed(699)
train.rose = ROSE(target~., data =train_data[,-c(1)],seed=32)$data

#target classes in balanced train data
table(train.rose$target)
valid.rose = ROSE(target~., data =valid_data[,-c(1)],seed=42)$data

#target classes in balanced valid data
table(valid.rose$target)

#Logistic regression model
set.seed(462)
lr_rose =glmnet(data.matrix(train.rose),data.matrix(train.rose$target), family = "binomial")
summary(lr_rose)

#Cross validation prediction
set.seed(473)
cv_rose = cv.glmnet(data.matrix(valid.rose),data.matrix(valid.rose$target),family = "binomial", type.measure = "class")
cv_rose

#Model performance on validation dataset
set.seed(442)
predict.rose=predict(cv_rose,data.matrix(valid.rose),s = "lambda.min", type = "class")
predict.rose

#Confusion matrix
set.seed(478)
#actual target variable
target=valid.rose$target
#convert to factor
target=as.factor(target)
#predicted target variable
#convert to factor
predict.rose=as.factor(predict.rose)
#Confusion matrix
confmat_rose=table(valid.rose$target,predict.rose)
confusionMatrix(confmat_rose)
#accuracy=100%

#ROC_AUC score and curve
set.seed(843)
#convert to numeric
predict.rose=as.numeric(predict.rose)
roc(valid.rose$target,predictor=predict.rose,auc=TRUE,plot=TRUE)
#AUC=100
