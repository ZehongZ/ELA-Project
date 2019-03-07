#Import data
myd=read.csv("HR.csv", header=T)
head(myd)
dim(myd)
#Check levels for variables
levels(myd$sales)
levels(myd$salary)
#Install library
library(e1071)
library(caret)
library(mlbench)
library(pROC)
library(ROCR)
#Visualize dataset
pairs(myd)
hist(myd$number_project)
hist(myd$average_montly_hours)
hist(myd$time_spend_company)

#Visualize dataset
pairs(subset)
plot(myd$left~myd$satisfaction_level)
plot(myd$left~myd$last_evaluation)

#Missing values plot
library(VIM)
library(mice)
aggr(myd, prop=FALSE, number=TRUE)
matrixplot(myd)

#Normalize variables
myd$average_montly_hours=scale(myd$average_montly_hours, center = TRUE, scale=TRUE)
myd$last_evaluation=scale(myd$last_evaluation, center=TRUE, scale=TRUE)

#Build dummy variables for salary
levels(myd$salary)
myd$high=ifelse(myd$salary=="high",1,0)
myd$low=ifelse(myd$salary=="low",1,0)
myd$medium=ifelse(myd$salary=="medium",1,0)
head(myd)

#Build dummy variables fr sales
levels(myd$sales)
myd$accounting=ifelse(myd$sales=="accounting",1,0)
myd$hr=ifelse(myd$sales=="hr",1,0)
myd$IT=ifelse(myd$sales=="IT",1,0)
myd$management=ifelse(myd$sales=="management",1,0)
myd$marketing=ifelse(myd$sales=="marketing",1,0)
myd$product_mng=ifelse(myd$sales=="product_mng",1,0)
myd$RandD=ifelse(myd$sales=="RandD",1,0)
myd$SALES=ifelse(myd$sales=="sales",1,0)
myd$support=ifelse(myd$sales=="support",1,0)
myd$technical=ifelse(myd$sales=="technical",1,0)
head(myd)

#Build a subset exclude sales and salary
mydc=myd[c("satisfaction_level","last_evaluation","number_project","average_montly_hours","time_spend_company","Work_accident","left","promotion_last_5years","high","low","medium","accounting","hr","IT","management","marketing","product_mng","RandD","SALES","support","technical")]

#Build traingset and testing set
#Build a training and testing set
train.size=round(0.66*nrow(myd))
id.train=sample(1:nrow(myd), train.size, replace = FALSE)
train=mydc[id.train,]
test=mydc[-id.train,]

#Build model hr (all vairables)
hr=lm(left~., data=train)
summary(hr)

#Test model on training set
train_hr=predict(hr, newdata=train,type="response")
confusionMatrix(data=as.numeric(train_hr>0.5),reference = train$left)
perf_train_hr=prediction(as.numeric(train_hr),as.numeric(train$left))
pred_train_hr=performance(perf_train_hr, "tpr","fpr")
plot(pred_train_hr,colorize=TRUE,main="ROC Curve")
auc_train_hr=performance(perf_train_hr,measure = "auc")
auc_train_hr@y.values[[1]]
#Test model on testing set
test_hr=predict(hr, newdata=test, type="response")
confusionMatrix(data=as.numeric(test_hr>0.5),reference = test$left)
perf_test_hr=prediction(as.numeric(test_hr),as.numeric(test$left))
pred_test_hr=performance(perf_test_hr, "tpr","fpr")
plot(pred_test_hr,colorize=TRUE,main="ROC Curve")
auc_test_hr=performance(perf_test_hr,measure = "auc")
auc_test_hr@y.values[[1]]

#hr2-manual selection
summary(hr)
hr2=lm(train$left~train$satisfaction_level+train$last_evaluation+train$number_project+train$average_montly_hours+train$time_spend_company+train$Work_accident+train$promotion_last_5years+train$high+train$low+train$IT+train$management+train$RandD)
summary(hr2)
#Test hr2 on training set
train_hr2=predict(hr2,newdata=train, type="response")
confusionMatrix(data=as.numeric(train_hr2>0.5),reference = train$left)
perf_train_hr2=prediction(as.numeric(train_hr2),as.numeric(train$left))
pred_train_hr2=performance(perf_train_hr2, "tpr","fpr")
plot(pred_train_hr2,colorize=TRUE,main="ROC Curve")
auc_train_hr2=performance(perf_train_hr2,measure = "auc")
auc_train_hr2@y.values[[1]]
#Test hr2 on testing set
test_hr2=predict(hr2, newdata=test, type="response")
confusionMatrix(data=as.numeric(test_hr2>0.5), reference = train$left)
perf_test_hr2=prediction(as.numeric(test_hr2),as.numeric(train$left))
pred_test_hr2=performance(perf_test_hr2, "tpr","fpr")
plot(pred_test_hr2,colorize=TRUE,main="ROC Curve")
auc_test_hr2=performance(perf_test_hr2,measure = "auc")
auc_test_hr2@y.values[[1]]

#Hr3--stepwise selection
#Peform model selection (backward)
library(MASS)
full.model=hr
hr3=step(hr, direction = "both")
summary(hr3)
#Test hr3 on training set
train_hr3=predict(hr3, newdata=train)
confusionMatrix(data=as.numeric(train_hr3>0.5), reference=train$left)
perf_train_hr3=prediction(as.numeric(train_hr3),as.numeric(train$left))
pred_train_hr3=performance(perf_train_hr3, "tpr","fpr")
plot(pred_train_hr3,colorize=TRUE,main="ROC Curve")
auc_train_hr3=performance(perf_train_hr3,measure = "auc")
auc_train_hr3@y.values[[1]]
#Test hr3 on testing set
test_hr3=predict(hr3, newdata=test)
confusionMatrix(data=as.numeric(test_hr3>0.5), reference = test$left)
perf_test_hr3=prediction(as.numeric(test_hr3),as.numeric(test$left))
pred_test_hr3=performance(perf_test_hr3, "tpr","fpr")
plot(pred_test_hr3,colorize=TRUE,main="ROC Curve")
auc_test_hr3=performance(perf_test_hr3,measure = "auc")
auc_test_hr3@y.values[[1]]


##Logistic regression
hr4=glm(left~., data=train, family=binomial(link=logit) )
summary(hr4)

#Test Hr4
train_hr4=predict(hr4, newdata=train)
confusionMatrix(data=as.numeric(train_hr4>0.5), reference = train$left)
perf_train_hr4=prediction(as.numeric(train_hr4),as.numeric(train$left))
pred_train_hr4=performance(perf_train_hr4, "tpr","fpr")
plot(pred_train_hr4,colorize=TRUE,main="ROC Curve")
auc_train_hr4=performance(perf_train_hr4,measure = "auc")
auc_train_hr4@y.values[[1]]
#Test hr4 on testing set
test_hr4=predict(hr4, newdata=test)
confusionMatrix(data=as.numeric(test_hr4>0.5), reference = test$left)
perf_test_hr4=prediction(as.numeric(test_hr4),as.numeric(test$left))
pred_test_hr4=performance(perf_test_hr4, "tpr","fpr")
plot(pred_test_hr4,colorize=TRUE,main="ROC Curve")
auc_test_hr4=performance(perf_test_hr4,measure = "auc")
auc_test_hr4@y.values[[1]]

#hr5
hr5=glm(left~satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years+high+low+medium+hr+IT+management+RandD, data=train, family=binomial(link=logit) )
summary(hr5)
#Test hr5 on train
train_hr5=predict(hr5, newdata = train)
confusionMatrix(data=as.numeric(train_hr5>0.5), reference = train$left)
perf_train_hr5=prediction(as.numeric(train_hr5),as.numeric(train$left))
pred_train_hr5=performance(perf_train_hr5, "tpr","fpr")
plot(pred_train_hr5,colorize=TRUE,main="ROC Curve")
auc_train_hr5=performance(perf_train_hr5,measure = "auc")
auc_train_hr5@y.values[[1]]
#Test hr5 on testing
testsub=test[c('satisfaction_level','last_evaluation','number_project','average_montly_hours','time_spend_company','Work_accident','promotion_last_5years','high','low','medium','hr','IT','management','RandD')]
test_hr5=predict(hr5, newdat=test)
confusionMatrix(data=as.numeric(test_hr5>0.5), reference = test$left)
perf_test_hr5=prediction(as.numeric(test_hr5),as.numeric(test$left))
pred_test_hr5=performance(perf_test_hr5, "tpr","fpr")
plot(pred_test_hr5,colorize=TRUE,main="ROC Curve")
auc_test_hr5=performance(perf_test_hr5,measure = "auc")
auc_test_hr5@y.values[[1]]

#hr6
hr6=step(hr4, direction = "both")
summary(hr6)
#Test hr6 on training
train_hr6=predict(hr6, newdata = train)
confusionMatrix(data=as.numeric(train_hr6>0.5), reference = train$left)
perf_train_hr6=prediction(as.numeric(train_hr6),as.numeric(train$left))
pred_train_hr6=performance(perf_train_hr6, "tpr","fpr")
plot(pred_train_hr6,colorize=TRUE,main="ROC Curve")
auc_train_hr6=performance(perf_train_hr6,measure = "auc")
auc_train_hr6@y.values[[1]]
#Test hr6 on training
test_hr6=predict(hr6, newdat=test)
confusionMatrix(data=as.numeric(test_hr6>0.5), reference = test$left)
perf_test_hr6=prediction(as.numeric(test_hr6),as.numeric(test$left))
pred_test_hr6=performance(perf_test_hr6, "tpr","fpr")
plot(pred_test_hr6,colorize=TRUE,main="ROC Curve")
auc_test_hr6=performance(perf_test_hr6,measure = "auc")
auc_test_hr6@y.values[[1]]

#Reorder columns
names(myd)
myd[c(7,1,2,3,4,5,6,8,9,10,11,12,13)]
myd$left=as.factor(myd$left)
#Perform Naive Bayes on training set
library(caret)
library(e1071)
library(mlbench)
#Split the dataset again
train.id=createDataPartition(y=myd$left, p=0.66, list=FALSE)
myd.train=myd[train.id,]
myd.test=myd[-train.id,]
#Compute Naive Bayes on Training set
train.naive=naiveBayes(left~., data=myd.train)
p.train=predict(train.naive, myd.train[,2:13], type="class")
train.accuracy=table(p.train, myd.train$left)
train.accuracy
confusionMatrix(train.accuracy, reference=myd.train$left)
1-sum(diag(train.accuracy))/sum(train.accuracy)
perf_p.train=prediction(as.numeric(p.train),as.numeric(myd.train$left))
pred_p.train=performance(perf_p.train, "tpr","fpr")
plot(pred_p.train,colorize=TRUE,main="ROC Curve")
auc_p.train=performance(perf_p.train,measure = "auc")
auc_p.train@y.values[[1]]
#Compute Naive Bayes on Testing set
p.test=predict(train.naive, myd.test[,2:13], type="class")
test.accuracy=table(p.test,myd.test$left)
confusionMatrix(p.test, reference = myd.test$left)
test.accuracy
1-sum(diag(test.accuracy))/sum(test.accuracy)
perf_p.test=prediction(as.numeric(p.test),as.numeric(myd.test$left))
pred_p.test=performance(perf_p.test, "tpr","fpr")
plot(pred_p.test,colorize=TRUE,main="ROC Curve")
auc_p.test=performance(perf_p.test,measure = "auc")
auc_p.test@y.values[[1]]

#Perform linear Random Forest on training
library(randomForest)
forest=randomForest(as.factor(left)~., data=myd.train, importance=TRUE, ntree=100)
train.pred=predict(forest, newdata=myd.train)
confusionMatrix(train.pred, reference = myd.train$left)
perf_train.pred=prediction(as.numeric(train.pred),as.numeric(myd.train$left))
pred_p.train=performance(perf_p.train, "tpr","fpr")
plot(pred_p.train,colorize=TRUE,main="ROC Curve")
auc_p.train=performance(perf_p.train,measure = "auc")
auc_p.train@y.values[[1]]

#Perform linear Random Forest on testing
test.forest=randomForest(as.factor(left)~., data=myd.test, importance=TRUE, ntree=100)
test.pred=predict(test.forest, newdata =myd.test)
confusionMatrix(test.pred, reference = myd.test$left)
perf_test.pred=prediction(as.numeric(test.pred),as.numeric(myd.test$left))
pred_p.test=performance(perf_p.test, "tpr","fpr")
plot(pred_p.test,colorize=TRUE,main="ROC Curve")
auc_p.test=performance(perf_p.test,measure = "auc")
auc_p.test@y.values[[1]]


#Perform decision tree on training set
library(rpart)
dtree=rpart(left~., data=train, method="class", parms=list(split="gini"))
#Testing decision tree on training set
train.pred=predict(dtree, newdata=train, type="class")
#Compute misclassification matrix 
train.perf=table(train$left, train.pred, dnn=c("Acutal", "Predicted"))
#Print misclassification matrix
prop.table(train.perf,1)
accuracy=sum(diag(train.perf))/sum(train.perf)
accuracy
#Testing decision tree on testing set
test.pred=predict(dtree, newdata=test, type="class")
#Compute misclassification matrix
test.perf=table(test$left, test.pred, dnn=c("Actual","Prediction"))
#Print misclassification matrix
prop.table(test.perf,1)
accuracy.test=sum(diag(test.perf))/sum(test.perf)
accuracy.test
#Visualize decision tree
library(rpart.plot)
prp(dtree, type=4, extra=104, fallen.leaves=FALSE, main="Decision Tree", faclen=0)



#Visualize decision tree
library(rpart.plot)
prp(dtree, type=4, extra=104, fallen.leaves=FALSE, main="Decision Tree", faclen=0)
