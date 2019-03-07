#Import data
myd=read.csv("HR.csv", header=T)
head(myd)
dim(myd)
#Check levels for variables
levels(myd$sales)
levels(myd$salary)

#Visualize dataset
pairs(myd)
hist(myd$number_project)
hist(myd$average_montly_hours)
hist(myd$time_spend_company)

#Visualize dataset
pairs(subset)
plot(myd$last_evaluation,myd$satisfaction_level)
plot(myd$average_montly_hours,myd$satisfaction_level)
plot(myd$time_spend_company,myd$satisfaction_level)

#Missing values plot
library(VIM)
library(mice)
aggr(myd, prop=FALSE, number=TRUE)
matrixplot(myd)

#Correlation matrix
m=cor(subset)

#Plot correlation values
library(corrplot)
corrplot(m, method="ellipse")
##Satisfaction level have light positive correlation with last evalution; light negative correlation with number of projects, average hours and time spend on company

#Normalize variables
myd$average_montly_hours=scale(myd$average_montly_hours, center = TRUE, scale=TRUE)
myd$last_evaluation=scale(myd$last_evaluation, center=TRUE, scale=TRUE)

#Build dummy variables for salary
levels(myd$salary)
myd$high=ifelse(myd$salary=="high",1,0)
myd$low=ifelse(myd$salary=="low",1,0)
myd$medium=ifelse(myd$salary=="medium",1,0)
head(myd)

#Build a training and testing set
train.size=round(0.66*nrow(myd))
id.train=sample(1:nrow(myd), train.size, replace = FALSE)
train=myd[id.train,]
test=myd[-id.train,]

#Build a model on training set
hr=lm(train$satisfaction_level~train$last_evaluation+train$number_project+train$average_montly_hours+train$time_spend_company+train$Work_accident+train$promotion_last_5years+train$sales+train$high+train$low+train$medium)
summary(hr)

#Testing hr on training set
prediction=predict(hr, data=train, type="response")
perf=table(train$satisfaction_level, prediction, dnn=c("Actual","Prediction"))
length(train$satisfaction_level)
length(prediction)
results=ifelse(perf>0.5,1,0)
misClassError=mean(results!=train$satisfaction_level)
print(paste('Accuracy',1-misClassError))


#Testing hr on testing set
prediction.test=predict(hr, data=test, type="response")
perf.test=table(train$satisfaction_level, prediction.test, dnn=c("Actual","Prediction"))
results.test=ifelse(perf>0.5,1,0)
misClassError.test=mean(results.test!=test$satisfaction_level)
print(paste('Accuracy',1-misClassError.test))
###Significant variables: Last evaluation, number of project, time spend on company, accident, promotion, low

#Rebuild the data on significant variables
hr2=lm(train$satisfaction_level~train$last_evaluation+train$number_project+train$time_spend_company+train$Work_accident+train$promotion_last_5years+train$low)
summary(hr2)

#Testing hr2 on training set
prediction.hr2.train=predict(hr2,data=train,type="response")
perf.hr2.train=table(train$satisfaction_level, prediction.hr2.train, dnn=c("Acutal","Prediction"))
results.hr2.train=ifelse(perf.hr2.train>0.5,1,0)
misClass.hr2.train=mean(results.hr2.train!=train$satisfaction_level)
print(paste('Accuracy',1-misClass.hr2.train))

#Testing hr2 on testing set
prediction.hr2.test=predict(hr2, data=test, type="response")
perf.hr2.test=table(train$satisfaction_level, prediction.hr2.test, dnn=c("Acutal", "Preidction"))
results.hr2.test=ifelse(perf.hr2.test>0.5,1,0)
misClass.hr2.test=mean(results.hr2.test!=test$satisfaction_level)
print(paste('Accuracy',1-misClass.hr2.test))
(1-misClass.hr2.train)-(1-misClass.hr2.test)

#Peform model selection (backward)
library(MASS)
full.model=hr
model=step(hr, direction = "backward")
summary(model)
##Significant models are last evaluation, number of project, time spend on company, work accident, low, high, promotion

#Build a model again
hr3=lm(train$satisfaction_level~train$last_evaluation+train$number_project+train$time_spend_company+train$Work_accident+train$low+train$high+train$promotion_last_5years)
summary(hr3)

#Test hr3 on training set
prediction.hr3.train=predict(hr3, data=train, type="response")
perf.hr3.train=table(train$satisfaction_level, prediction.hr3.train, dnn=c("Actual","Prediction"))
results.hr3.train=ifelse(perf.hr3.train>0.5,1,0)
misClass.hr3.train=mean(results.hr3.train!=train$satisfaction_level)
print(paste('Accuracy',1-misClass.hr3.train))

#Test hr3 on testing set
prediction.hr3.test=predict(hr3, data=test, type="response")
perf.hr3.test=table(test$satisfaction_level, prediction.hr3.test, dnn=c("Acutal","Prediction"))
results.hr3.test=ifelse(perf.hr3.test>0.5,1,0)
misClass.hr3.test=mean(results.hr3.test!=test$satisfaction_level)
print(paste('Accuracy',1-misClass.hr3.test))
length(test$satisfaction_level)
length(prediction.h3.test)

#Build hr4 logstic regression model on training set
hr4=glm(satisfaction_level~last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years+sales+high+low+medium, data=train, family=binomial(link=logit))
summary(hr4)
#Testing hr4 on train set
prediction.train.hr4=predict(hr4, data=train, type="response")
perf.train.hr4=table(train$satisfaction_level, prediction.hr4, dnn=c("Actual","Prediction"))
results.train.hr4=ifelse(perf.train.hr4>0.5,1,0)
misClass.train.hr4=mean(results.train.hr4!=train$satisfaction_level)
print(1-misClass.train.hr4)
#Testing hr4 on testing set
prediction.test.hr4=predict(hr4,data=test,type="response")
perf.test.hr4=table(train$satisfaction_level,prediction.test.hr4, dnn=c("Actual","Prediction"))
result.test.hr4=ifelse(perf.test.hr4>0.5,1,0)
misClass.test.hr4=mean(result.test.hr4!=test$satisfaction_level)
print(1-misClass.test.hr4)

#Build hr5 logstic regression model on training set
hr5=glm(satisfaction_level~last_evaluation+number_project+time_spend_company+Work_accident+low,data=train, family=binomial(link=logit) )
summary(hr5)
#Testing hr5 on training set
prediction.train.hr5=predict(hr5, data=train, type="response")
perf.train.hr5=table(train$satisfaction_level, prediction.train.hr5, dnn=c("Actual","Prediction"))
result.train.hr5=ifelse(perf.train.hr5>0.5,1,0)
misClass.train.hr5=mean(result.train.hr5!=train$satisfaction_level)
print(1-misClass.train.hr5)
#Testing hr5 on testing set
prediction.test.hr5=predict(hr5,data=test,type="response")
perf.test.hr5=table(train$satisfaction_level,prediction.test.hr5, dnn=c("Actual","Prediction"))
result.test.hr5=ifelse(perf.test.hr5>0.5,1,0)
misClass.test.hr5=mean(result.test.hr5!=test$satisfaction_level)
print(1-misClass.test.hr5)

#Build hr6 based on model selectin on training set
model2=step(hr4, direction = "backward")
hr6=glm(satisfaction_level ~ last_evaluation + number_project + time_spend_company + Work_accident + promotion_last_5years + high + low, data=train,family=binomial(link=logit) )
#Testing hr6 on training set
prediction.hr6.train=predict(hr6, data=train, type="response")
perf.hr6.train=table(train$satisfaction_level, prediction.hr6.train, dnn=c("Actual","Prediction"))
results.hr6.train=ifelse(perf.hr6.train>0.5,1,0)
misClass.hr6.train=mean(results.hr6.train!=train$satisfaction_level)
print(paste('Accuracy',1-misClass.hr6.train))
#Testing hr6 on testing set
prediction.hr6.test=predict(hr6, data=test, type="response")
perf.hr6.test=table(train$satisfaction_level, prediction.hr6.test, dnn=c("Acutal","Prediction"))
results.hr6.test=ifelse(perf.hr6.test>0.5,1,0)
misClass.hr6.test=mean(results.hr6.test!=test$satisfaction_level)
print(paste('Accuracy',1-misClass.hr6.test))

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
1-sum(diag(train.accuracy))/sum(train.accuracy)
#Compute Naive Bayes on Testing set
p.test=predict(train.naive, myd.test[,2:13], type="class")
test.accuracy=table(p.test,myd.test$left)
test.accuracy
1-sum(diag(test.accuracy))/sum(test.accuracy)

#Perform linear Random Forest on training
library(randomForest)
forest=randomForest(as.factor(left)~., data=train, importance=TRUE, ntree=100)
train.pred=predict(forest, newdata=train)
train.perf=table(train$left, train.pred, dnn=c("Actual","Predicted"))
train.perf
1-sum(diag(train.perf))/sum(train.perf)
sensitivity(train.perf)
specificity(train.perf)

#Perform linear Random Forest on testing
test.forest=randomForest(as.factor(left)~., data=test, importance=TRUE, ntree=100)
test.pred=predict(test.forest, newdata = test)
test.pref=table(test$left, test.pred, dnn=c("Actual","Predicted"))
test.pref
1-sum(diag(test.perf))/sum(test.perf)
