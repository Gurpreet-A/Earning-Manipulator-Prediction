summary(CompleteData)
str(CompleteData)
View(CompleteData)
colnames(CompleteData)[1] <- "Company_ID"
colnames(CompleteData)[9] <- "C_Manipulator"
CompleteData$`C_Manipulator` <- as.factor(CompleteData$`C_Manipulator`)
index1 = sample(2,nrow(CompleteData), replace=TRUE,prob = c(0.7,0.3))
table(CompleteData$C_Manipulator)

CompleteData <-subset(CompleteData, select = -c(C_Manipulator))
CompleteData <-subset(CompleteData, select = -c(Company_ID))
colnames(trial)[10] <- "C_Manipulator"

#train and test datasets
set.seed(123)
trainData <- CompleteData[index1 ==1,]
summary(trainData)
testData <- CompleteData[index1 ==2,]
table(trainData$`C_Manipulator`)

rf4 <- randomForest(trainData$`C_Manipulator`~.,data = trainData, importance=TRUE,proximity= TRUE)
pred<-confusionMatrix(predict(rf4, testData), testData$C_Manipulator, positive = '1')
print(pred)

##balancing the data

str(SampleData)
colnames(SampleData)[1] <- "Company_ID"
colnames(SampleData)[1] <- "Company_ID"
SampleData$`C-MANIPULATOR` <- as.factor(SampleData$`C-MANIPULATOR`)
table(SampleData$`C-MANIPULATOR`)


install.packages("caret")
library(caret)
install.packages("ROSE")
library(ROSE)
#over sampling

trial <-subset(CompleteData, select = -c(Manipulator))
trial <-subset(trial, select = -c(Company_ID))
colnames(trial)[10] <- "C_Manipulator"
str(trial)
over1 <- ovun.sample(C_Manipulator~.,data = CompleteData, method = "over", N=2000)$data
table(trial$`C_Manipulator`)
table(over1$`C_Manipulator`)
summary(over)


View(over)

##Logostic regression
install.packages("aod")
library(aod)
install.packages("ggplot2")
library(ggplot2)
install.packages("Rcpp")
library(Rcpp)

##diving balanced data in train and test
set.seed(123545)
index1 = sample(2,nrow(over1), replace=TRUE,prob = c(0.7,0.3))
trainData1 <- over1[index1 ==1,]
testData1 <- over1[index1 ==2,]
table(trainData1$C_Manipulator)
str(trainData1)
str(testData1)

LogReg <- glm(C_Manipulator~.,data=trainData1, family = "binomial")
summary(LogReg)

confint(LogReg)
exp(coef(LogReg))

exp(cbind(OR = coef(LogReg), confint(LogReg)))

test_predict <- predict(LogReg, newdata = testData1,type = "response")

table(test_predict$C_Manipulator)

table(predict(LogReg, newdata=testData1, type="response"), testData1$C_Manipulator)

with(LogReg, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
confusionMatrix(predict(LogReg, testData1), testData1$C_Manipulator, positive = '1')


##random forest
library(randomForest)
install.packages("randomForest")

str(trainData)

rf2 <- randomForest(trainData1$`C_Manipulator`~.,data = trainData1)
rf2
summary(rf2)
str(rf2)
plot(rf2)
summary(trainData)
table(predict(rf2, newdata = testData1,type = "class"), testData1$C_Manipulator)
predict(rf2, newdata = testData1$C_Manipulator)
str(testData1)
table(testData1$C_Manipulator, predict(rf2, newdata = testData1))
confusionMatrix(predict(rf2, testData1), testData1$C_Manipulator, positive = '1')


rf5 <- randomForest(trainData1$`C_Manipulator`~.,data = trainData1, importance=TRUE,proximity= TRUE)
pred<-confusionMatrix(predict(rf, testData1), testData1$C_Manipulator, positive = '1')
predSensitivity[[i]] <-(1-pred$byClass['Sensitivity'])

print(pred)

##Cross validation implementation vor balanced data

over<-over[sample(nrow(over)),] 
#Create k equally size folds 
folds <- cut(seq(1,nrow(over)),breaks=10,labels=FALSE) 
#Perform k fold cross validation 


for(i in 1:5)
{ 
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i, arr.ind=TRUE) 
  testIndexes
  testDatarrf <- over[testIndexes, ]
  trainDatarf <- over[-testIndexes, ]
  # Use the test and train data for whatever you want
  rf <- randomForest(trainDatarf$`C_Manipulator`~.,data = trainDatarf, importance=TRUE,proximity= TRUE)
  pred<-confusionMatrix(predict(rf, testDatarrf), testDatarrf$C_Manipulator, positive = '1')
  print(pred)
  
}
rf$confusion

confusionMatrix(predict(rf, trainDatarf), trainDatarf$C_Manipulator, positive = '1')
str(trainDatarf)

##cross validation with unblanaced sample data

trial<-trial[sample(nrow(trial)),] 
#Create k equally size folds 
folds <- cut(seq(1,nrow(trial)),breaks=10,labels=FALSE) 
#Perform k fold cross validation 
str(trial)
View(trial)
table(trial$`C_MANIPULATOR`)
predSensitivity<- vector("list", 10)
for(i in 1:10)
{ 
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i, arr.ind=TRUE) 
  
  testDatarrf <- trial[testIndexes, ]
  trainDatarf <- trial[-testIndexes, ]
  # Use the test and train data for whatever you want
  rf <- randomForest(trainDatarf$`C_Manipulator`~.,data = trainDatarf, importance=TRUE,proximity= TRUE)
  pred<-confusionMatrix(predict(rf, testDatarrf), testDatarrf$C_Manipulator, positive = '1')
  predSensitivity[[i]] <-(1-pred$byClass['Sensitivity'])
  
  print(pred)
  
}
print(predSensitivity)


## balancing the entire dataset

over_Comp<- ovun.sample(C_Manipulator~.,data = CompleteData, method = "over", N=2400)$data
table(CompleteData$`C_Manipulator`)
table(over$`C_Manipulator`)
summary(over)

table(over_Comp$C_Manipulator)

##Random forest on sampoled complete data
over_Comp<-over_Comp[sample(nrow(over_Comp)),] 

folds <- cut(seq(1,nrow(over_Comp)),breaks=10,labels=FALSE) 
#Perform k fold cross validation 
str(over_Comp)
View(over_Comp)
table(over_Comp$C_Manipulator)
predSensitivity<- vector("list", 10)
for(i in 1:10)
{ 
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i, arr.ind=TRUE) 
  
  testDatarf3 <- over_Comp[testIndexes, ]
  trainDatarf3 <- over_Comp[-testIndexes, ]
  # Use the test and train data for whatever you want
  rf3 <- randomForest(trainDatarf3$`C_Manipulator`~.,data = trainDatarf3, importance=TRUE,proximity= TRUE)
  pred<-confusionMatrix(predict(rf, testDatarf3), testDatarf3$C_Manipulator, positive = '1')
  predSensitivity[[i]] <-(1-pred$byClass['Sensitivity'])
  
  print(pred)
  
}
print(predSensitivity)


##Random forest on complete unbalanced data
CompleteData<-CompleteData[sample(nrow(CompleteData)),] 
folds <- cut(seq(1,nrow(CompleteData)),breaks=10,labels=FALSE) 
#Perform k fold cross validation 
str(folds)
summary(folds)
View(folds)
View(overCompleteData_Comp)
table(CompleteData$C_Manipulator)
predSensitivity<- vector("list", 10)
for(i in 1:10)
{ 
  
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i, arr.ind=TRUE) 
  
  testDatarf3 <- CompleteData[testIndexes, ]
  trainDatarf3 <- CompleteData[-testIndexes, ]
  table(trainDatarf3$C_Manipulator)
  
  # Use the test and train data for whatever you want
  rf3 <- randomForest(trainDatarf3$`C_Manipulator`~.,data = trainDatarf3, importance=TRUE,proximity= TRUE)
  pred<-confusionMatrix(predict(rf3, testDatarf3), testDatarf3$C_Manipulator, positive = '1')
  predSensitivity[[i]] <-(1-pred$byClass['Sensitivity'])

  print(pred)

}
table(trainDatarf3$C_Manipulator)
print(predSensitivity)



##boosting

trial2 <-subset(CompleteData, select = -c(Manipulator))
trial <-subset(trial, select = -c(Company_ID))
colnames(trial)[10] <- "C_Manipulator"
str(trial)
over <- ovun.sample(C_Manipulator~.,data = trial, method = "over", N=362)$data
table(trial$`C_Manipulator`)
table(over_Comp$`C_Manipulator`)
summary(over)

set.seed(565925)
indexboost <- sample(2,nrow(CompleteData), replace=TRUE,prob = c(0.7,0.3))
trainBoost <- CompleteData[indexboost==1,]
testBoost <- CompleteData[indexboost==2,]
install.packages("mboost")
library(mboost)
str(trainBoost)
table(trainBoost$`C_Manipulator`)   


overTrain <- ovun.sample(C_Manipulator~.,data = trainBoost, method = "over", N=1662)$data
table(trial$`C_Manipulator`)
table(testBoost$`C_Manipulator`)


data.adaboost <- mboost(overTrain$`C_Manipulator` ~ ., data = overTrain,family = Binomial(type=c("adaboost")),control = boost_control(mstop=10 ))
data.adaboost
table(predict(data.adaboost, newdata=testBoost, type="class"), testBoost$C_Manipulator)
predBoost<-confusionMatrix(predict(data.adaboost, testBoost,type="class"), testBoost$C_Manipulator, positive = '1')
predBoost