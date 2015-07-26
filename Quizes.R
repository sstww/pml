library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]


library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
qplot(mixtures$Superplasticize, geom="histogram")
qplot(log(mixtures$Superplasticize+1), geom="histogram")


library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(3433)
IL <- grep("^IL", colnames(training), value=TRUE)
ILpredictors <- predictors[, IL]
df <- data.frame(diagnosis, ILpredictors)
inTrain <- createDataPartition(df$diagnosis, p=3/4)[[1]]
training <- df[inTrain, ]
testing <- df[-inTrain, ]
modelFit <- train(diagnosis ~ ., method="glm", data=training)
predictions <- predict(modelFit, newdata=testing)
C1 <- confusionMatrix(predictions, testing$diagnosis)
print(C1)
acc1 <- C1$overall[1]
acc1 # Non-PCA Accuracy: 0.65 

modelFit <- train(training$diagnosis ~ ., 
                  method="glm", 
                  preProcess="pca", 
                  data=training, 
                  trControl=trainControl(preProcOptions=list(thresh=0.8)))
C2 <- confusionMatrix(testing$diagnosis, predict(modelFit, testing))
print(C2)
acc2 <- C2$overall[1]
acc2 # PCA Accuracy: 0.72



tr<-training[, grep("^IL", colnames(training))]
preP<-preProcess(tr, method="pca", thresh=.80)
preP$rotation

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


library(caret); library(kernlab); data(spam)
inTrain<-createDataPartition(y=spam$type, p=0.75, list=FALSE)

training<-spam[inTrain,]
testing<-spam[-inTrain,]
modelFit<-train(type~., data=training, method="glm")

inTrain<-createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training<-Wage[inTrain,]
testing<-Wage[-inTrain,]
dim(training); dim(testing)

featurePlot(x=training[, c("age", "education", "jobclass")], 
            y=training$wage, 
            plot="pairs")

library(Hmisc)
cutWage<-cut2(training$wage, g=3)
table(cutWage)

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)


training<-segmentationOriginal[segmentationOriginal$Case=="Train",]
testing<-segmentationOriginal[segmentationOriginal$Case=="Test",]
dim(training); dim(testing)
library(rpart)
library(rattle)
library(rpart.plot)
set.seed(125)
modelFit<-train(Case~., data=training, method="rpart")
fancyRpartPlot(modelFit$finalModel)
modelFit$finalModel
predict(modelFit, newdata=training)

#Quiz4
#Question 1
library(caret)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y<-factor(vowel.train$y)
vowel.test$y<-factor(vowel.test$y)
set.seed(33833)

mod1<-train(y~., data=vowel.train, method="rf", trCountrol=trainControl(method="cv", number=3))
mod2<-train(y~., data=vowel.train, method="gbm")

pred1<-predict(mod1, vowel.test); pred2<-predict(mod2, vowel.test)
qplot(pred1, pred2, colour=y, data=vowel.test)

rf_accuracy<-sum(pred1==vowel.test$y)/length(pred1)
gbm_accuracy<-sum(pred2==vowel.test$y)/length(pred2)
rf_accuracy
gbm_accuracy

predDF<-data.frame(pred1, pred2, y=vowel.test$y, 
                   agree=pred1==pred2)
accuracy<-sum(pred1[predDF$agree]==predDF$y[predDF$agree])/sum(predDF$agree)
accuracy

#Question 2
#Load the Alzheimer's data using the following commands

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
mod1<-train(diagnosis~., data=training, method="rf")
mod2<-train(diagnosis~., data=training, method="gbm", verbose=FALSE)
mod3<-train(diagnosis~., data=training, method="lda")

pred1<-predict(mod1, testing)
pred2<-predict(mod2, testing)
pred3<-predict(mod3, testing)

predData<-data.frame(pred1, pred2, pred3, diagnosis=testing$diagnosis)
comb_mod<-train(diagnosis~., data=predData, method="rf")
pred_comb<-predict(comb_mod, testing)

c1<-confusionMatrix(pred1, testing$diagnosis)$overall[1]
c2<-confusionMatrix(pred2, testing$diagnosis)$overall[1]
c3<-confusionMatrix(pred3, testing$diagnosis)$overall[1]
c4<-confusionMatrix(pred_comb, testing$diagnosis)$overall[1]
print(paste(c1, c2, c3, c4)) 


#Question 3
#Load the concrete data with the commands:

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
mod1<-train(CompressiveStrength~., data=training, method="lasso")
plot.enet(mod1$finalModel, xvar="penalty", use.color=T)
#cement

#Question 4

#Load the data on the number of visitors to the instructors blog from here:
#  https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv

#Using the commands:
  
  library(lubridate)  # For year() function below
dat = read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

library(forecast)
fit <- bats(tstrain)
fit
pred <- forecast(fit, level=95, h=dim(testing)[1])
names(data.frame(pred))
predComb <- cbind(testing, data.frame(pred))
names(testing)
names(predComb)
predComb$in95 <- (predComb$Lo.95 < predComb$visitsTumblr) & 
  (predComb$visitsTumblr < predComb$Hi.95)
# How many of the testing points is the true value within the 
# 95% prediction interval bounds?
prop.table(table(predComb$in95))[2] # 0.9617021


# Problem 5.
set.seed(3523)
library(AppliedPredictiveModeling)
library(e1071)
data(concrete)
inTrain <- createDataPartition(concrete$CompressiveStrength, p=3/4)[[1]]
training <- concrete[inTrain, ]
testing <- concrete[-inTrain, ]
set.seed(325)
fit <- svm(CompressiveStrength ~., data=training)
# OR another way
# fit <- train(CompressiveStrength ~. data=training, method="svmRadial")
pred <- predict(fit, testing)
acc <- accuracy(pred, testing$CompressiveStrength)
acc
acc[2] # RMSE 6.715009