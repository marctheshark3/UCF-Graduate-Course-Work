#                              ------------<<<<<<<<<<< LIBRARY >>>>>>>>----------

library(ISLR)
library(MASS)
library(glmnet)
library(pls)
library(pROC)
library(ROCR)
library(plotROC)
library (leaps)
library(caret)
library(pryr)
library(broom)
library(dotwhisker)
library("gridExtra")
library(randomForest)
library(tree)
library(aod)
library(ggplot2)
library(naivebayes)
library(bnclassify)
#library(logicFS)
library(doParallel)
#                              ------------<<<<<<<<<<< Data Input >>>>>>>>----------

data <- read.csv('/Users/marctheshark/Documents/MODELING AND SIMULATION/Data Mining Methodology/Final Project/DataSet/UCF Dataset 2018 - Training set.csv')


set.seed(33)


#                              ------------<<<<<<<<<<< Outlier Removal >>>>>>>>----------

#Remove_Outliers Function
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.05, .95), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

#No outliers for loan number
boxplot(data$LoanNumber)

#Do not use on credit score, removes 5k scores, which is highest predictor?
#ModifiedCreditScoreOR <- remove_outliers(train$ModifiedCreditScore)
#train$ModifiedCreditScore <- ModifiedCreditScoreOR

#1 NA!
ModifiedBankruptcyScoreOR <- remove_outliers(data$ModifiedBankruptcyScore)
data$ModifiedBankruptcyScore <- ModifiedBankruptcyScoreOR

EmployedMonthsOR <- remove_outliers(data$EmployedMonths)
data$EmployedMonths <- EmployedMonthsOR

#Removed 4654 obs, consider not using
#PrevEmployedMonthsOR <- remove_outliers(train$PrevEmployedMonths)
#train$PrevEmployedMonths <- PrevEmployedMonthsOR

#5269 observations
#CoEmployedMonthsOR <- remove_outliers(train$CoEmployedMonths)
#train$CoEmployedMonths <- CoEmployedMonthsOR

#5000 NAs, small range as well. consider not using
#CoPrevEmployedMonthsOR <- remove_outliers(train$CoPrevEmployedMonths)
#train$CoPrevEmployedMonths <- CoPrevEmployedMonthsOR

PrimeMonthlyIncomeOR <- remove_outliers(data$PrimeMonthlyIncome)
data$PrimeMonthlyIncome <- PrimeMonthlyIncomeOR

#Removed 3537 Observations, consider not using. 
#CototalMonthlyIncomeOR <- remove_outliers(train$CototalMonthlyIncome)
#train$CototalMonthlyIncome <- CototalMonthlyIncomeOR

#Removes 1772 incomes, consider not using?
#TotalMonthlyIncomeOR <- remove_outliers(train$TotalMonthlyIncome)
#train$TotalMonthlyIncome <- TotalMonthlyIncomeOR

PrimeMonthlyLiabilityOR <- remove_outliers(data$PrimeMonthlyLiability)
data$PrimeMonthlyLiability <- PrimeMonthlyLiabilityOR

PrimeMonthlyRentOR <- remove_outliers(data$PrimeMonthlyRent)
data$PrimeMonthlyRent <- PrimeMonthlyRentOR

TotalMonthlyDebtBeforeLoanOR <- remove_outliers(data$TotalMonthlyDebtBeforeLoan)
data$TotalMonthlyDebtBeforeLoan <- TotalMonthlyDebtBeforeLoanOR

VehicleYearOR <- remove_outliers(data$VehicleYear)
data$VehicleYear <- VehicleYearOR

VehicleMileageOR <- remove_outliers(data$VehicleMileage)
data$VehicleMileage <- VehicleMileageOR

TotalVehicleValueOR <- remove_outliers(data$TotalVehicleValue)
data$TotalVehicleValue <- TotalVehicleValueOR

AmountRequestedOR <- remove_outliers(data$AmountRequested)
data$AmountRequested <- AmountRequestedOR

#Consider not removing because it takes out 3066 observations
#DownPaymentOR <- remove_outliers(train$DownPayment)
#train$DownPayment <- DownPaymentOR

LoantermOR <- remove_outliers(data$Loanterm)
data$Loanterm <- LoantermOR

OccupancyDurationOR <- remove_outliers(data$OccupancyDuration)
data$OccupancyDuration <- OccupancyDurationOR

EstimatedMonthlyPaymentOR <- remove_outliers(data$EstimatedMonthlyPayment)
data$EstimatedMonthlyPayment <- EstimatedMonthlyPaymentOR

NumberOfOpenRevolvingAccountsOR <- remove_outliers(data$NumberOfOpenRevolvingAccounts)
data$NumberOfOpenRevolvingAccounts <- NumberOfOpenRevolvingAccountsOR

DTIOR <- remove_outliers(data$DTI)
data$DTI <- DTIOR


#                              ------------<<<<<<<<<<< Data Processing >>>>>>>>----------

#cleaning and preprocessing data
nrows <- nrow(data)
ncomplete <- sum(complete.cases(data))
ncomplete
missing <-!ncomplete

ncomplete/nrows
#find NA/NULL and getting rid of
data[!complete.cases(data),]
newdata <- na.omit(data)

typeof(data$LTV)

is.factor(data$LTV)
is.numeric(data$LTV)
data$CoMonthlyLiability

#sampling data
samp <-sample(1:nrow(newdata), floor(0.8*nrow(newdata)))

#creating test and training set

training <- newdata[samp,]
training.labels <- training$LoanStatus


test <- newdata[-samp,]
test.labels <- test$LoanStatus

#relabeling our newdata
data <- newdata




#Models to test

#Best Models 23 is better than 24 according to CV
mod23 = LoanStatus~PrimeMonthlyIncome + EstimatedMonthlyPayment + TotalMonthlyIncome + DTI + CoPrevEmployedMonths + LoanNumber + CoEmployedMonths + TotalMonthlyDebtBeforeLoan + isNewVehicle + ModifiedCreditScore + ModifiedBankruptcyScore + MemberIndicator + NumberOfOpenRevolvingAccounts + CoApplicantIndicator + EmployedMonths + AmountRequested + Loanterm + PrimeMonthlyLiability + VehicleMileage + VehicleYear + Source
mod24 = LoanStatus~OccupancyDuration + PrimeMonthlyIncome + EstimatedMonthlyPayment + TotalMonthlyIncome + DTI + CoPrevEmployedMonths + LoanNumber + CoEmployedMonths + TotalMonthlyDebtBeforeLoan + isNewVehicle + ModifiedCreditScore + ModifiedBankruptcyScore + MemberIndicator + NumberOfOpenRevolvingAccounts + CoApplicantIndicator + EmployedMonths + AmountRequested + Loanterm + PrimeMonthlyLiability + VehicleMileage + VehicleYear + Source

#Worst Model
mod1 =  LoanStatus~ ModifiedCreditScore



#                              ------------<<<<<<<<<<< Getting Results >>>>>>>>----------

#setting up Caret Settings
trellis.par.set(caretTheme()) 
#having the train function use repeated 10fold CV
control<-trainControl(method = "repeatedcv", number = 10)
metric<-"Accuracy"
set.seed(33)

#setting up the parrallel processing 
#DONT GO ABOVE 5!
cl <- makePSOCKcluster(5)
#telling the PC to use 5 cores
registerDoParallel(cl)
#telling the fitting functions that we want to test model 24
model <- mod24
#training our respective models for training
fit.lda <- train(model, data=training, method="lda", metric=metric, trControl=control, allowParallel = T)
fit.qda <- train(model, data=training, method="qda", metric=metric, trControl=control, allowParallel = T)
fit.knn <- train(model, data=training, method="knn", metric=metric, trControl=control, allowParallel = T)

fit.rf <- train(model, data=training, method="rf", metric=metric, trControl=control, allowParallel = T)
fit.svm1 <- train(model, data=training, method="svmRadial", metric=metric, trControl=control, allowParallel = T)
fit.svm2 <- train(model, data=training, method="svmLinear", metric=metric, trControl=control, allowParallel = T)
fit.adaboost <- train(model, data=training, method="adaboost", metric=metric, trControl=control, allowParallel = T)

#Getting Results
results <- resamples(list(lda=fit.lda,qda=fit.qda, svm1=fit.svm1,svm2=fit.svm2,
                          ada = fit.adaboost, rf= fit.rf))
#summarizes Results
summary(results)
#plots Results
dotplot(results)

#predicting the models on the test dataset
lda.pred <-postResample(predict(fit.lda, test), test$LoanStatus)
qda.pred <-postResample(predict(fit.qda, test), test$LoanStatus)
rf.pred <-postResample(predict(fit.rf, test), test$LoanStatus)
svm1.pred <-postResample(predict(fit.svm1, test), test$LoanStatus)
svm2.pred <-postResample(predict(fit.svm2, test), test$LoanStatus)
ada.pred <-postResample(predict(fit.adaboost, test), test$LoanStatus)

#getting the ROC Graphs for Results
roc0 <- roc( test$LoanStatus, 
            predict(fit.lda, test, type = "prob")[,1], 
            levels = rev(levels(test$LoanStatus)))


roc1 <- roc( test$LoanStatus, 
             predict(fit.qda, test, type = "prob")[,1], 
             levels = rev(levels(test$LoanStatus)))


roc2 <- roc( test$LoanStatus, 
             predict(fit.rf, test, type = "prob")[,1], 
             levels = rev(levels(test$LoanStatus)))


roc3 <- roc( test$LoanStatus, 
             predict(fit.svm1, test, type = "prob")[,1], 
             levels = rev(levels(test$LoanStatus)))


roc4 <- roc( test$LoanStatus, 
             predict(fit.svm2, test, type = "prob")[,1], 
             levels = rev(levels(test$LoanStatus)))


roc5 <- roc( test$LoanStatus, 
             predict(fit.adaboost, test, type = "prob")[,1], 
             levels = rev(levels(test$LoanStatus)))

#calling the roc plots we just made and plotting all of them
plot(roc1, print.thres = c(.5), type = "S")
lines(roc0, col = 'purple', type = 's')
lines(roc2, col = 'red', type='o')
#lines(roc3, col = 'blue')
#lines(roc4, col = 'gree')
lines(roc5, col = 'orange', type = 's')
legend('right',legend = c('LDA','QDA', 'Random Forest', 'AdaBoost'), fill = c('purple', 'black','red','yellow'))
title(main= '\n 2nd Best Model with 24 Predictors ROC Results')

#turns off Parallel Processing
stopCluster(cl)
#                           --------  <<<<<<<<<<< Initial MODEL ANALYSIS >>>>> ----------


#logtisitc Regression
LogitisticRegression <- function(model){
  lr <- glm(mod1,data = training , family = binomial)
  logit.pred <- predict(lr,test, type = 'response')
  plot(lr)
  lr.prob <- ifelse(logit.pred > .5 , "Up" , "Down")
  t <- table(lr.prob,test$LoanStatus)
  acc = ((t[1]+t[4])/(t[1]+t[2]+t[3]+t[4]))
  #paste("This accuracy for is:", acc)
  acc
  
  return ( t)
}


#LDA

LinearDiscrA <- function(model){
  l.fit <- lda(model,data = training)
  plot(l.fit)
  l.pred = predict(l.fit, test, type= 'response')
  lda.class = l.pred$class
  t = table(lda.class,test$LoanStatus)
  (mean(lda.class == test$LoanStatus))
  
  return (t)
}

#QDA


QuadDiscrA <- function(model){
  q.fit = qda(model, data = training)
  q.pred = predict(q.fit,test, type = 'response')
  qda.class = l.pred$class
  t = table(qda.class,test$LoanStatus)
  mean(qda.class == test$LoanStatus)
  
  
  return (t)
}

#Tree-Based

simpleTree <- function(model){
  tree.based= tree(model, data= training)
  plot(tree.based)
  text(tree.based)
  
  tree.pred = predict(tree.based,test,type = 'class')
  t = table(tree.pred,test$LoanStatus)
  mean(tree.pred == test$LoanStatus)
  
  
  return (t)
}


#CV Tree Based
cv_tree <- function(model){
  tree.based= tree(model, data= training)
  #crossValidating to find the right number of nodes!
  cv.tree <- cv.tree(tree.based, FUN = prune.misclass)
  names(cv.tree)
  cv.tree
  par(mfrow=c(1,2))
  
  plot(cv.tree$size,cv.tree$dev,type = 'b')
  plot(cv.tree$k,cv.tree$dev,type = 'b')
  
  
  prune.tree <- prune.misclass(tree.based, best = 6)
  plot(prune.tree)
  text(prune.tree)
  
  prune.pred =predict(prune.tree,test, type= 'class')
  t = table(prune.pred,test$LoanStatus)
  mean(prune.pred == test$LoanStatus)
  
  
  
  return (t)
  
}


#calling functions
model = mod1
LogitisticRegression(model)
LinearDiscrA(model)
QuadDiscrA(model)
simpleTree(model)
cv_tree(model)


all_models <- function(model){
  lr = LogitisticRegression(model)
  lda = LinearDiscrA(model)
  qda = QuadDiscrA(model)
  tree = simpleTree(model)
  cross_valid_tr = cv_tree(model)
  
  acc <- function(t){
    accuracy =((t[1]+t[4])/(t[1]+t[2]+t[3]+t[4]))
    if (t == lr){
      mod = "Logisitic Regession"
    } else if (t == lda){
      mod = "LDA"
    }  else if (t == cross_valid_tr ) {
      mod = "Cross Validated Pruned Tree"
    } else if (t == qda){
      mod = "QDA"
    }else {
      mod = "Tree Based Method" 
    }
    
    paste( "The Accuracy for the",mod,"is", accuracy )
  }
  
  
  a1 <- acc(lr)
  a2 <- acc(lda)
  a3 <- acc(qda)
  a4 <- acc(tree)
  a5 <- acc(cross_valid_tr)
  return (cat("\n",a1,"\n", a2,"\n",a3,"\n",a4,"\n",a5,'\n'))
  
}
all_models(model)


ggplot(data,aes(d=data$ModifiedCreditScore , m= data$LoanStatus))+ geom_roc()

#                              ------------<<<<<<<<<<< Density Graphs Data Exploration >>>>>>>>----------

density_graphs <- function(){
  
  #2D Density Estimation
  den2d<-ggplot(data ,aes(y= data$AmountRequested, x=data$ModifiedCreditScore ,color = data$LoanStatus, shape = data$LoanStatus))+geom_point() +scale_fill_manual(values = c('#800000','#191970'))+ stat_density_2d(aes(fill = data$LoanStatus), geom="polygon") + ylim(0,200000)
  #Data with Regresion line (need to zoom in)
  #ggplot(data ,aes(y= data$AmountRequested, x=data$ModifiedCreditScore ,color= data$LoanStatus, shape = data$LoanStatus))+geom_point() + geom_smooth(method = lm , aes(fill = data$LoanStatus)) + ylim(0,200000)
  #Data eith Regressin line Zoomed In
  ggplot(data ,aes(y= data$AmountRequested, x=data$ModifiedCreditScore ,color= data$LoanStatus, shape = data$LoanStatus))+geom_point() + geom_smooth(method = lm , aes(fill = data$LoanStatus)) + ylim(0,75000)
  
  densityY <- ggplot(data, aes(x = data$ModifiedCreditScore, fill=data$LoanStatus)) + 
    geom_density(alpha=.5) + 
    scale_fill_manual(values = c('#800000','#191970')) + 
    theme(legend.position = "none")
  
  
  densityX <- ggplot(data, aes(x = data$AmountRequested, fill=data$LoanStatus)) + 
    geom_density(alpha=.5) + xlim(0,100000)+
    scale_fill_manual(values = c('#800000','#191970')) + 
    theme(legend.position = "none")
  
  
  scatter <-  ggplot(data ,aes(y= data$AmountRequested, x=data$ModifiedCreditScore ,color = data$LoanStatus, shape = data$LoanStatus))+geom_point() +scale_fill_manual(values = c('#800000','#191970')) + ylim(0,150000)
  
  
  return (grid.arrange(densityX, scatter, densityY,den2d, ncol= 2 , nrow = 2 ))
  
}
density_graphs()


