library(ISLR)
library(glmnet)
library(pls)
data <- read.csv('/Users/marctheshark/Documents/MODELING AND SIMULATION/Data Mining Methodology/Final Project/DataSet/UCF Dataset 2018 - Training set.csv')
data$LoanNumber <- NULL
data$VehicleMake <- NULL
data$AppReceiveDate <- NULL
data$LTV <- NULL
data$CoMonthlyLiability<-NULL
data$CoMonthlyRent<-NULL
data$RequestType <- NULL 
data$OccupancyStatus <- NULL

set.seed(33)
library (leaps)
library(caret)
library(pryr)


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

#Try to figure out the how to fix the predictors
#sampling
samp <-sample(1:nrow(newdata), floor(0.8*nrow(newdata)))

#creating test and training set

training <- newdata[samp,]
training.labels <- train$LoanStatus


test <- newdata[-samp,]
test.labels <- test$LoanStatus


data <- newdata

library(aod)

#logistic_reg = glm(data$LoanStatus~. , data = data, family = 'binomial')
#summary(logistic_reg)

#Random Forest


rf <- train(LoanStatus~ModifiedCreditScore,data=test ,method = 'adaboost',preProc = c("center", "scale"))

#logtisitc Regression
mod1 = LoanStatus~ModifiedCreditScore+EmploymentStatus+EmployedMonths+ModifiedBankruptcyScore+VehicleMileage+EstimatedMonthlyPayment
mod2 = LoanStatus~ModifiedCreditScore+EmploymentStatus+EmployedMonths+ModifiedBankruptcyScore+VehicleMileage+EstimatedMonthlyPayment+Loanterm+TotalMonthlyIncome+EmployedMonths


LogitisticRegression <- function(model){
  lr <- glm(model,data = training , family = binomial)
  logit.pred <- predict(lr,test, type = 'response')
  lr.prob <- ifelse(logit.pred > .5 , "Up" , "Down")
  t <- table(lr.prob,test$LoanStatus)
  acc = ((t[1]+t[4])/(t[1]+t[2]+t[3]+t[4]))
  #paste("This accuracy for is:", acc)
  acc
}


#LDA
library(MASS)
LinearDiscrA <- function(model){
  l.fit <- lda(model,data = training)
  l.fit
  l.pred = predict(l.fit)
  lda.class = l.pred$class
  table(lda.class,training$LoanStatus)
  
  
return (mean(lda.class == training$LoanStatus))
}

#QDA
QuadDiscrA <- function(model){
  q.fit = qda(model, data = training)
  q.pred = predict(q.fit)
  qda.class = l.pred$class
  table(qda.class,training$LoanStatus)
  mean(qda.class == training$LoanStatus)
}


#SVM 
library(e1071)
svmfit = svm(mod1,data = training,kernel = 'linear', cost =10, scale = FALSE)
plot(svmfit, training)
ypred = predict(svmfit,test)
table(ypred,test$LoanStatus)
mean(ypred == test$LoanStatus) #50.44%

#Tree-Based
library(tree)
simpleTree <- function(model){
  tree.based= tree(model, data= training)
  plot(tree.based)
  text(tree.based)
  
  tree.pred = predict(tree.based,test,type = 'class')
  table(tree.pred,test$LoanStatus)
  mean(tree.pred == test$LoanStatus)
}

#CV Tree Based
tree.based= tree(model, data= training)
#crossValidating to find the right number of nodes!
cv.tree <- cv.tree(tree.based, FUN = prune.misclass)
names(cv.tree)
cv.tree
par(mfrow=c(1,2))

plot(cv.tree$size,cv.tree$dev,type = 'b')
plot(cv.tree$k,cv.tree$dev,type = 'b')


prune.tree <- prune.misclass(tree.based, best = 5)
plot(prune.tree)
text(prune.tree)

prune.pred =predict(prune.tree,test, type= 'class')
table(prune.pred,test$LoanStatus)
mean(prune.pred == test$LoanStatus)


#calling functions
model = mod2
LogitisticRegression(model)
LinearDiscrA(model)
QuadDiscrA(model)
simpleTree(model)