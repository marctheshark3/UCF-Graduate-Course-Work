library(leaps)
set.seed(11)

train <- read.csv ('/Users/marctheshark/Documents/MODELING AND SIMULATION/Data Mining Methodology/Final Project/DataSet/UCF Dataset 2018 - Training set.csv')

#Sets the size of the data partition
sample <- sample.int(n = nrow(train), size = floor(.80*nrow(train)), replace = F)
train$class <- factor(train$LoanStatus)
#apply the size of the data partition to the data
train <- train[sample, ]
test  <- train[-sample, ]

#view data 
summary(train)
print(train)
names(train) 

#Remove App Recieved date variable
train$AppReceiveDate <- NULL
train$LoanNumber <- NULL
train$LTV <- NULL
train$CoMonthlyLiability <- NULL
train$CoMonthlyRent <-NULL
train$VehicleMake <- NULL
train$RequestType <- NULL

#omit missing variables.
train = na.omit(train)
sum(is.na(train))



#Using the generated results for the below models

mod1 <- class~train$ModifiedCreditScore.
#mod2
#mod3
#mod4 
#mod5

#using logisitic Regression

l1 = glm(train$class ~ train$ModifiedCreditScore , data = train, family = 'binomial')

#l2 = glm(mod2 , data = newdata, family = 'binomial')

#l3 = glm(mod3 , data = newdata, family = 'binomial')

#l4 = glm(mod4 , data = newdata, family = 'binomial')

#l5 = glm(mod5 , data = newdata, family = 'binomial')

summary(l1)


l1.predict <- predict(l1, newdata = test$class, type = 'response')
l1.predict <- ifelse(l1.predict > 0.5,1,0)

misClasificError <- mean(l1.predict != test$class)
print(paste('Accuracy',1-misClasificError))







fwd <- regsubsets(LoanStatus ~., data = train, nvmax = 10, nbest = 5, really.big = TRUE, method ="exhaustive")

summary(fwd)
names(fwd)

#view plot pf the variables selected in the subset selection
library(car)
subsets(fwd, statistic = "cp")
print(fwd)

#View data frame of the summary of the subset selection
fwdsum <-summary(fwd)
as.data.frame(fwdsum$outmat)
