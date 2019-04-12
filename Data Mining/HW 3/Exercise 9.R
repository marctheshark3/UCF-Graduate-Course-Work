#Excerise 9 

library(ISLR)

data(College)
set.seed(33)

train <- sample(1:nrow(College), nrow(College)/2)
test <- -train

College.train <- College[train,]
College.test <- College[test,]

train.lm <- lm(Apps ~ ., data = College.train)

predict.lm <-predict(train.lm, College.test)
mean((predict.lm - College.test$Apps)^2)

#the test MSE is 1286412

#part c
train.rid = model.matrix(Apps~.,data = College.train)
test.rid = model.matrix(Apps~.,data = College.test)

grid <- 10 ^ seq(4, -2, length = 100)

training.ridge <- glmnet(train.rid, College.train$Apps,alpha = 0, lambda = grid, thresh = 1e-12)
cv.ridge <-cv.glmnet(train.rid, College.train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
lam.ridge <- cv.ridge$lambda.min
lam.ridge


pred.ridge <- predict(training.ridge, s = lam.ridge, newx = test.rid)
mean((pred.ridge - College.test$Apps)^2)
#The MSE for the Ridge is 1340660 and is higher than the least squars

#part d 
#aplha = 1 for Lasso
lasso = glmnet(train.rid, College.train$Apps, alpha = 1 ,lambda = grid, thresh = 1e-12)
lasso.cv = cv.glmnet(train.rid,College.train$Apps, alpha = 1, lambda =  grid, thresh = 1e-12)
lam = cv.lasso$lambda.min

pred.lasso <- predict(lasso,s= lam , newx = test.rid)
mean((pred.lasso - College.test$Apps)^2)
#the MSE for the lasso is 1258122
#this MSE is higher than the least squares

library(pls)

#part e
train.pcr <-pcr(Apps ~ ., data = College.train, scale = TRUE, validation = "CV")
validationplot(train.pcr,val.type = "MSEP")
mtext("PCR")
pred.pcr <- predict(train.pcr,College.test,ncomp = 10)
mean((pred.pcr - College.test$Apps)^2)
#the MSE is 2674954

#part F
train.pls <- plsr(Apps ~ ., data = College.train, scale = TRUE, validation = "CV")
validationplot(train.pls,val.type = 'MSEP')
mtext("PLS")
pred.pls <- predict(train.pls,College.test,ncomp = 10)
mean((pred.pls - College.test$Apps)^2)
#the MSE for PLS is 1311462

#part g
test.avg <- mean(College.test$Apps)
lm.r2 <- 1 - mean((predict.lm - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
ridge.r2 <- 1 - mean((pred.ridge - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
lasso.r2 <- 1 - mean((pred.lasso - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
pcr.r2 <- 1 - mean((pred.pcr - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
pls.r2 <- 1 - mean((pred.pls - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)

paste('test.avg:',test.avg) #2802.527
paste('lm.r2:',lm.r2) #.91525
paste('ridge.r2:',ridge.r2) #.9117
paste('lasso.r2:',lasso.r2) #.91711
paste('pcr.r2:',pcr.r2) #.82377
paste('pls.r2:',pls.r2) #.9136

#all models have high accuracy except for the PCR model

