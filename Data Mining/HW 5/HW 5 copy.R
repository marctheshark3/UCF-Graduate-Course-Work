library(ISLR)

data = Carseats

head(data)
#part A
set.seed(333)
samp <-sample(1:nrow(data), floor(0.8*nrow(data)))

training <- data[samp,]


test <- data[-samp,]

#Part B

library(tree)
tree.model <- tree(Sales ~ ., data = training)
summary(tree.model)

plot(tree.model)
text(tree.model, pretty = 0)

ypred = predict(tree.model, newdata = test)
mean((ypred - test$Sales)^2)

#Part C
set.seed(333)
cross.tree<- cv.tree(tree.model)
plot(cross.tree$size,cross.tree$dev,type = 'b')
tree.min = which.min(cross.tree$dev)
points(tree.min,cross.tree$dev[tree.min],col = 'red', cex = 2 , pch = 20)


prune.tree <- prune.tree(tree.model , best =9)
plot(prune.tree)
text(prune.tree , pretty = 0)

ypred = predict(prune.tree, newdata = test)
mean((ypred - test$Sales)^2)

library(randomForest)
bag <-  randomForest(Sales ~.,data = training , mtry = 10 , ntree = 500, importance = T)
ypred <- predict(bag, newdata = test)
mean(( ypred - test$Sales)^2)

importance(bag)

rf <- randomForest(Sales~.,data = training, mtry = 3 , ntree = 500 , importance = T)
rf.pred <- predict(rf, newdata= test)
mean((rf.pred-test$Sales)^2)

importance(rf)


#Exercise 11 

#part a
library(gbm)
data = Caravan

head(data)
set.seed(333)
trains <- 1:1000
#part b
Caravan$Purchase <- ifelse(Caravan$Purchase == "Yes", 1, 0)
c.train <- Caravan[trains,]
c.test <- Caravan[-trains,]

set.seed(333)

boost.c <- gbm(Purchase ~.,data = c.train,distribution = "gaussian", n.trees = 1000, shrinkage = 0.01)
xlim = x
summary(boost.c)



