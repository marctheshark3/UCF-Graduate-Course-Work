library(ISLR)
attach(Auto)
library(class)
mpg01 <- rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
Auto <- data.frame(Auto, mpg01)
Auto

pairs(Auto)
plot(Auto$mpg01,Auto$horsepower)
plot(Auto$mpg01,Auto$weight)
plot(Auto$mpg01,Auto$mpg)
plot(Auto$mpg01,Auto$acceleration)
plot(Auto$mpg01,Auto$cylinders)

#looking at these scatter plots above acceleration and cylinders arent ideal predictors 
#This is to due to there being some overlap and an ideal predictor would like the comparison to mpg
#the predictors I would choose are horsepower, weight, and mpg

#part c

train = (year %% 2 == 0 )
data.train = Auto[train,]
data.test = Auto[!train,]
mpg01.test <- mpg01[!train]

#part d
model = mpg01 ~ horsepower + weight + mpg
d.lda = lda(model)
d.lda

p.lda = predict(d.lda,data.test)
table(p.lda$class,mpg01.test)
#produced the confusion matrix

mean(p.lda$class != mpg01.test)
#so the test Error Rate is 3.29%

#part e


e.qda = qda(model)
p.qda = predict(e.qda,data.test)
table(p.qda$class,mpg01.test)

mean(e.qda$class !=mpg01.test)
#test error rate is 4.95%

#part f

f.glm = glm(model)
re= predict(f.glm,data.test, type = 'response')
p.glm = rep(0, length(re))
p.glm[re > 0.5] = 1
table(p.glm, mpg01.test)
mean(p.glm !=mpg01.test)
#The error rate is 3.2%

#part g

train.auto <- cbind(mpg, weight, horsepower)[train, ]
test.auto <- cbind(mpg, weight, horsepower)[!train, ]
train.mpg01 <- mpg01[train]
set.seed(1)
p.knn <- knn(train.auto, test.auto, train.mpg01, k = 20)
table(p.knn, mpg01.test)
mean(p.knn != mpg01.test)

#The error for:
#k=1 is 14.83516
#k=3 is 15.38
#k=10 is 15.93
#k=50 is 14.29
#k=100 is 14.835
#k=200 is 54.94%
# so it seems right between say 25 and 75 is a spot where the optimal k lies



