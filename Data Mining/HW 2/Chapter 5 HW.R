#Chapter 5
#exercise 2 part g


x = 1:100000
y = (1-(1-1/x)**x)
plot(x,y)

#it can be seen from the plot that it shows the curve reaching a limit 
#which is approximately .6323


#exercise part h 
store <- rep(NA, 10000)
for (i in 1:10000){
  store[i] <- sum(sample(1:100, rep=TRUE ) == 4) >0
}
mean(store)
#The result was .632
#this shows that the average value of the 4th observation was .632
#Also helps show that indeed we are hitting a limit of about .632


#exercise 8
#given
set.seed(1)
y<- rnorm(100)
x<- rnorm(100)
y <-x - 2*x^2+rnorm(100)

#part a
#the n is 100, p= 2
#model is Y = X -2X^2 + epsilon

#part b

plot(x,y)
#There seems to be a non-linear relationship
library(boot)
set.seed(1)


data <- data.frame(x,y)
model.1 <- glm(y~x)
cv.glm(data, model.1)$delta[1]

model.2 <- glm(y~poly(x,2))
cv.glm(data, model.2)$delta[1]

model.3 <- glm(y~poly(x,3))
cv.glm(data, model.3)$delta[1]

model.4 <- glm(y~poly(x,4))
cv.glm(data, model.4)$delta[1]
#part d
set.seed(333)


data <- data.frame(x,y)
model.1 <- glm(y~x)
cv.glm(data, model.1)$delta[1]

model.2 <- glm(y~poly(x,2))
cv.glm(data, model.2)$delta[1]

model.3 <- glm(y~poly(x,3))
cv.glm(data, model.3)$delta[1]

model.4 <- glm(y~poly(x,4))
cv.glm(data, model.4)$delta[1]
#the results are the same because LOOCV 
#analyses the n folds of a single observation


#part e
#the smallest error was model 2 which make sense since the relationship looks quadratic
#this result confirms indeed that the realtion is quadratic

#part f

summary(model.4)
#based on the above results it can be seen that the Probabilites for linear and
#quadratic terms are stastically significant from the P-value and for the 3rd and 4th degree terms have no significance
#this backs up the claim that this relationship is indeed quadratic and agrees with the CV results



