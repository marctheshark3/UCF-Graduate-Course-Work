#Chapter 7 Exercise 9

library( ISLR)
library(boot)
library(MASS)
set.seed(333)
fit <- lm(nox ~ poly(dis,3), data = Boston)
summary(fit)
attach(Boston)
#finding the smallest and largest values
dislims = range(Boston$dis)

dis.grid = seq(from = dislims[1],to = dislims[2], by = .1)
pred.fit <- predict(fit, newdata = list(dis = dis.grid))

plot(nox~ dis, data = Boston ,xlim = dislims, cex = .5 , col = 'darkgrey')
title("Cubic Polynomial")
lines(dis.grid,pred.fit,lwd = 2, col = 'blue')

#from the summary all of the polynomial degrees are significant

#b
#testing for all 10 variables relative to RSS'

rss <- rep(NA, 10) #empty list

for ( i in 1:10){
  fit <- lm(nox ~ poly(dis,i), data = Boston)
  rss[i] <- sum(fit$residuals^2)
}

plot(1:10, rss , xlab = "Degree", ylab = "RSS", type = "l")
title('Part B testing different Polynomials')

#part C perform CV to select the optimal polynomial

cv <- rep(NA,10)

for (i in 1:10){
  fit <- glm(nox ~poly(dis,i), data = Boston)
  cv[i] <- cv.glm(Boston, fit, K=10)$delta[1]
}

plot(1:10, cv , xlab = "Degree", ylab = "Test MSE", type = "l")
title('Part C: Cross Validation')
summary(cv)


#part d

#fitting a regression spline

library(splines)

fit = lm(nox~ bs(dis, knots = c(3,7,10)))
pred = predict(fit,list(dis = dis.grid))

plot(nox~ dis, data = Boston ,xlim = dislims, cex = .5 , col = 'darkgrey')
title("Part D Regression Spline ")
lines(dis.grid,pred,lwd = 2, col = 'blue')
lines(dis.grid,pred.fit,lwd = 2, col = 'red')
#all of the polynomials are significant except for the first order. 

rss <- rep(NA, 16)
for (i in 3:16) {
  fit <- lm(nox ~ bs(dis, df = i), data = Boston)
  rss[i] <- sum(fit$residuals^2)
}
plot(3:16, rss[-c(1, 2)], xlab = "Degrees of freedom", ylab = "RSS", type = "l")
title('Part E Degrees of Freedom')

#the RSS decreases and levels off at 13 or 14 degrees of freedom
#part f CV for degrees of freedom
cv <- rep(NA, 16)
for (i in 3:16) {
  fit <- glm(nox ~ bs(dis, df = i), data = Boston)
  cv[i] <- cv.glm(Boston, fit, K = 10)$delta[1]
}
plot(3:16, cv[-c(1, 2)], xlab = "Degrees of freedom", ylab = "Test MSE", type = "l")

#the minimum look to be 11 for this instance