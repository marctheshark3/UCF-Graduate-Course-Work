#Exercise 8

#part a

library (leaps)
set.seed(333)
X <- rnorm(100)
e <- rnorm(100)
b0 <- 5
b1 <- 4
b2 <- 35
b3 <- -5
Y <- b0 + b1*X + b2*(X^2) + b3*(X^3)+e

data <- data.frame( y = Y,x = X)
regfit <- regsubsets(Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data = data, nvmax = 10)

reg.sum <- summary(regfit)

par(mfrow = c(2,2)) 

plot(reg.sum$rss, xlab = ' Number of Variables' , ylab = 'RSS', type = "l")

points(which.min(reg.sum$rss), reg.sum$rss[which.min(reg.sum$rss)], col = 'red' , cex = 2 , pch = 20)

plot(reg.sum$bic, xlab = ' Number of Variables' , ylab = 'BIC', type = 'l')

points( which.min(reg.sum$bic), reg.sum$bic[ which.min(reg.sum$bic)], col = 'red' , cex = 2 , pch = 20)

plot(reg.sum$adjr2, xlab = ' Number of Variables' , ylab = 'Adjusted R2', type = 'l')

points(which.max(reg.sum$adjr2), reg.sum$adjr2[which.max(reg.sum$adjr2)], col = 'red' , cex = 2 , pch = 20)

plot(reg.sum$cp, xlab = ' Number of Variables' , ylab = 'Cp', type = 'l')
min.cp <- which.min(reg.sum$cp)
points(min.cp, reg.sum$cp[min.cp], col = 'red' , cex = 2 , pch = 20)
mtext('PART C: Best Subset Selection', side = 3 , line = -2, outer = T)
# the number of variables for each value are 10 RSS, 3 BIC, 7 Adjusted R^2, 3 Cp


#part D Forward Stepwise

regfit.fwd <- regsubsets(Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data = data, nvmax = 10 , method = 'forward')

reg.sum.fwd <- summary(regfit.fwd)

par(mfrow = c(2,2)) 

plot(reg.sum.fwd$rss, xlab = ' Number of Variables' , ylab = 'RSS', type = "l")

points(which.min(reg.sum.fwd$rss), reg.sum.fwd$rss[which.min(reg.sum.fwd$rss)], col = 'red' , cex = 2 , pch = 20)

plot(reg.sum.fwd$bic, xlab = ' Number of Variables' , ylab = 'BIC', type = 'l')
 
points(which.min(reg.sum.fwd$bic), reg.sum.fwd$bic[which.min(reg.sum.fwd$bic)], col = 'red' , cex = 2 , pch = 20)

plot(reg.sum.fwd$adjr2, xlab = ' Number of Variables' , ylab = 'Adjusted R2', type = 'l')

points(which.max(reg.sum.fwd$adjr2), reg.sum.fwd$adjr2[which.max(reg.sum.fwd$adjr2)], col = 'red' , cex = 2 , pch = 20)

plot(reg.sum.fwd$cp, xlab = ' Number of Variables' , ylab = 'Cp', type = 'l')

points(which.min(reg.sum.fwd$cp), reg.sum.fwd$cp[which.min(reg.sum.fwd$cp)], col = 'red' , cex = 2 , pch = 20)
mtext('PART D: forward', side = 3 , line = -2, outer = T)

#in part D I found that the number of variables are RSS 10 , BIC 5, Adjusted R^2 6, and Cp 5



#backwards

regfit.bwd <- regsubsets(Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data = data, nvmax = 10 , method = 'backward')

reg.sum.bwd <- summary(regfit.bwd)

par(mfrow = c(2,2)) 

plot(reg.sum.bwd$rss, xlab = ' Number of Variables' , ylab = 'RSS', type = "l")

points( which.min(reg.sum.bwd$rss), reg.sum.bwd$rss[ which.min(reg.sum.bwd$rss)], col = 'red' , cex = 2 , pch = 20)

plot(reg.sum.bwd$bic, xlab = ' Number of Variables' , ylab = 'BIC', type = 'l')

points(which.min(reg.sum.bwd$bic), reg.sum.bwd$bic[which.min(reg.sum.bwd$bic)], col = 'red' , cex = 2 , pch = 20)

plot(reg.sum.bwd$adjr2, xlab = ' Number of Variables' , ylab = 'Adjusted R2', type = 'l')

points(which.max(reg.sum.bwd$adjr2), reg.sum.bwd$adjr2[which.max(reg.sum.bwd$adjr2)], col = 'red' , cex = 2 , pch = 20)

plot(reg.sum.bwd$cp, xlab = ' Number of Variables' , ylab = 'Cp', type = 'l')

points( which.min(reg.sum.bwd$cp), reg.sum.bwd$cp[ which.min(reg.sum.bwd$cp)], col = 'red' , cex = 2 , pch = 20)
mtext('PART D: Backwards', side = 3 , line = -2, outer = T)

#the number of variables for the values are 10 RSS, 3 BIC, 7 adjusted R^2, 3 Cp
coef(regfit, which.min(reg.sum$cp))
coef(regfit.fwd, which.min(reg.sum.fwd$cp))
coef(regfit.bwd, which.min(reg.sum.bwd$cp))

#All are the same except for the forward selection where the Cp chosen was 5 instead of 3, yet we can see that the first 3 are the same for all. The variable coeffcients are also very similar


library(glmnet)

lasso = model.matrix(Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data = data)[, -1]
cv.lasso = cv.glmnet(lasso,Y,alpha =1 )
plot(cv.lasso)
lam = cv.lasso$lambda.min

predict(cv.lasso, s = lam, type = "coefficients")[1:11, ]
#the lasso picks the intercept x^2, x^3,x^5 which follows the answers calculated the the previous part except that the X coeffecient is not used

#part f
b7 = 13
Y= b0 + b7*X^7+e



data <- data.frame( y = Y,x = X)
regfit <- regsubsets(Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data = data, nvmax = 10)

reg.sum <- summary(regfit)

par(mfrow = c(2,2)) 

plot(reg.sum$rss, xlab = ' Number of Variables' , ylab = 'RSS', type = "l")

points(which.min(reg.sum$rss), reg.sum$rss[which.min(reg.sum$rss)], col = 'red' , cex = 2 , pch = 20)

plot(reg.sum$bic, xlab = ' Number of Variables' , ylab = 'BIC', type = 'l')

points( which.min(reg.sum$bic), reg.sum$bic[ which.min(reg.sum$bic)], col = 'red' , cex = 2 , pch = 20)

plot(reg.sum$adjr2, xlab = ' Number of Variables' , ylab = 'Adjusted R2', type = 'l')

points(which.max(reg.sum$adjr2), reg.sum$adjr2[which.max(reg.sum$adjr2)], col = 'red' , cex = 2 , pch = 20)

plot(reg.sum$cp, xlab = ' Number of Variables' , ylab = 'Cp', type = 'l')
min.cp <- which.min(reg.sum$cp)
points(min.cp, reg.sum$cp[min.cp], col = 'red' , cex = 2 , pch = 20)
mtext('PART F: Best Subset Selection', side = 3 , line = -2, outer = T)
coef(regfit, which.min(reg.sum$cp))
coef(regfit.fwd, which.min(reg.sum.fwd$cp))
coef(regfit.bwd, which.min(reg.sum.bwd$cp))

lasso = model.matrix(Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data = data)[, -1]
cv.lasso = cv.glmnet(lasso,Y,alpha =1 )
plot(cv.lasso)
mtext('Part F Lasso')
lam = cv.lasso$lambda.min

predict(cv.lasso, s = lam, type = "coefficients")[1:11, ]

