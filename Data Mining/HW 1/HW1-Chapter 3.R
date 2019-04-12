#chapter 3 Question 11

set.seed(1)
x = rnorm(100)
y = 2*x+rnorm(100)
p = lm(y~x+0)
summary(p)
#The value of B is 1.9939
#The SE is 0.1065
#the t-score is 18.73
#p-value is 2.2e-16
#since our p is small we reject the Ho

#partb

q = lm(x~y+0)
summary(q)
#The value of B is .39111
#The SE is 0.02098
#the t-score is 18.73
#p-value is 2.2e-16
#since our p is small we reject the Ho

#partC 

#the values of B for both results do differ as for the error.
#the t-scores and p-values are equal to each other
#This shows that we are analyzing the same line that was initially created.

#part d

w = length(x)

z = sqrt(w-1)*(x %*% y)
e = sqrt(sum(x**2)*sum((y**2))-(x%*%y)**2)
t.stat = z/e
as.numeric(t.stat)
#18.72593

#parte argue why the t stats woudl be the same if we replace x and y with each other.

#this can be seen from the formulas above. If i were to replace them mathematically nothing will change
#so of course they will be the same.

#part f

p = lm(y~x)
summary(p)

q= lm(x~y)
summary(q)
#the t stats for x and y are 18.56 with a p value of 2.2e-16
#again showing that the regression for y onto x is the same as x onto y

