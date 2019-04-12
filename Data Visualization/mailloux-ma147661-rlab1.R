setwd("/Users/jennalindsey/desktop/data viz")

#data = read.csv("lab1.dat", header = FALSE, comment.char = "")
tables <- read.table("lab1.dat", header = TRUE,comment.char = "#",sep = ",")t
print(tables)

col = colnames(tables, do.NULL = TRUE, prefix = "col")
print(col)

summary = summary(tables)
#Providing Global Summary
print(summary)

winetype = tables[,1]
#winetype
cat("The Wine Type are" , winetype)

#determing what the levels are
lev <- levels(winetype)
print(lev)

#couting how many are in each level
table(winetype)
cat("The number of levels in the Wine Type are: \n", table(winetype))

colorIntensity = tables[,11]
avg = mean(colorIntensity)
cat("\n Color Intensity Mean is:" ,avg)
cat("\n Color Intensity Variance is:" , var(colorIntensity))

#computing the CI of Color Intensity
ci = t.test(colorIntensity, mu = avg)
print(ci)

#hue <- tables[,12]
#print(hue)
print("PART 6")
classone = tables[,1]
hue = tables[,12]
partsix <- subset(tables, tables$Winetype == "Class1", select = (tables$Hue))
partsix <-tables[(tables$Winetype),(tables$Hue)]
#hue <- tables[ which(tables$WineType == "Class 1 & tables$Hue")]
print(partsix)

#part7

x <- 120-536
cat("\n x is: ", x)

y <- 3 * 7
cat("\n y is ", y)

z <- 12 / 3
cat(" \n z is ", z)

loggy <- log2(128)
cat(" \n the log base 2 of 128 is:", loggy)

ninecube <- 9 ** 3
cat(" \n nine cubed is:", ninecube)

#part8

hue = tables$Hue
colorIntensity = tables$ColorIntensity

pairwise = hue * colorIntensity
print(pairwise)
pairsum = sum(pairwise)
cat("The Pairwise sum of Hue and ColorIntensity is:", pairsum)

#part9

uni = runif(n = 10, min = 0, max = 10)
cat("\n Uniform:" ,uni)

normal = rnorm(n=10, mean = 1, sd = 3)
cat("\n normal",normal)

Probablydense = dnorm(.12,mean = 1,sd = 3)
cat("\n PDF result of .12:",Probablydense)

cdf = pnorm(1.96,mean = 1,sd = 3)
cat("\n Probability of 1.96 is:", cdf)

#part10
print("part10")

n= 0
arriving <- c()
add<-c()
simList <-c()
cumil<-c()
for  (n in 1:100){ 
  
  #samp = abs(rnorm(1,2,1))
  #arrivalRate <- dpois(x = 10,lambda = samp, log = FALSE)
  
  arrivalRate <- rpois(10,2)
  #add <- list(poisson = arrivalRate)
  add[[n]] <- arrivalRate
}

for (i in i:100){
  
  cumil[[i]] <- cumsum(add[[i]])
  
}
print("Simulation Distribution")
print(add)

x1 = summary(add[[1]])
print("1st column summary:  ")
print(x1)

x2 = summary(add[[2]])
print("2nd column summary: ")
print(x2)

x3 = summary(add[[3]])
print("3rd column summary: ")
print(x3)

x4 = summary(add[[4]])
print("4th column summary:")
print(x4)

x5 = summary(add[[5]])
print("5th column summary:")
print(x5)

x6 = summary(add[[6]])
print("6th column summary:")
print(x6)

x7 = summary(add[[7]])
print("7th column summary:")
print(x7)

x8 = summary(add[[8]])
print("8th column summary:")
print(x8)

x9 = summary(add[[9]])
print("9th column summary:")
print(x9)

x10 = summary(add[[10]])
print("10th column summary:")
print(x10)

print("Total number of people in line per sim:")
print( cumil)

estimate <- function(n) {
  return (4*sum((runif(n)^2 + runif(n)^2) < 1)/n)
}

print("\n estimate at 10! \n ")
print(estimate(10))

print("\n estimate at 10,000! \n ")
print(estimate(10000))

print("\n estimate at 10,000,000! \n ")
print(estimate(10000000))

#AHHHH its the estimation of pi!!



