setwd("/Users/jennalindsey/desktop/data viz")

library(ggplot2)


#part1

#Create two random numeric vectors using the absolute value of draws from a standard normal distribution, each with 50 values in them

Turtle.Height <- c(sample(1:300,50,replace=T))
 
Turtle.Weight<-c(sample(1:275,50,replace=T))

#Create a factor with 50 random values out of three levels
tut.type= c("LoggerHead", "Kemps Ridley", "LeatherBack")
Turtle.type <- c(sample(tut.type,50,replace = T))

#Create a dataframe using these three components as variables -- call the first vector "Turtle.Height", the second "Turtle.Weight", and the third "Turtle.Type"
print(Turtle.type)
df = data.frame(Turtle.Height,Turtle.Weight,Turtle.type)
#print(df)
T.Height = df[1]
T.Weight = df[2]
len = nrow(T.Height)

#part2 Produce a data frame with only turtle type and turtle heights as variables, where the heights are less than 0.5

newdf= subset(df , Turtle.Height < 120 , select = c(Turtle.Height,Turtle.Weight))
Tnew.Height = newdf[1]
#print(Tnew.Height)
Tnew.Weight = newdf[2]
newlength = nrow(Tnew.Height)
#print(newlength)
#part3
#Use ggplot to produce a histogram of turtle heights X-NOT-types-X from the original data setg
l = c(1:len)
p <- ggplot(df, geom = "histogram" , aes(l,T.Height))
p <- p + geom_bar(stat = "identity")
print(p)



#Use ggplot to produce a histogram of turtle heights X-NOT-types-X from the subset of the data set

newl = c(1:newlength)
q <- ggplot(newdf, geom = "histogram" , aes(newl,Tnew.Height))
q <- q + geom_bar(stat = "identity")
print(q)

#part4
#scatter plot of Turtle Hieght vs Turtle Weight

w <- ggplot(df, aes(x=T.Height,y=T.Weight)) + geom_point(aes(color= Turtle.type))
print(w)

#part5

#look at the Orange dataset
data("Orange")
head(Orange)

naranja <-head(Orange, n =33L)
print(naranja)

mod <- mode(naranja)
cat("the data is a :",mod , " \n")

cls <- class(naranja)
cat("the class are:" , cls)

#part6

oran <- ggplot(naranja, aes(x=circumference,y=age)) + geom_point(aes(color=Tree))
print(oran)
