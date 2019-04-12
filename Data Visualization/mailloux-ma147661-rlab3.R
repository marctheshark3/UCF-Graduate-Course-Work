setwd("/Users/jennalindsey/desktop/data viz")

library(ggplot2)
library(dplyr)
library(reshape2)

#part1 Getting and Reformatting Data

data = read.table("sc-est2014-agesex-civ.csv" , header = TRUE,comment.char = "#",sep = ",")
#data = read.csv("sc-est2014-agesex-civ.csv", header = T,sep = ",")
a <-colnames(data)
#print(data)
data <- filter(data, AGE <99)
print(data)
florida <- subset(data,NAME == 'Florida',  select=c(AGE,POPEST2010_CIV,POPEST2011_CIV,POPEST2012_CIV,POPEST2013_CIV,POPEST2014_CIV))

print('florida')
print(florida)

alabama = subset(data,NAME == 'Alabama',select=c(AGE,POPEST2010_CIV,POPEST2011_CIV,POPEST2012_CIV,POPEST2013_CIV,POPEST2014_CIV))
print('melt')
m_fl = melt(florida,id = "AGE")
print(m_fl)

m_al= melt(alabama, id = "AGE")


p<-ggplot(m_fl, aes(x= AGE, y=value , color = variable)) + geom_line() + geom_point() + labs(x = 'AGE', y='Population', title = "Florida Data Set")
q <- ggplot(m_al, aes(x= AGE, y=value ,color = variable)) + geom_line() + geom_point() + labs(x = 'AGE', y='Population', title = "Alabama Data Set")
print(q)
print(p)

#part2

knapsack = read.table("knapsack-data.csv" , header = TRUE,comment.char = "#",sep = ",")

#print(knapsack)
ks = subset(knapsack,knapsack$ProblemInstance == 'P1')

ks = summary(knapsack)
p <- ggplot(knapsack , aes(x = ProblemInstance, y = BestSolution)) + geom_boxplot()
#print(ks)
print(p)

#part3
print('part3')
pga <-read.csv("pga2004.csv", header = TRUE,comment.char = "#",sep = ",")
pga <- pga[complete.cases(pga),]
pga1 <- pga[-77,]
#print(pga1)
group = data.frame(pga1$winnings,pga1$drvacc,pga1$grnreg,pga1$savepct)
print(group)

meltgroup <-melt(group,id = "pga1.winnings")
print(meltgroup)

z<- ggplot(meltgroup,  aes(y= pga1.winnings, x= value,color = variable)) +geom_point(alpha=.3)  + labs(x = 'Value')
print(z)

#I picked these variables because they would use a similar scale in the x-axis and so easily comparable on the same axis although can be seen as confusing

#part 4
print('part4')
fit = lm(data = pga1,formula = pga1$winnings ~ pga1$drvacc+ pga1$avedrv +pga1$grnreg + pga1$savepct + pga1$events + pga1$aveputt)
fits = summary(fit)$r.squared

fit2 = lm(data = pga1,formula = pga1$winnings ~ pga1$grnreg + pga1$savepct + pga1$events + pga1$aveputt)
fits2 = summary(fit2)$r.squared
print("fit")
print(fits)
print("fit2")
print(fits2)
#Possibly more variables to consider the better the approximation
#part5

p5 = read.csv("pga2004b.csv")

p5 = data.frame(p5$winnings,p5$drvacc,p5$grnreg,p5$savepct)
print(p5)
m_p5 = melt(p5, id = p5$winnings)
print(m_p5)

fit5 <-  lm(data=p5, formula = pga1$winnings ~ pga1$drvacc+ pga1$avedrv +pga1$grnreg + pga1$savepct + pga1$events + pga1$aveputt)
fit52 = lm(data = p5,formula = pga1$winnings ~ pga1$grnreg + pga1$savepct + pga1$events + pga1$aveputt)
fits5 <- summary(fit5)$r.squared
fits52 <- summary(fit52)$r.squared
#print(fit5)
print('fits')
print(fits5)
#print(fits52)
print('fits52')
print(fits52)

#if the same logic follows as in 4 then it would same again the more variables the higher the number, and oddly enough they are the same numbers so thats odd
#more variables the higher the estimation!
