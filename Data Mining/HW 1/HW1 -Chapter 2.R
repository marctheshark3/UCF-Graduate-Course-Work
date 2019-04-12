library(MASS)
library(ggplot2)
library(plyr)

?Boston
#Q1: How many rows are in this data set? How Many Columns? What do the Rows and Columns represent?
#506 rows and 14 Columns
#The Columns represent the predictors and the rows are the values for each observation of the suburb
dim(Boston)
str(Boston)
print(Boston)
#partb
par(mfrow = c(1, 2))
plot(Boston$rm, Boston$crim)
plot(Boston$age, Boston$crim)
pairs(Boston)
#From looking at the scatter plots it seems that there are some correlations between various variables
#Specfically Age has correlations between other variables like nox,Istat, and crim
#it also shows which variables are categorical and which are quantitative, where the qualitative variables are either 1 to 5
#part c
hist(Boston$crim, breaks = 50)
#the data is for the most part under 18
#Using less than 20 to have a better look at the data
pairs(Boston[Boston$crim < 18, ])
#Yes the variables crim seems to be associated with AGE,NOX,rm,dis,medv, and Istat. Other variable correlations are difficult to tell
#looking only at the age and crim it clear that there is some sort of relationship

#partd
hist(Boston$crim, breaks = 50)
nrow(Boston[Boston$crim > 20, ])
#only 18 areas have higher crime rate of 20% or higher than the rest of Boston
nrow(Boston[Boston$crim > 60, ])
#only 3 urban areas have crime rates of 60% and higher
hist(Boston$tax, breaks = 50)
nrow(Boston[Boston$tax > 600, ])
#137 urban areas have a tax rate of 600 or higher
#from the data set we see alot are at 666 tax rate so lets find out how many areas that is exaclty
nrow(Boston[Boston$tax == 666, ])
#There are 132 urban areas with the tax rate of 666. 
hist(Boston$ptratio, breaks = 50)
#from this historgram it can be seen that there are alot of areas near 21 so lets determine how many there
nrow(Boston[Boston$ptratio > 20 ,])
#There are 201 homes with a high ptratio
#Yes some of the suburbs appear to have high crime rates, Tax Rates appear to have some urban areas with high tax rates, although majority are below 50%
#The pupil teacher ratio seems to be uniform over the population set

#parte
y = count(Boston$chas)
print (y)
nrow(Boston[Boston$chas == 1 , ])
#there are 35 cities touching the Charles River

#partf

pupil = Boston$ptratio
s = summary(pupil)
print (s)
# the Median Student to Pupil ratio is 19.05

#partg
newBoston <- Boston[order(Boston$medv),]
print (newBoston)

row.names(Boston[min(Boston$medv), ])
#so suburb 5 has the smalled medv
range(Boston$ptratio)
#the range of ptratio for Boston is 12.6 to 22.0
Boston[min(Boston$medv), ]$ptratio
#finding out what suburb 5's ptratio: 18.7 so there are more students in this area than children

#part h

nrow(Boston[Boston$rm > 7.0,])
#There are 64 urban areas with homes with more than 7 rooms on average
nrow(Boston[Boston$rm > 8.0,])
#There are 13 urban areas with homes more than 8 rooms
#so 64-13 = 51 and so there are 51 areas that have 7 rooms on average

