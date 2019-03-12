realestate = "/Users/jennalindsey/Desktop/Marc/real estate data sim.csv"

re = read.csv(realestate, comment.char = "",stringsAsFactors=FALSE)
re = re[1:18,1:7]
print(re)
library(ggmap)
library(ggplot2)
library(reshape)

sorted.Rent = re[order(-re$Rent),]
re.debt = re[order(-re$Debt.Owed),]


re.debtonly = re.debt[1:10,]
re.debtonly = re.debtonly[order(-re.debtonly$Rent),]
#re.debtonly = re.debtonly[order(re.debtonly$EST.WORTH),]
print(sum(re.debtonly[3:10,5]))

re.paidoff = re.debt[11:18,]
print(re.paidoff)

#formating long and lat
lat = re$LAT...Y.
lon = re$LON..X.

loc = data.frame(lon,lat)



#correlationPLOT
library(dplyr,quietly=TRUE, warn.conflicts=FALSE)
library(GGally)
p = ggpairs(re[,2:7])
print(p)


#Calculations
#Variables
rent = sorted.Rent$Rent
worth = sorted.Rent$EST.WORTH
print('renting')
print(rent)
#sum Total

#Debt Calc
d.worth = re.debtonly$EST.WORTH
d.rent=re.debtonly$Rent
loan.total.per.house = re.debtonly$Debt.Owed
print(loan.total.per.house)
loan.payment.per.house = re.debtonly$Monthly.Payment.for.Debt
print(sum(loan.payment.per.house))
print(loan.total.per.house)
#Further Calculation
rent.sum = sum(rent)
total.loan.amount = sum(loan.total.per.house)

print(total.loan.amount)
total.payment.amount = sum(loan.payment.per.house)
print(total.payment.amount)
Net.Worth = sum(worth)



payoff = rent.sum - total.payment.amount

#quick Estimate
months = total.loan.amount / payoff


relative.stats = data.frame(rent.sum,total.payment.amount)
relative.stats = melt(relative.stats)
print(relative.stats)

bigpicture.stats = data.frame(total.loan.amount,Net.Worth)
bigpicture.stats = melt(bigpicture.stats)


#PLOTS

re.debt = re.debt %>% mutate ( ToHighLight = ifelse (re.debt$Debt.Owed == 0, 'Paid Off', ' Not Paid') )


v = get_googlemap(center ='Bradenton Florida', source = google, zoom = 13) 
#color = re.debt$Rent, shape = re.debt$ToHighLight,size = 4
#color = re$Rent
w = ggmap(v) + geom_point(aes(y = lon, x = lat,color = re.debt$Rent, shape = re.debt$ToHighLight,size = 4),data = re) + ggtitle("Rental Property Location Comparision by Rent Per Month") + guides(color=guide_legend(' Rent Amount'))
w =w + scale_shape(solid=T) + xlab('Longitude') +ylab('Latitude') + guides(shape=guide_legend('Loan'),color = guide_legend('Rent'),size = guide_legend('none')) + scale_color_gradient(low="black", high=" lightsalmon")


#trying to over lay points so that the red houses are the ones that are paid off


w2 = ggmap(v) + geom_point(aes(y = lon, x = lat,color = re$EST.WORTH, size = 4, shape = re.debt$ToHighLight),data = re) + ggtitle("Rental Property Location Comparision by Net Worth") + guides(color=guide_legend(' Net Worth'))

w2 =w2 + scale_shape(solid=T) + xlab('Longitude') +ylab('Latitude')+guides(shape=guide_legend('Loan'),color = guide_legend('Rental Property Worth in Dollars'),size = guide_legend('none')) + scale_color_gradient(low="black", high=" lightsalmon")
print(w2)
print(w)






print(re.debt)
p = ggplot(re.debt, aes(y= re.debt$EST.WORTH, x=re.debt$Address, color = re.debt$Rent, shape = re.debt$ToHighLight,size = 4)) + geom_point() + coord_flip()
p = p + ggtitle("Rental Property Net Worth Summary") + ylab("Rental Property Value") + xlab('Rental Houses')+ guides(shape=guide_legend('Debt Status'),color = guide_legend('Rent'),size = guide_legend('none'))
print(p)

p.stats = ggplot(relative.stats,aes(x= variable,y=value,fill = variable,label = value)) +geom_bar(stat="identity",position=position_dodge())+geom_text(size = 6,colour = 'red', position = position_stack(vjust = .9))
p.stats= p.stats + ggtitle("Total Rent Per Month vs Total Interest Payment") + ylab('Value in Dollars') + scale_fill_manual('Variables',labels = c('Total Rent Per Month', 'Total Interest Payment'),values = c('dark blue','light blue')) + theme(axis.text.x = element_blank() ,axis.title.x=element_blank())
print(p.stats)

pq.stats =ggplot(bigpicture.stats,aes(x= variable,y=value,fill = variable,label = value)) +geom_bar(stat="identity",position=position_dodge()) +geom_text(size = 6,colour = 'red', position = position_stack(vjust = .9))
pq.stats =pq.stats+ggtitle("Net Worth vs Total Loan Amount") + ylab('Value in Dollars') + scale_fill_manual('Variables',labels = c('Net Loan Amount', 'Net Worth Estimate'),values = c('dark blue',' light blue')) + theme(axis.text.x = element_blank() ,axis.title.x=element_blank())
print(pq.stats)

print(re.debtonly)

re.debtonly = within(re.debtonly, Debt.Owed <- factor(Debt.Owed,levels = names(sort(table(Debt.Owed),decreasing = T))))
pq = ggplot(re.debtonly, aes(y= Debt.Owed, x=re.debtonly$Address,fill = re.debtonly$Rent)) + geom_bar(stat="identity",position=position_dodge()) + coord_flip()
pq = pq + ggtitle("Debt Summary") + ylab("Rental Property Debt") + xlab('Rental Property')+ guides(fill=guide_legend(' Rent Amount',color=guide_legend(' none')))
pq =pq + scale_fill_gradient(low="white", high=" blue")
print(pq) 


a = multiplot(p.stats,pq.stats)



#need to loop into sums so that we have a plan of attack with the columns
#then one by one lose the payments value of that house for the sum variable. 

# basically in month intervals take the rent.sum from the total payment amount, where the difference
#is taken from one of the loan payments in some order. Once it hits zero, the payment for the loan
#is take out of the payment column. Repeating this until loan amount is zero 

i=1
print(loan.payment.per.house)
month.counter= 1
tracking = vector()
t.homes = vector()
t.loans = vector()

#Vacancy Cost
vacancy.repair = 2000
print(loan.total.per.house)
#simulation stratagy 1
while (total.loan.amount > 0){
  #probability for Vacancy
  vacancy.prop = runif(1,1,10)

 
  #Randomly Choosing house that is Vacant
  which.house = sample(i:length(loan.payment.per.house),1)
  
  print(month.counter)
  total.loan.amount = sum(loan.total.per.house)
  total.payment.amount = sum(loan.payment.per.house)
  payoff = rent.sum - total.payment.amount
  loan.total.per.house[i] = loan.total.per.house[i] - payoff
  
  if (loan.total.per.house[i] < 0) {
    loan.total.per.house[i+1] = loan.total.per.house[i+1] + abs(loan.total.per.house[i])
    loan.total.per.house[i] = 0
    loan.payment.per.house[i] = 0
    tracking[i] = c(month.counter)
    t.homes[i] = c(re.debtonly$Address[i])
    t.loans[i] = c(loan.payment.per.house[i])
    i = i + 1
    #print(i)
  }
  
  #Accounting for Vancancy 
  if (vacancy.prop < 1/6){
    print('Vacancy @')
    print(loan.payment.per.house[which.house])
    payoff = payoff - loan.payment.per.house[which.house] - vacancy.repair
  }
  #if (month.counter == 12){
    #break 
  #}
  month.counter = month.counter + 1
  
  
}

stats= data.frame(c(1:10),tracking,t.homes,t.loans)
print(stats)
library(ggrepel)
stat.plot = ggplot(stats,aes(y=stats$tracking,x=stats$c.1.10., label = re.debtonly$Address)) +geom_line()+geom_point()+ ggtitle("Rental Property Loan Free Timeline ")+ labs(subtitle = ' \n Strategy \n Paying off Lowest Renting Properties First')+ xlab('Rental Properties Paid Off') + ylab("Number of Months")
stat.plot = stat.plot + geom_label_repel(aes(label = re.debtonly$Address),hjust=0, vjust=4)
print(stat.plot)

firstyear = re.debtonly$Debt.Owed
total.loan = sum(re.debtonly)
lost = 750+527
debtpermo = 4887 - lost
print(debtpermo)
print(rent.sum*.25)
print(total.payment.amount)
debt.income = total.payment.amount / rent.sum * 100
print(debt.income)


