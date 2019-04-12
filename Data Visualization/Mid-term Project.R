

library(ggplot2)
library(dplyr)
library(reshape2)
library(gtable)
library(gridExtra )
library(wesanderson)

#EURO FIT DATA 
preyoga = c(15.1,3.64,17.98,25.14,9.61,15.67,15.06,1.63,.98)
postyoga = c(16.34,2.6,16.37,25.15,12.18,16.65,15.88,2.5,1.63)
cohenyoga=c(.6936,.2441,.4917,.0042,.4468,.1807,.1594,.2595,.2537)
variables = c("B.M.I. (kg/m2)",'Bent arm (seconds)','Plate tapping (seconds)','Shuttle run (seconds)','No. of sit-ups in 30 sec', ' Right hand grip strength (kg)', "Left hand grip strength (kg)", 'Flamingo balance right (no. of falls)','Flamingo balance left (no. of falls)')

prephy=c(15.36,3.99,17.46,26.09,8.63,15.19,14.31,1.29,1.56)
postphy=c(16.6,3.29,16.85,25.44,11.37,15.67,14.98,2.83,2.19)
cohenphy=c(.4681,.0895,.1885,.2687,.4638,.0843,.1169,.4705,.2035)



#BATTLES SELF-ESTEEM DATA

battle_pre_yoga = c(33.51,11.68,5.62,7.89,8.32)
battle_variables = c("Total Score of S.E.", 'General S.E.', 'Social S.E.','Academic S.E', 'Parental S.E')
battle_post_yoga = c(36.98,13.68,6.06,8.28,8.96)
battle_pre_pe = c(35.13,12.36,6.43,7.91,8.43)
battle_post_pe = c(36.74,13.28,6.77,8.02,8.68)
battle_yoga_cohen = c(.6088,.7836,.2698,.2523,.3662)
battle_pe_cohen = c('.2921','.3345','.2214','.0644','.1865')


#Stroop Color-word task
stroop_variables = c('Word raw score', 'Word T score', 'Color raw score', 'Color T score', 'Color-word raw score', 'Color-word T score', 'Interference raw score', 'Interference T score')
stroop_preyoga = c(63.63,48.7,43.86,42.12,25.77,38.35,-18.09,46.09)
stroop_postyoga = c(68.7,51.33,48.63,44.47,29.3,42.6,-19.56,48.23)
stroop_cohenyoga = c(.2528,.2447,.5123,.3965,.4561,.4775,.1831,.2344)

stroop_prepe= c(61.6,47.3,43.09,41.21,27.88,40.88,-15.21,43.28)
stroop_postpe = c(68.65,51.09,50.02,45.88,30.86,44.93,-19.35,47.88)
stroop_cohenpe=c(.3801,.3788,.714,.7538,.4119,.5057,.5330,.4962)

#Teacher Survey
teach_variables = c('Obedience','Academic Performance','Attention','Punctuality', 'Behaviors with Friends','Behavior with Teachers')
teach_preyoga =c(6.05,5.25,5.48,6.28,6.61,7.16)
teach_postyoga = c(7.59,7.48,7.42,7.87,7.78,8.35)
teach_cohenyoga = c(.7847,1.0628,1.0704,.8366,.767,.8206)

teach_prepe = c(5.98,5.13,5.32,6.23,6.53,7.01)
teach_postpe = c(7.68,7.733,7.71,7.94,7.86,8.39)
teach_cohenpe = c(.9808,.2158, 1.1776,.7952,.8794,1.0034)

#battle Data_frame

battle = data.frame(battle_variables,battle_pre_yoga,battle_post_yoga,battle_pre_pe,battle_post_pe)
mbattle = melt(battle , id.vars = c('battle_variables'))

#battle plot
battle_plot = ggplot(mbattle, aes(x = battle_variables, y=value, fill = variable))+guides(fill = guide_legend(reverse=TRUE)) + coord_flip() + geom_bar(stat="identity",position=position_dodge())
battle_plot= battle_plot + labs(x = 'Score', caption = "Max Achievable points: \n Social, Parental, and Academic Self- 10 \n General Self Esteem - 20 \n Total Self Esteem - 50 ") + ggtitle('Self-Esteem Metrics') + scale_fill_manual(labels = c('Pre Yoga','Post Yoga' ,'Pre Physical Exercise','Post Physical Exercise') , values=c("#ffa900","#e86f00",'#b800ff','#600085'))+ theme_minimal() +theme(plot.background = element_rect(fill = "#f5f5f5"),axis.title.y=element_blank(),axis.title.x=element_blank())
print(battle_plot)


#EUROFIT DATAFRAME

eurofit = data.frame(variables,preyoga,postyoga,prephy,postphy)
meurofit = melt(eurofit, id.vars = c('variables'))


#eurofit graph
europlot = ggplot(meurofit, aes(x = variables, y = value, fill = variable))+guides(fill = guide_legend(reverse=TRUE)) + coord_flip() + geom_bar(stat="identity",position=position_dodge())
europlot = europlot + ggtitle("Eurofit Physical Fitness Battery Metric")+ scale_fill_manual(labels = c('Pre Yoga','Post Yoga' ,'Pre Physical Exercise','Post Physical Exercise'),values=c("#ffa900","#e86f00",'#b800ff','#600085'))+ theme_minimal() +theme(plot.background = element_rect(fill = "#f5f5f5"),axis.title.x=element_blank(),axis.title.y=element_blank())

#print(europlot)

#Cohen dataframes Eurofit & Battle and plot
eurofit_cohen = data.frame(variables,cohenyoga,cohenphy)
print(eurofit_cohen)
melt_fitcohen = melt(eurofit_cohen, id.vars = c('variables'))


battle_cohen = data.frame(battle_variables,battle_yoga_cohen,battle_pe_cohen)
melt_battle_cohen = melt(battle_cohen, id.vars = c('battle_variables'))

 

#COHEN COMPARisson Graph

eurocohen = ggplot(melt_fitcohen,aes(x=variables,y=value,fill = variable))+ labs(title ='Standardized Difference of Two Means:Cohen d', subtitle = 'Eurofit Metric') + coord_flip() + geom_bar(stat="identity",position=position_dodge())
eurocohen =eurocohen+guides(fill = guide_legend(reverse=TRUE))+scale_y_continuous(name="Effect Size Score")+ scale_fill_manual('Group',labels = c('YOGA','PHYSICAL EDUCATION' ),values=c("#ffa900",'#b800ff'))+ theme_minimal() +theme(plot.background = element_rect(fill = "#f5f5f5"),axis.title.y=element_blank())
#print(eurocohen)

battlecohen = ggplot(melt_battle_cohen,aes(x = battle_variables,y=value,fill=variable)) + ggtitle('Standardized Difference of Two Means:Cohen d', subtitle = 'Self-Esteem Metric') + coord_flip() + geom_bar(stat="identity",position=position_dodge())
battlecohen = battlecohen + ylab('Effect Size Score') + scale_fill_manual(labels = c('YOGA','PHYSICAL EDUCATION' ),'Group',values=c("#ffa900",'#b800ff'))+ theme_minimal() + theme(plot.background = element_rect(fill = "#f5f5f5"),axis.title.y=element_blank())
#print(battlecohen)

#Stroop Dataframe

stroop = data.frame(stroop_variables,stroop_preyoga,stroop_postyoga,stroop_prepe,stroop_postpe)
melt_stroop = melt(stroop, id = 'stroop_variables')

stroop_cohen = data.frame(stroop_variables,stroop_cohenyoga,stroop_cohenpe)
melt_stroop_cohen = melt(stroop_cohen, id = 'stroop_variables')

#Teacher Dataframe

teach = data.frame(teach_variables,teach_preyoga,teach_postyoga,teach_prepe,teach_postpe)
melt_teach = melt(teach,id = 'teach_variables')

teach_cohen = data.frame(teach_variables,teach_cohenyoga,teach_cohenpe)
melt_teach_cohen = melt(teach_cohen,id ='teach_variables')

#STROOP Graph

sgg=ggplot(melt_stroop, aes(x= stroop_variables, y=value,fill = variable)) +guides(fill = guide_legend(reverse=TRUE)) + coord_flip() + ggtitle('Stroop-Word Test') + geom_bar(stat="identity",position=position_dodge())
sgg = sgg + ylab('Score') + scale_fill_manual(labels = c('Pre Yoga','Post Yoga' ,'Pre Physical Exercise','Post Physical Exercise'),values=c("#ffa900","#e86f00",'#b800ff','#600085'))+  theme_minimal() + theme(plot.background = element_rect(fill = "#f5f5f5"),axis.title.y=element_blank())

sggcoh = ggplot(melt_stroop_cohen, aes(x= stroop_variables, y=value,fill = variable)) + ggtitle('Standardized Difference of Two Means:Cohen d', subtitle = 'Stroop-Word Test')+ coord_flip() + geom_bar(stat="identity",position=position_dodge())
sggcoh = sggcoh +guides(fill = guide_legend(reverse=TRUE))+ ylab('Effect Size Score') + scale_fill_manual(labels = c('YOGA', 'PHYSICAL EXERCISE'),values=c("#ffa900",'#b800ff'))+  theme_minimal() + theme(plot.background = element_rect(fill = "#f5f5f5"),axis.title.y=element_blank())
print(melt_stroop)
print(sggcoh)
#print(sgg)

#Teacher Graph
tgg=ggplot(melt_teach, aes(x= teach_variables, y=value,fill = variable)) +guides(fill = guide_legend(reverse=TRUE))+ coord_flip() + geom_bar(stat="identity",position=position_dodge())
tgg = tgg + ylab('Score') + ggtitle('Teacher Survey' )+ labs(caption = ' \n  Max Achievable Score: 10') + scale_fill_manual(labels = c('Pre Yoga','Post Yoga' ,'Pre Physical Exercise','Post Physical Exercise'),values=c("#ffa900","#e86f00",'#b800ff','#600085'))+  theme_minimal() + theme(plot.background = element_rect(fill = "#f5f5f5"),axis.title.y=element_blank())

tggcoh = ggplot(melt_teach_cohen, aes(x=teach_variables, y=value,fill = variable)) + ggtitle('Standardized Difference of Two Means:Cohen d', subtitle = 'Teacher Survey') + coord_flip() + geom_bar(stat="identity",position=position_dodge())
tggcoh = tggcoh +guides(fill = guide_legend(reverse=TRUE))+ ylab('Effect Size Score') + scale_fill_manual(labels = c('YOGA', 'PHYSICAL EXERCISE'),values=c("#ffa900",'#b800ff'))+  theme_minimal() + theme(plot.background = element_rect(fill = "#f5f5f5"),axis.title.y=element_blank())

#print(tggcoh)
print(tgg)


allfour = grid.arrange(sgg, tgg,europlot,battle_plot, nrow = 2)
print(allfour)

allfourco = grid.arrange(sggcoh, tggcoh,eurocohen,battlecohen, nrow = 2)
print(allfourco)
