setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library('magrittr',lib='C:/Progra~1/R/R-3.1.3/library')
library('reshape2',lib='C:/Progra~1/R/R-3.1.3/library')

##### COMBINE CLEANED DATA SETS BEFORE DE-IDENTIFICATION #########

#combining all dataset with Job hours data set and Inv_sum data set.
#Job.Hours data set columns need to replace some of the all columns because they are more accurate. 
# these include: Start.Date, tot.TS.Charge, tot.TS.cost, tot.TS.hours, Year

all4e<- read.csv('all4e.csv')[,-1]
str(all4e)
#delete Start.Date, tot.TS.Charge, tot.TS.cost, tot.TS.hours, Year
all4e<- all4e %>% select(-Start.Date, -Tot.TS.Cost, -Tot.TS.Charge, -TS.Hours, -Year, -Profit, -Tot.Invoiced)

#combine Inv.Sum
Inv.sum<- read.csv('inv_sum.csv')[,-1]
all4e<- merge(all4e, Inv.sum %>% select(Job.Number, Tot.Invoiced), by= 'Job.Number', all.x=TRUE, all.y=FALSE)
#set NA = 0
all4e[is.na(all4e$Tot.Invoiced),]$Tot.Invoiced <- 0

#combine Job.Hours
Job.Hours<- read.csv('Job_hours.csv')[,-1]
head(Job.Hours)
all5<- merge(all4e, Job.Hours, by.x='Job.Number', by.y= 'Job.num', all.x=TRUE, all.y=FALSE)
str(all5)

#create new 'profit' and 'balance' column
#profit column
all5<- transform(all5, profit= Tot.Invoiced-charge-Dis.subcon)
#balance column - subtract real costs
all5<- transform(all5, balance= Tot.Invoiced-cost-Dis.subcon)
all5<- all5[-c(2068:2070),]
write.csv(all5, 'all5.csv')
all5<- read.csv('all5.csv')[,-1]

#Need to sum up all the profits and balances for jobs with milestones
# first 
#create subset of just the things i want to add up - profits, balance, hours in each milestone
temp<- all5 %>% select(Job.Number, hours, profit, balance, cost, Dis.subcon, Tot.Invoiced)
temp$Job.Number<- as.character(temp$Job.Number)
temp1<- temp[1:2426,]
#string split to create another column with the first part of a job number. For the first half of jobs just the first bit indicates it could be invoiced together ie 2002.001 by itself
temp1$mlsto<-sapply(temp1$Job.Number,FUN=function(x){
        paste(strsplit(x,split='[.]')[[1]][1], strsplit(x,split='[.]')[[1]][2], sep=".")})
#Then ddply using this shortened job number to group. Add up all balance, profit hours in shortened job number. MErge back with original data set.
temp1<- merge(temp1, ddply(temp1, .(mlsto), summarise, profit.mlsto= sum(profit), balance.mlsto = sum(balance), hrs.mlsto = sum(hours), cost.mlsto=sum(cost),
                           dis.sc.mlsto = sum(Dis.subcon),  inv.mlsto=sum(Tot.Invoiced)),
              by.x= 'mlsto', by.y= 'mlsto', all.x=TRUE, all.y=FALSE)

#do the same with second half of data
temp2<- temp[2427:4169,]
temp2$mlsto<-sapply(temp2$Job.Number,FUN=function(x){
        substr(x,1,10)})
temp2$mlsto<- ifelse(temp2$mlsto == '2011.072.3', temp2$Job.Number, temp2$mlsto)
temp2<- merge(temp2, ddply(temp2, .(mlsto), summarise, profit.mlsto= sum(profit), balance.mlsto = sum(balance), hrs.mlsto = sum(hours), cost.mlsto=sum(cost),
                           dis.sc.mlsto= sum(Dis.subcon), inv.mlsto=sum(Tot.Invoiced)),
              by.x= 'mlsto', by.y= 'mlsto', all.x=TRUE, all.y=FALSE)

#rbind temp1 and temp2 together.
temp<- rbind(temp1, temp2)

dim(temp)
all5<- merge(all5, temp[,names(temp) %in% c('mlsto','Job.Number','profit.mlsto','balance.mlsto','hrs.mlsto','cost.mlsto','dis.sc.mlsto', 'inv.mlsto')],
             by='Job.Number')
all5 <- transform(all5, return.pdol= balance.mlsto/(cost.mlsto+dis.sc.mlsto))
all5 <- transform(all5, inv.vs.cost= inv.mlsto/(cost.mlsto+dis.sc.mlsto))

all5<- write.csv(all5, 'all5a.csv')
# go to de_identify.R
