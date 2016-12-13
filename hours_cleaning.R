setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library('magrittr',lib='C:/Progra~1/R/R-3.1.3/library')
library('reshape2',lib='C:/Progra~1/R/R-3.1.3/library')
hours12<- read.csv('hours0212.csv', na.strings= c(""," ", "NA"))[,-1]
hours1314<- read.csv('20132014hoursdata.csv', na.strings= c(""," ", "NA"))

#ensure column orders and names are correct
hours1314<- hours1314[,c(13,1:12)]
colnames(hours12)[names(hours12) %in% 'Job.Number']<-'Job.num'
colnames(hours12)[names(hours12) %in% 'Discipline.1']<-'Discipline'


#what is WIP only invoice??

#clean up
# any columns unnecessary?
summary(hours1314)
#not really
#remove NA's from hours1314 first. look into full NA rows
dim(hours)
hours1314[rowSums(is.na(hours1314))== 13,] %>% nrow()
hours1314<-hours1314[rowSums(is.na(hours1314))!=12,]
hours1314<-hours1314[rowSums(is.na(hours1314))!=13,]
#now to rows with 3 values
hours1314[rowSums(is.na(hours1314))== 9,] %>% head()
#are all the charge/cost values 0?? yes - only details - delete these rows
hours1314<-hours1314[rowSums(is.na(hours1314))!=9,]
#investigate last 9 rows of NA in charge
hours1314[is.na(hours1314$Charge.Amount..B...AUD.),] %>% head()
#these were full NA rows
hours1314<-hours1314[rowSums(is.na(hours1314))!=13,]

#remove NA's from hours12 now. look into full NA rows
summary(hours12)
hours12[rowSums(is.na(hours12))== 12,] %>% head()
hours12<-hours12[rowSums(is.na(hours12))!=13,]
hours12<-hours12[rowSums(is.na(hours12))!=12,]
#now to rows with 3 values
hours12[rowSums(is.na(hours12))== 9,] %>% head()
#are all the charge/cost values 0?? yes - only details - delete these rows
hours12<-hours12[rowSums(is.na(hours12))!=9,]
#investigate last 9 rows of NA in charge
hours12[is.na(hours12$Charge.Amount..B...AUD.),] %>% head()
#these were full NA rows
hours12<-hours12[rowSums(is.na(hours12))!=13,]

#### rbind two datasets #########
hours<- hours12 %>% rbind(hours1314)
hours<- hours %>% arrange(Job.num)
#rename columns
colnames(hours)[names(hours) %in% 'Charge.Amount..B...AUD.']<-'Charge'
colnames(hours)[names(hours) %in% 'Cost.Amount..A...AUD.']<-'Cost'
colnames(hours)[names(hours) %in% 'time_CreatedBy']<-'input.by'
colnames(hours)[names(hours) %in% 'Date.work.performed']<-'Work.Date'
colnames(hours)[names(hours) %in% 'Reconciled.Invoice']<-'Invoice'

####### MORE CLEANING ##########
#delete anything from 2015!!! 
#break apart dates and times
hours$Created.Date<- as.character(hours$Created.Date)
hours$Created.Time<- sapply(hours$Created.Date,FUN=function(x){strsplit(x,split='[ ]')[[1]][2]})
hours$Created.Date<- sapply(hours$Created.Date,FUN=function(x){strsplit(x,split='[ ]')[[1]][1]})
class(hours$Created.Date)
hours$Created.Date<- as.Date(hours$Created.Date, "%d/%m/%Y")
hours<- hours %>% filter(Created.Date< as.Date('2015-01-01'))

#######ENGINEERING VARIABLES##########
#new column for disbursements

hours$Disburse<- c(rep(0,nrow(hours)))

#investigate very low hours entries#
hours %>% filter(Job.num == '2011.246.201')
#strange - peadar wrote down .75 hours and invoiced $7000
hours %>% filter(Job.num == '2005.019.1') %>% unique()
# invoiced $108000 for  21 hours of work - check invoicing data 
hours %>% filter(Job.num == '2012.172.201') %>% unique()

#dont include rows with 'km' or mileage or printing anywhere
#grep anything with '0' in Cost column--> set hours and charge to '0' and move Charge to Disbursements column
#grep anything with 'km or mileage and move charge to disbursements. set hours charge and cost to zero
#if the user contains 'disbursement' move charge over to disbursements, set hours, charge, cost to zero

hours$Disburse<- ifelse(abs(hours$Cost) <2 |
                                grepl("mileage|[0-9]km|\\bkm",hours$Details, ignore.case=TRUE)|
                                grepl("disburse",hours$User, ignore.case=TRUE),
                        ifelse(hours$Charge>hours$Cost, hours$Charge, hours$Cost), hours$Disburse)
hours$Charge<- ifelse(abs(hours$Cost) <2 |
                              grepl("mileage|[0-9]km|\\bkm",hours$Details, ignore.case=TRUE)|
                              grepl("disburse",hours$User, ignore.case=TRUE),
                        0, hours$Charge)
hours$Hours<- ifelse(abs(hours$Cost) <2 |
                             grepl("mileage|[0-9]km|\\bkm",hours$Details, ignore.case=TRUE)|
                             grepl("disburse",hours$User, ignore.case=TRUE),
                      0, hours$Hours)
hours$Cost<- ifelse(abs(hours$Cost) <2 |
                            grepl("mileage|[0-9]km|\\bkm",hours$Details, ignore.case=TRUE)|
                            grepl("disburse",hours$User, ignore.case=TRUE),
                     0, hours$Cost)

#if the ratio of cost/hours <1, set everything equal to zero and copy charge to disbursements column
hours$Disburse<- ifelse(hours$Cost>0 & hours$Hours >0,
                        ifelse(hours$Cost/hours$Hours <2,
                               ifelse(hours$Charge>hours$Cost, hours$Charge, hours$Cost), hours$Disburse),
                        hours$Disburse)

hours$Charge<- ifelse(hours$Cost>0 & hours$Hours >0,
                      ifelse(hours$Cost/hours$Hours <2,
                             0, hours$Charge),
                      hours$Charge)
hours$Hours<- ifelse(hours$Cost>0 & hours$Hours >0,
                     ifelse(hours$Cost/hours$Hours <2,
                            0, hours$Hours),
                     hours$Hours)
hours$Cost<- ifelse(hours$Cost>0 & hours$Hours >0,
                    ifelse(hours$Cost/hours$Hours <2,
                           0, hours$Cost),
                    hours$Cost)


########## CREATE NEW SUMMARY DATAFRAME ###############
#summarise by summing total cost and charge amounts for each job number
hours<- unique(hours)
write.csv(hours, 'hours.csv')
hours<- read.csv('hours.csv')[,-1]
tot.cost<- hours%>%
        group_by(Job.num) %>%
        summarise(charge = sum(Charge, na.rm=TRUE), cost= sum(Cost, na.rm=TRUE), hours= sum(Hours, na.rm=TRUE), Dis.subcon= sum(Disburse, na.rm=TRUE))

########## CHECK NEW SUMMARY DATA FRAME BY CHECKING WITH ALL DATAFRAME ###############
#compare summarised charges and costs with all data. all4d
all4d<- read.csv('all4d.csv')[,-1]
compare<- all4d %>% select(Job.Number, Tot.TS.Charge, Tot.TS.Cost, TS.Hours)
compare1<- tot.cost %>% 
        merge(compare, by.x= 'Job.num', by.y= 'Job.Number') %>%
        arrange(Job.num)

#from compare, extract rows where the costs do not equal
# compare1 %>% 
#         filter(charge>Tot.TS.Charge | charge<Tot.TS.Charge|
#                        cost>Tot.TS.Cost| cost< Tot.TS.Cost| hours< TS.Hours| hours> TS.Hours) %>%
#         nrow()
#1271 rows differ

#after some investigation , 2004.250 has doubled itself. not sure why. check if this is the case in the original hours
#yes it is the case in the original!!
#need to delete half of the rows
# hours %>% unique %>% nrow()
#144 rows. Only want to keep the first half. use unique()?
#add a column in compare 1 for charge/Tot.TS.Charge
compare1<- transform(compare1, ratio= charge/Tot.TS.Charge)
compare1 %>% filter(ratio == 2) %>% nrow()
#fixed

#create two columns: one for ratio of Charge to hours for both datasets
compare1<- transform(compare1, charge.ph= charge/hours)
compare1<- transform(compare1, charge.ph2= Tot.TS.Charge/TS.Hours )
compare1<- transform(compare1, charge.pc= charge/cost)
compare1<- transform(compare1, charge.pc2= Tot.TS.Charge/Tot.TS.Cost)
#round to 2 digits
compare1[,8:12]<- compare1 %>% select(ratio, charge.ph, charge.ph2, charge.pc, charge.pc2) %>% round(2)

#try filtering them down to ones that differ by more than $500
compare1 %>% 
        filter(abs(charge-Tot.TS.Charge)/charge >.01|
                       abs(cost-Tot.TS.Cost)/cost >.01 | 
                       abs(hours- TS.Hours)/hours> .02, charge.pc2<2.6 & charge.pc2>1) %>%
        View()

#look at jobs individually
hours %>% filter(User == 'Brian Jacobsen') %>% View()

#finally happy with the technique.

############ TRIM BACK COMBINED DATA SET AND SAVE########### 
#final data set
Tot.TS<- compare1 %>% select(Job.num, charge, cost, Dis.subcon, hours, charge.ph, charge.pc)
write.csv(Tot.TS, 'Tot_TS.csv')

############ DE_IDENTIFY  #####################

#de-identify employees
# summary(hours$input.by)
# summary(hours$User)
#use User column
#load staff codes
code.u<- read.csv('code_users2.csv')[,-1]
hours<- read.csv('hours.csv')[,-1]
hours1<- merge(hours, code.u, by.x= 'User', by.y= 'User', all.x=TRUE)
#rename columns
colnames(hours1)[names(hours1) %in% 'code']<-'user.code'
colnames(hours1)[names(hours1) %in% 'Discipline.y']<-'user.disc'
colnames(hours1)[names(hours1) %in% 'Discipline.x']<-'Discipline'

#is user position NA registering as unknown?
hours1 %>% filter(is.na(User)) %>% head()
hours1['86',6]<- '2014-'
#fix a date in hours1
hours1[hours1$Work.Date %in% '4/12/1912', names(hours1) %in% 'Work.Date'] <- '12/12/2013'
#yes it is working

#delete User column
hours1<- hours1 %>% select(-User, -input.by)
write.csv(hours1, 'hours_clean.csv')
Job.Hours<- Job.Hours %>% unique

write.csv(Job.Hours, 'Job_hours.csv')

