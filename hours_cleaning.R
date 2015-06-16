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

hours1<- read.csv('hours_clean.csv')[,-1]


############ ENGINEER MORE VARIABLES  #####################

# number of diff people who worked on a job
V1<- hours1[!(hours1$user.code %in% c('disburse', 'Admin')), names(hours1) %in% c('Job.num', 'user.code')]
V1 <- V1 %>% arrange(Job.num) %>%
        unique() 
V1a<- ddply(V1, .(Job.num), nrow)
#rename columns
colnames(V1a)[names(V1a) %in% 'V1']<-'no.users'
V1a %>% arrange(-no.users) %>% head()



# variables: % hours by each position

V2<- hours1[!(hours1$user.code %in% c('disburse', 'Admin')), 
            names(hours1) %in% c('Job.num', 'Position', 'Hours' )]

#reshape data - sum number of hours for each positon, then convert into percentage of total hours

V2a<- acast(V2, Job.num~Position, sum, value.var= 'Hours')
V2b<- round(prop.table(as.matrix(V2a),1)*100,2)
V2b<- na.omit(V2b)
#rename columns
V2b<- as.data.frame(V2b)
colnames(V2b)[names(V2b) %in% 'Contract Technical']<-'pc.contracttech'
colnames(V2b)[names(V2b) %in% 'Director']<-'pc.director'
colnames(V2b)[names(V2b) %in% 'Grad Professional']<-'pc.gradpro'
colnames(V2b)[names(V2b) %in% 'Mid Professional']<-'pc.midpro'
colnames(V2b)[names(V2b) %in% 'Mid Technical']<-'pc.midtech'
#colnames(V2b)[names(V2b) %in% 'Principal Professional']<-'pc.princpro'
colnames(V2b)[names(V2b) %in% 'Senior Professional']<-'pc.seniorpro'
colnames(V2b)[names(V2b) %in% 'Senior Technical']<-'pc.seniortech'
colnames(V2b)[names(V2b) %in% 'Unknown']<-'pc.unknown'



# variable: % hours by techies, pros, unknown
pros<- c('pc.director', 'pc.gradpro', 'pc.midpro', 'pc.seniorpro')
tech<- c('pc.contracttech', 'pc.midtech', 'pc.seniortech')
#new column summing percentages in certain columns
V3<- V2b
V3$pc.pro<- apply(V2b[,names(V2b) %in% pros], 1, sum)
V3$pc.tech<- apply(V2b[,names(V2b) %in% tech], 1, sum)
#leave pc.unknown column as this will function for total percent as well
#visualise
percent.plot<- melt(V2b,value.name='percent.hrs') %>% filter(percent.hrs>.5)
pos.plot<- ggplot(percent.plot, aes(x=variable, y= percent.hrs,
                         colour= variable)) + 
        geom_boxplot(alpha=0) +geom_jitter(alpha= I(1/2))
        

#### variable: Start date in month
hours1$Work.Date<- as.Date(hours1$Work.Date, "%d/%m/%Y")
hours1<- hours1 %>% arrange(Job.num, Work.Date)
#first get rid of date outliers
date.plot<- ggplot(hours1[2000:4000,], aes(x=Job.num, y= Work.Date,
                                    colour= Job.num)) + 
        stat_boxplot(alpha=0, coef=2.2)+
        theme(legend.position="none", axis.text.x=element_text(angle=45,hjust=1),
              text=element_text(size=12))
#coefficient of 2.2 seems to work well
# x=hours1[1:13,]$Work.Date
# test<-as.Date(quantile(as.numeric(x)),origin="1970-01-01")
# limit1= (test[4]-test[1])*2.2 + test[4]
# limit2= test[2]-(test[4]-test[1])*2.2


temp<- hours1[!is.na(hours1$Work.Date),c('Job.num','Work.Date')]


hours2<- temp
# hours2<- ddply(temp, .(Job.num), function(x){
#         quants = as.Date(quantile(as.numeric(x$Work.Date)),origin="1970-01-01")
#         limit1 = (quants[4]-quants[1])*1.8 +quants[4]
#         limit2 = quants[2]-(quants[4]-quants[1])*1.8
#         x[as.numeric(x$Work.Date - limit1)*as.numeric(limit2 - x$Work.Date) > 0,]
# })
# 
# date.plot2<- ggplot(hours2[2000:4000,], aes(x=Job.num, y= Work.Date,
#                                         colour= Job.num)) + 
#         stat_boxplot(alpha=0, coef=2.2)+
#         theme(legend.position="none", axis.text.x=element_text(angle=45,hjust=1),
#               text=element_text(size=12))

#I'm happy with a coefficient of 1.8 in my hours2 function

#finally, on to start date - skim head and tail for date in each job
#skim the top entry from every job to be the 'start date'
V4<- ddply(hours2, .(Job.num), head, 1)
colnames(V4)[names(V4) %in% 'Work.Date']<-'Start.Date'
V4a<- ddply(hours2, .(Job.num), tail, 1)
colnames(V4a)[names(V4a) %in% 'Work.Date']<-'End.Date'
V4$End.Date<- V4a$End.Date
#create year column
V4$Year<- format(V4$Start.Date,"%Y")
V4 %>% head()


#### variable: timespan of job hours
V4<- transform(V4, timespan= End.Date- Start.Date)

##### variable: number of days worked on a job
#need to count unique job number and work.Date combinations - ddply
#below ddply gives a list of Job nUmbers and Work Dates and number of times someone recorded hours on a WorkDate
V6<- ddply(hours1, .(Job.num, Work.Date), nrow)
#investigate high jobs
hours1 %>% filter(Job.num =='2009.238.1' & Work.Date == '3/03/2010') %>% head(20)
#find mean number of people working on each job per day as well as total days worked on each job
V6a<- ddply(V6, .(Job.num), nrow)
colnames(V6a)[names(V6a) %in% 'V1']<-'Num.days'
V6b<- ddply(V6, .(Job.num), summarise, mean.peeps = mean(V1))


#### variable: # of user.disciplines
#first delete unknown user.discipline
hours3<- hours1 %>% filter(!(user.disc == 'Unknown'))
V5<- ddply(hours3, .(Job.num, user.disc), nrow)
V5a<- ddply(V5, .(Job.num), nrow)
colnames(V5a)[names(V5a) %in% 'V1']<-'Num.disc'

##### COMBINE ALL ENGINEERED VARIABLES INTO FINAL JOB.HOURS DATA SET ######
#first load summed charges, costs, hours
Job.Hours<- read.csv( 'Tot_TS.csv')[,-1]
#merge with V1a - num users who worked on job
Job.Hours <- merge(Job.Hours, V1a, by= 'Job.num', all.x= TRUE)

#merge with V3 percent by each position/group
Job.Hours<- merge(Job.Hours, V3, by.x= 'Job.num', by.y= 0, all.x= TRUE)

#merge with timespan V4
Job.Hours<- merge(Job.Hours, V4, by= 'Job.num',all.x= TRUE)

#merge with V5a - number of disciplines
Job.Hours<- merge(Job.Hours, V5a, by= 'Job.num',all.x= TRUE)

#merge with V6a and V6b - number of days and users per day
Job.Hours<- merge(Job.Hours, V6a, by= 'Job.num',all.x= TRUE)
Job.Hours<- merge(Job.Hours, V6b, by= 'Job.num',all.x= TRUE)
Job.Hours<- transform(Job.Hours, hours.perday= hours/Num.days)


write.csv(Job.Hours, 'Job_hours.csv')

