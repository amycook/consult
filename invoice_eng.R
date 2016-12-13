setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library('magrittr',lib='C:/Progra~1/R/R-3.2.0/library')


Invoices.a<-read.csv('invoices_clean.csv')[,-1]
Inv.sum<-read.csv('inv_sum.csv')[,-1]


#### ENGINEERING VARIABLES ######

#need to do by milestone!! because this is what relates directly to the return.pdol variable which we are measuring as 'success'

#Need to sum up all the profits and balances for jobs with milestones
# first 
#create subset of just the things i want to add up - profits, balance, hours in each milestone
temp<- Invoices.a %>% select(Job.Number, Inv.Amount, Invoice.Date)
temp<- temp %>% arrange(Job.Number)
temp$Invoice.Date <- as.character(temp$Invoice.Date)
temp$Job.Number <- as.character(temp$Job.Number)
#create column for Date of prev invoice, prev job number, and prev invoiced amount
temp$testp.Invoice<- c(NA, temp$Invoice.Date[-nrow(temp)])
temp$prev.JN<- c(NA, temp$Job.Number[-nrow(temp)])
temp$prev.inv<- c(NA, temp$Inv.Amount[-nrow(temp)])
#new column which prints 'NA" if prev job.number is not the same as current OR if prevInv <=0
temp$prev.Invoice<- ifelse(temp$Job.Number == temp$prev.JN & temp$prev.inv>0 & temp$Inv.Amount>0, temp$testp.Invoice, NA)
temp<- temp %>% select(-testp.Invoice, -prev.JN, -prev.inv)
temp$Invoice.Date<- as.Date(temp$Invoice.Date)
temp$prev.Invoice<- as.Date(temp$prev.Invoice)

#create new column for milestone prefix
temp1<- temp[1:7680,]
#string split to create another column with the first part of a job number. For the first half of jobs just the first bit indicates it could be invoiced together ie 2002.001 by itself
temp1$mlsto<-sapply(temp1$Job.Number,FUN=function(x){
        paste(strsplit(x,split='[.]')[[1]][1], strsplit(x,split='[.]')[[1]][2], sep=".")})
temp1$mlsto<- ifelse(temp1$mlsto %in% c('2004.056','2010.236'), temp1$Job.Number, temp1$mlsto)
#do the same with second half of data
temp2<- temp[7681:11627,]
temp2$mlsto<-sapply(temp2$Job.Number,FUN=function(x){
        substr(x,1,10)})
temp2$mlsto<- ifelse(temp2$mlsto %in% c('2011.072.3','2011.063.3','2012.453.3'), temp2$Job.Number, temp2$mlsto)
#rbind temp1 and temp2 back together to make temp
temp<- rbind(temp1, temp2)

#now create engineered variables using mlsto column in ddply

### VAR1 ###

#number of invoices per mlsto
v1<- ddply(temp %>% filter(Inv.Amount>0), .(mlsto), nrow)
#mean size of invoice per project
temp<- transform(temp, btwn.inv = Invoice.Date-prev.Invoice)
v2<- ddply(temp %>% filter(Inv.Amount>0), .(mlsto), summarise, 
           mean.inv = mean(Inv.Amount) %>% round(2))
#mean freq of invoice per mlsto
v3<- ddply(temp %>% filter(Inv.Amount>0 & btwn.inv>0), .(mlsto), summarise, 
           Inv.freq = mean(na.omit(btwn.inv)) %>% round(0))
#num of negative invoices for project
v4<- ddply(temp %>% filter(Inv.Amount<0), .(mlsto), nrow)


#now to add client codes as column
all5a<-read.csv('all5a.csv')[,-1]
all5a<- all5a %>% select(Job.Number, client2, inv.mlsto)
#substitute all the codes in
#client2
code.client2<- read.csv('Code_client2.csv')
all5a<- merge(all5a, code.client2[,2:3], by= 'client2', all.x=TRUE)
colnames(all5a)[names(all5a) %in% 'code']<-'code.client'
#code.jobnum
code.job<- read.csv('code_job.csv')[,-1]
all5a<- merge(all5a, code.job, by.x= 'Job.Number', by.y= 'job1', all.x=TRUE)
colnames(all5a)[names(all5a) %in% 'code2']<-'code.jobnum'

#add client code and code.jobnum as column to temp
temp<- merge(temp, all5a %>% select(Job.Number,code.client, inv.mlsto, code.jobnum), by = 'Job.Number', all.x=TRUE) 
#delete all rows where code.jobnum == NA
temp<- temp %>% filter(!is.na(code.jobnum))


#start making summary variables by client

#mean invoice size for client
v5<- ddply(temp %>% filter(Inv.Amount>0), .(code.client), summarise, 
           client.meaninv = mean(Inv.Amount) %>% round(2))
#mean inv freq for client
v6<- ddply(temp %>% filter(Inv.Amount>0 & btwn.inv>0), .(code.client), summarise, 
           client.invfreq = mean(na.omit(btwn.inv)) %>% round(0))
#number of neg inv for client
v7<- ddply(temp %>% filter(Inv.Amount<0), .(code.client), nrow)


#start creating final df to merge to all 6 via mlsto
inv.eng<- temp %>% select(code.jobnum) %>% unique
inv.eng<- merge(inv.eng, temp %>% select(code.jobnum, mlsto) %>% unique(), by='code.jobnum', all.x=TRUE, all.y=FALSE)
inv.eng<- merge(inv.eng, temp %>% select(code.jobnum, inv.mlsto) %>% unique(), by='code.jobnum', all.x=TRUE, all.y=FALSE)
inv.eng<- merge(inv.eng, temp %>% select(code.jobnum, code.client) %>% unique(), by='code.jobnum', all.x=TRUE, all.y=FALSE)
inv.eng<- merge(inv.eng, v1, by='mlsto', all.x=TRUE)
inv.eng<- merge(inv.eng, v2, by= 'mlsto', all.x=TRUE)
inv.eng<- merge(inv.eng, v3, by= 'mlsto', all.x=TRUE)
inv.eng<- merge(inv.eng, v4, by= 'mlsto', all.x=TRUE)

#turn all NA into zeroes

inv.eng<- merge(inv.eng, v5, by= 'code.client', all.x=TRUE)
inv.eng<- merge(inv.eng, v6, by= 'code.client', all.x=TRUE)
inv.eng<- merge(inv.eng, v7, by= 'code.client', all.x=TRUE)
colnames(inv.eng)[names(inv.eng) %in% 'V1.x']<-'num.inv'
colnames(inv.eng)[names(inv.eng) %in% 'V1.y']<-'num.neginv'
colnames(inv.eng)[names(inv.eng) %in% 'V1']<-'client.neginv'
inv.eng[is.na(inv.eng$num.neginv),]$num.neginv <- 0
inv.eng[is.na(inv.eng$client.neginv),]$client.neginv <- 0

#final variables!!
#mean size of invoice over total invoiced per project - same as num.inv!! except inverse
#mean number of invoices per job for client
v8<- ddply(inv.eng %>% filter(!is.na(num.inv)), .(code.client), summarise, client.numinv = mean(num.inv) %>% round(2))
#mean total invoiced per client
v9<- ddply(inv.eng, .(code.client), summarise, client.totinv = mean(inv.mlsto) %>% round(2))


inv.eng<- merge(inv.eng, v8, by= 'code.client', all.x=TRUE)
inv.eng<- merge(inv.eng, v9, by= 'code.client', all.x=TRUE)

#save
write.csv(inv.eng, 'invoices_eng.csv')
#now can merge inv.eng with all6 :)! see end of de-identify file


#can we get rid of some of the NA values in inv.eng?? 
# inv.eng$Inv.freq has 1131 NA rows - fine - either 1 invoice or 2 invoices in different milestones etc.
# inv.eng$client.invfreq has 269 NA rows - fine
# client.numinv has 21 NA rows - all pre 2007 - fine
# client.meaninv has 21 NAs - all pre 2007 - fine
