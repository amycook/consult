setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library('magrittr',lib='C:/Progra~1/R/R-3.2.0/library')

###DELETE VARIABLES###

invoices<-read.csv('invoices.csv')
head(invoices)
str(invoices)
summary(invoices)
invoices<-invoices[,-c(10,11)]
invoices<-invoices[,!(names(invoices) %in% 'Created.Date')]
invoices<-invoices[,!(names(invoices) %in% 'Status')]

###RENAME COLUMNS###
colnames(invoices)[names(invoices) %in% 'Job']<-'Job.Name'
colnames(invoices)[names(invoices) %in% 'Amount.Paid..AUD.']<-'Amnt.Paid'
colnames(invoices)[names(invoices) %in% 'Inv.Amount..E...AUD.']<-'Inv.Amount'
colnames(invoices)[names(invoices) %in% 'qbin_CreatedBy']<-'Created.by'

###RE-CLASS COLUMNS###
invoices$Invoice.Date<-as.Date(invoices$Invoice.Date, '%d/%m/%Y')

###DELETE CASES CREATED ON JAN 16 2014 - after original data was harvested
invoices<-invoices[!(invoices$Invoice.Date > as.Date("2015-01-10")),]

###ENGINEERED VARIABLES###

#delete job number from job.name
invoices$Job.Name<-as.character(invoices$Job.Name)
strsplit(invoices$Job.Name[1],split='[:]')[[1]][2]
invoices$Job.Name<-sapply(invoices$Job.Name,FUN=function(x){strsplit(x,split='[:]')[[1]][2]})
invoices$Job.Name<-sub(' ','',invoices$Job.Name)
#year of job
invoices$Job.Number<-as.character(invoices$Job.Number)
strsplit(invoices$Job.Number[1],split='[.]')[[1]][1]
invoices$Year<-sapply(invoices$Job.Number,FUN=function(x){strsplit(x,split='[.]')[[1]][1]})
invoices$Year<-as.integer(invoices$Year)
#num invoices per job- haven't accounted for negative invoices neutralising an equal number of positive invoices.
num.invoices<-ddply(invoices[invoices$Inv.Amount>0,],.(Job.Number), nrow)
colnames(num.invoices)[names(num.invoices) %in% 'V1']<-'Num.invoices'

###QUESTIONS/EXPLORATION###

#How do Job.Numbers compare to 'all' data frame?
jobnums<-unique(invoices$Job.Number)
summary(jobnums)
length(jobnums)
all<-read.csv('temp_all.csv')
jobnums.all<-unique(all$Job.Number)
length(jobnums.all)
#WIP ONLY?
WIP<-invoices[invoices$Invoice.Number %in% 'WIP ONLY',]
nrow(WIP)
summary(WIP)
wip.tf<-WIP$Job.Number
#create True False column in final Inv.Sum- refer summary data frame at end.

#BAD DEBT meaning? associated with negative invoices.
Neg<-invoices[invoices$Inv.Amount<0,]
Neg[Neg$Inv.Amount <0,]
#create Job variable for number of negative invoices.
Neg.count<-(ddply(Neg,.(Job.Number), nrow))
nrow(Neg.count)
colnames(Neg.count)[names(Neg.count) %in% 'V1']<-'Num.NegInv'
#sum total $ negative invoices
Tot.NegInv<-(ddply(Neg,.(Job.Number),summarise, Tot.NegInv = sum(Inv.Amount)))

#blank in status? 4 of - can't see any meaning
invoices[invoices$Status == "",]
#Unassigned in status?
invoices[invoices$Status == "Unassigned",]
#delete status column

#Account.Status blanks
invoices[invoices$Account.Status == "",]
invoices[invoices$Account.Status == "Invoice Outstanding",]
#two blank cases show invoices not paid. relabel to 'invoice outstanding'.
invoices[invoices$Account.Status == "",]$Account.Status<- 'Invoice Outstanding'
#Account.Status Partially Paid
invoices[invoices$Account.Status == "Invoice Partially Paid",]$Account.Status<- 'Invoice Outstanding'
levels(droplevels(invoices$Account.Status))

#Amount.Paid is negative
invoices[invoices$Amnt.Paid != invoices$Inv.Amount & invoices$Amnt.Paid<0,]
#in all cases, equals the amount of a negative invoice - fine

#Amount.Paid is zero
Unpaid<-invoices[invoices$Amnt.Paid==0 & invoices$Inv.Amount>0,]
head(Unpaid,20)
summary(Unpaid$Account.Status)
#should all these cases be labelled 'Invoice Outstanding'. currently 61/126 labelled as 'paid'
ggplot(Unpaid,aes(x=Year))+geom_bar()
ggplot(Unpaid[Unpaid$Account.Status=='Invoice Paid',],aes(x=Invoice.Date))+geom_bar()+
        theme(axis.text.x=element_text(angle=45,hjust=1))
ggplot(Unpaid[Unpaid$Account.Status=='Invoice Outstanding',],aes(x=Invoice.Date))+geom_bar()+
        theme(axis.text.x=element_text(angle=45,hjust=1))
#can probably assume all invoices 'Invoice Outstanding' from Oct,Nov,Dec 2014 will be paid even if they aren't paid yet
#change to paid? and change Amt paid equal to invoice amount? 44 cases.
Change<-Unpaid[Unpaid$Account.Status=='Invoice Outstanding'& Unpaid$Invoice.Date > as.Date('2014-10-01'),]
head(Change)
RN<-row.names(Change)
invoices[row.names(invoices) %in% RN,]$Account.Status<-'Invoice Paid'




######## CHECKING ALL Unpaid[Unpaid$Account.Status=='Invoice Paid',] TO MAKE SURE THEY CANCEL A NEGATIVE #######







###QUICK PLOTS###

bar.plots<-function(df){
        nm=colnames(df)
        for(i in seq_along(nm)){
                plots<-ggplot(df,aes_string(x=nm[i]))+
                        geom_bar()+
                        theme(axis.text.x=element_text(angle=45,hjust=1))
                ggsave(plots,filename=paste("barplot",nm[i],".png",sep=""),
                       path="C:/Users/n9232371/Documents/Consultbusiness/barplots/invoices")
                
                cat(i," ")
        }
}
# bar.plots(invoices)
# bar.plots(WIP)
# bar.plots(Neg)

###CREATE SUMMARY DATA FRAME- ONE ROW PER JOB.NUMBER###

Inv.sum<-data.frame('Job.Number'=jobnums)
#column for T/F whether WIP row appears in job.number
Inv.sum$WIP<-apply(Inv.sum[1],1,function(x) x %in% wip.tf)
Inv.sum<-merge(Inv.sum, Neg.count, by='Job.Number', all=TRUE)
Inv.sum<- merge(Inv.sum, Tot.NegInv, by='Job.Number', all=TRUE)
Inv.sum<-merge(Inv.sum, num.invoices, by='Job.Number', all=TRUE)
#subtract Num.NegInv from Num.invoices to cater for neg invoice neutralising a positive inv.
Inv.sum[is.na(Inv.sum)]<-0
temp<-Inv.sum[,c(3,5)]
temp<-apply(temp,1, function(x) x[2]-x[1])
Inv.sum$Num.invoices<-temp

#sum invoices for each job
temp1<- ddply(invoices, .(Job.Number), summarise, sum(Inv.Amount))
Inv.sum<- merge(Inv.sum, temp1, by='Job.Number')
colnames(Inv.sum)[names(Inv.sum) %in% '..1']<-'Tot.Invoiced'

#check tot.invoiced against all data set
all5a<- read.csv('all5a.csv')
temp.all<-  all5a[,names(all5a) %in% c('Job.Number','Tot.Invoiced')]
Inv.sum<- merge(Inv.sum, temp.all, by.x= 'Job.Number', by.y='Job.Number', all.x=TRUE, all.y=FALSE)
Inv.sum<- transform(Inv.sum, check= Tot.Invoiced.x-Tot.Invoiced.y)
#have a look through checks
Inv.sum %>% arrange(check) %>% head()
#in all cases, Inv.sum is correct
colnames(Inv.sum)[names(Inv.sum) %in% 'Tot.Invoiced.x']<-'Tot.Invoiced'
colnames(Inv.sum)[names(Inv.sum) %in% 'Tot.Invoiced.y']<-'Tot.Invoiced.check'

#### investigate duplicate invoices
#order by Invoice.Date
invoices$Invoice.Date<- as.Date(invoices$Invoice.Date)
invoices<- invoices %>% arrange(Invoice.Date, Job.Number, Inv.Amount)
#check if any identical invoice amounts on the same Invoice.Date
check<- ddply(invoices, .(Invoice.Date, Inv.Amount, Job.Number), nrow)
check %>% filter(V1>2)
#create 3 new columns that copy the row above for Job.Number, Invoice.Date, Inv.Amount
invoices$JN.2 <- c(0,as.character(invoices$Job.Number[-nrow(invoices)]))
invoices$ID.2 <- c("2000-01-01",as.character(invoices$Invoice.Date[-nrow(invoices)]))
invoices$ID.2 <- as.Date(invoices$ID.2)
invoices$IA.2 <- c(0,invoices$Inv.Amount[-nrow(invoices)])
#delete any rows that have the same 3 columns listed above as the row above
invoices.a<- invoices[!(invoices$Invoice.Date == invoices$ID.2 & 
                                invoices$Job.Number==invoices$JN.2 &
                                invoices$Inv.Amount == invoices$IA.2),]
#remove the new rows
invoices.a<- invoices.a[,!names(invoices.a) %in% c('JN.2','ID.2','IA.2')]

write.csv(Inv.sum, 'inv_sum.csv')
write.csv(invoices.a, 'invoices_clean.csv')







