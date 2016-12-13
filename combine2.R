setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library('magrittr',lib='C:/Progra~1/R/R-3.1.3/library')
library('reshape2',lib='C:/Progra~1/R/R-3.1.3/library')
library('rCharts',lib='C:/Progra~1/R/R-3.2.0/library')

invoices<- read.csv('invoices_clean.csv')[,-1]
hours<- read.csv('hours_clean.csv')[,-1]

#### CREATING SHINY PRODUCT - MONTHLY REVIEW

### STEP 0
# subset invoices df and hours df to 2009 onwards 
invoices$Invoice.Date <- as.Date(invoices$Invoice.Date)
invoices<- arrange(invoices, Invoice.Date)
invoices<- invoices %>% filter(Invoice.Date>= as.Date('2009-01-01'))

hours$Work.Date <- as.Date(hours$Work.Date, "%d/%m/%Y")
hours<- hours %>% filter(Work.Date>= as.Date('2009-01-01'))

### STEP 0.5
# assign tag name to all rows between dates
# use last date in month
invoices$MonthYear<- format(invoices$Invoice.Date,"%Y-%b")
tag<- ddply(invoices, .(MonthYear), tail, 1)
tag<-tag %>% arrange(Invoice.Date)

df<- data.frame('start' = c(as.Date('2008-12-31'), tag$Invoice.Date[-nrow(tag)]),'end'= tag[,6], 'tag' = tag[,10])


## try ifelse function --> similar to grepping 
#assign tag to all hour rows based on whether they are between two dates in the df data.frame (see above)
#these dates are determined by the last date of invoices in each month

btwn.dates<- function(date.df = df, col.to.tag = hours$Work.Date){
        a= c(rep(NA,length(col.to.tag)))
        for( i in 1:nrow(date.df)){
                a = ifelse(col.to.tag >= date.df[i,1] & col.to.tag < date.df[i,2], as.character(date.df$tag[i]), a) 
        } 
        a<- as.factor(a)
        a
}

hours$tag<- btwn.dates(date.df = df, col.to.tag = hours$Work.Date)
#if hours$tag = NA, delete the row. This means that the hour was performed after hte last invoice in 2014 (dec 10 2014)
hours<- hours %>% filter(!is.na(tag))

#re-tag invoices data set. want all invoices more than 4 days BEFORE the last invoices, to be classified as a  late invoice for the PREVIOUS month
#add another column to df for 'invoice end' which will be a copy of 'start' date of the next month

df$invoice.end<- c(tag$Invoice.Date[-1], as.Date('2014-12-31'))

#now create tags for invoices based on 5 days before df$end and 5 days before df$invoice.end

btwn.dates<- function(date.df = df, col.to.tag = invoices$Invoice.Date){
        a= c(rep(NA,length(col.to.tag)))
        for( i in 1:nrow(date.df)){
                a = ifelse(col.to.tag >= (date.df[i,2]-5) & col.to.tag < (date.df[i,4]-5), as.character(date.df$tag[i]), a) 
        } 
        a<- as.factor(a)
        a
}

invoices$tag<- btwn.dates( date.df = df, col.to.tag = invoices$Invoice.Date)
invoices<- invoices[,!names(invoices) %in% 'MonthYear']


### STEP 1 
# ddply both data sets separately for each month tag and Job.Number
invoices.dd<- ddply(invoices, .(Job.Number, tag), summarise, sum(Inv.Amount))
#add a date column to enable sorting
invoices.dd$Date<- paste(invoices.dd$tag, '05',sep='-')
invoices.dd$Date<- as.Date(invoices.dd$Date, '%Y-%b-%d')
colnames(invoices.dd)[names(invoices.dd) %in% '..1']<-'Invoiced.Amount'
invoices.dd<- invoices.dd %>% arrange(Date)

#now hours dataset
hours.dd<- ddply(hours, .(Job.num, tag), summarise, hours=sum(Hours), charge=sum(Charge), cost= sum(Cost), disburse=sum(Disburse))

hours.dd$Date<- paste(hours.dd$tag, '05',sep='-')
hours.dd$Date<- as.Date(hours.dd$Date, '%Y-%b-%d')
hours.dd<- hours.dd %>% arrange(Date)


### STEP 2 
# merge invoices, hours, all based on job number AND month (invoices and hours only)
monthly<- merge(invoices.dd, hours.dd %>% select(-Date), by.x= c('Job.Number', 'tag'), by.y= c('Job.num', 'tag'))
head(monthly)
monthly<- transform(monthly, profit.mon = Invoiced.Amount - charge- disburse, 
                    bal.mon = Invoiced.Amount - cost- disburse,
                    return.mon= (Invoiced.Amount - cost- disburse)/(cost+disburse))

#merge with all
#need to codify Job.Number to match all6
code.job<- read.csv('code_job.csv')[,-1]
monthly<- merge(monthly, code.job, by.x= 'Job.Number', by.y= 'job1', all.x=TRUE)
colnames(monthly)[names(monthly) %in% 'code2']<-'code.jobnum'
monthly<- monthly[,!names(monthly) %in% 'Job.Number']

#merge with all6
all6<- read.csv('all6.csv')[,-1]
all6$Start.Date<- as.Date(all6$Start.Date)
month.a<- merge( monthly, all6 %>% select(Tot.Invoiced, timespan, Num.disc, code.client, code.contact, code.jobnum, code.director,
                                          code.ProjEng, ProjEng.Pos, Billing.Type, Discipline, Job.Type.Primary, client.count, Business, no.employees,
                                          Biz.size, Biz.type, contact.count, JD.Primary, JD.Second, Type, mlsto), 
                 by.x='code.jobnum', by.y='code.jobnum', all.x=TRUE, all.y=FALSE)

month.a<- month.a %>% arrange(-return.mon)
month.a[month.a$return.mon== Inf,]$return.mon <- 800

#figure out what kind of ggplot you want
#create bins for return.mon
month.a$return.cut <- cut_number(month.a$return.mon,15)
#need to cut Total Timespan, Client count, Client no. employees
#into bins
month.a$timespan.cut <- cut_number(month.a$timespan,12)
month.a$clientcount.cut <- cut_number(month.a$client.count,8)
month.a$clientemps.cut <- cut_number(month.a$no.employees,12)

month.a$return.cut <- cut_number(month.a$return.mon,15)


write.csv(month.a, 'monthly.csv')
month.a<- read.csv('monthly.csv')[,-1]
month.a$Date<- as.Date(month.a$Date)

levels(month.a$return.cut) =c("< -$0.51", "-$0.51 to -$0.24", "-$0.24 to -$0.02","-$0.02 to $0.18",
                              "$0.18 to $0.33", "$0.33 to $0.45", "$0.45 to $0.50", "$0.50 to $0.61",
                              "$0.61 to $0.75", "$0.75 to $0.92", "$0.92 to $1.20", "$1.20 to $1.68", 
                              "$1.68 to $2.47", "$2.47 to $5.24", "> $5.24")


#first subset to a single month
month.sub<- month.a %>% filter( tag== '2012-Mar', Discipline =='Civil')

s<- ggplot(month.sub, aes_string(x='return.cut', y= 'bal.mon', fill= 'code.client')) +
  geom_bar(stat='identity', position='identity')+
  theme(legend.position="right", axis.text.x=element_text(angle=45,hjust=1),
        text=element_text(size=12)) +
  labs(x = 'Return per Dollar') +
  scale_x_discrete(limits = c("< -$0.51", "-$0.51 to -$0.24", "-$0.24 to -$0.02","-$0.02 to $0.18",
                              "$0.18 to $0.33", "$0.33 to $0.45", "$0.45 to $0.50", "$0.50 to $0.61",
                              "$0.61 to $0.75", "$0.75 to $0.92", "$0.92 to $1.20", "$1.20 to $1.68", 
                              "$1.68 to $2.47", "$2.47 to $5.24", "> $5.24"))

py<-plotly()
py$ggplotly()

#troubleshooting monthly review plot! look at Civil, March 2012:



#layer Rcharts over!!!!


n1<- nPlot(Invoiced.Amount ~ return.cut, group = "Discipline", data = month.sub, type = "multiBarChart")

n1$xAxis( rotateLabels = -45)

n1

#create rows of '0' for every group in every month.. :(




