setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
# setwd("~/OneDrive/shared files/Bligh Tanner/masters/data")
library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library('magrittr',lib='C:/Progra~1/R/R-3.1.3/library')
library('reshape2',lib='C:/Progra~1/R/R-3.1.3/library')

all6<- read.csv('all6.csv')[,-1]
all6$Start.Date<- as.Date(all6$Start.Date)
#check extremes
all6 %>% arrange(-return.pdol) %>% select(return.pdol, Tot.Invoiced, mlsto, hours) %>% head(20)
#check specific job numbers
all6 %>% select(return.pdol, Tot.Invoiced, mlsto, hours, cost.mlsto, dis.sc.mlsto) %>% filter(mlsto == '2013.430.4')

#all7 has deleted all milestone duplicates and deleted outliers
all7<- read.csv('all7.csv')[,-1]
all7$Start.Date<- as.Date(all7$Start.Date)
all7<- all7 %>% filter(!(inv.vs.cost<0.2), !(inv.vs.cost>4))


#profitability
#discipline vs profit
#subset out invoiced=0
dup<- all6[,names(all6) %in% c('mlsto', 'Discipline')]
subd1<-all6[!all6$Tot.Invoiced==0 & !is.na(all6$Discipline) & !duplicated(dup),] 
#make outlier points match variable colour
update_geom_defaults("point", list(colour = NULL))
bal.discplot<- ggplot(subd1, aes(x=Discipline, y= balance.mlsto,
                                     colour= Discipline)) + 
        geom_boxplot(alpha=0) +geom_jitter(alpha= I(1/2))+
        scale_y_continuous(breaks = seq(round(min(subd1$balance.mlsto),-5), round(max(subd1$balance.mlsto),-5), by = 20000))

prof.discplot<- ggplot(subd1, aes(x=Discipline, y= profit.mlsto,
                                 colour= Discipline)) + 
        geom_boxplot(alpha=0) +geom_jitter(alpha= I(1/2))+
        scale_y_continuous(breaks = seq(round(min(subd1$balance.mlsto),-5), round(max(subd1$balance.mlsto),-5), by = 20000))

#look at distance variable vs balance
subd2<-all6[!all6$Tot.Invoiced==0 & !is.na(all6$dist) & !duplicated(dup),] 
bal.distplot<- ggplot(subd2, aes(x=dist, y= balance.mlsto,
                                 colour= Discipline)) + 
        geom_point()+
        scale_y_continuous(breaks = seq(round(min(subd2$balance.mlsto),-5), round(max(subd2$balance.mlsto),-5), by = 20000))

#look at billing type
subd3<-all6[!all6$Tot.Invoiced==0 & !is.na(all6$Billing.Type) & !duplicated(dup),] 
bal.billplot<- ggplot(subd3, aes(x=Billing.Type, y= balance.mlsto)) + 
        geom_boxplot(alpha=0) +geom_jitter(alpha= I(1/2), aes(colour=Discipline))+
        scale_y_continuous(breaks = seq(round(min(subd3$balance.mlsto),-5), round(max(subd3$balance.mlsto),-5), by = 20000))

bal.billplot2<- ggplot(subd3, aes(x=Billing.Type, y= balance.mlsto, colour=Discipline)) + 
        geom_boxplot(alpha=0) +
        scale_y_continuous(breaks = seq(round(min(subd3$balance.mlsto),-5), round(max(subd3$balance.mlsto),-5), by = 20000))

#look at business type???
subd4<-all6[!all6$Tot.Invoiced==0 & !is.na(all6$Business) & !duplicated(dup),] 
bal.bizplot<- ggplot(subd4, aes(x=Business, y= balance.mlsto, colour=Discipline)) + 
        geom_boxplot(alpha=0) +
        scale_y_continuous(breaks = seq(round(min(subd4$balance.mlsto),-5), round(max(subd4$balance.mlsto),-5), by = 20000))+
        theme(legend.position="right", axis.text.x=element_text(angle=45,hjust=1),
              text=element_text(size=12))

#look at business size by num offices???
subd5<-all6[!all6$Tot.Invoiced==0 & !is.na(all6$Biz.size) & !duplicated(dup),] 
bal.bizplot2<- ggplot(subd5, aes(x=Biz.size, y= balance.mlsto, colour=Discipline)) + 
        geom_boxplot(alpha=0) +
        scale_y_continuous(breaks = seq(round(min(subd5$balance.mlsto),-5), round(max(subd5$balance.mlsto),-5), by = 20000))+
        theme(legend.position="right", axis.text.x=element_text(angle=45,hjust=1),
              text=element_text(size=12))

#look at business sector???
subd6<-all6[!all6$Tot.Invoiced==0 & !is.na(all6$Biz.type) & !duplicated(dup),] 
bal.bizplot3<- ggplot(subd6, aes(x=Biz.type, y= balance.mlsto, colour=Discipline)) + 
        geom_boxplot(alpha=0) +
        scale_y_continuous(breaks = seq(round(min(subd6$balance.mlsto),-5), round(max(subd6$balance.mlsto),-5), by = 20000))+
        theme(legend.position="right", axis.text.x=element_text(angle=45,hjust=1),
              text=element_text(size=12))

#look at business num employees???
subd7<-all6[!all6$Tot.Invoiced==0 & !is.na(all6$no.employees) & !duplicated(dup),]
subd7$emp.bin<- cut(subd7$no.employees, 20)
bal.bizplot4<- ggplot(subd7, aes(x=emp.bin, y= balance.mlsto)) + 
        geom_boxplot() +
        scale_y_continuous(breaks = seq(round(min(subd7$balance.mlsto),-5), round(max(subd7$balance.mlsto),-5), by = 20000))+
        theme(legend.position="right", axis.text.x=element_text(angle=45,hjust=1),
              text=element_text(size=12))
# zoom in on 1-10000 employees
subd8 <- subd7[subd7$no.employees <= 10000,]
subd8$emp.bin<- cut(subd8$no.employees, 20)
bal.bizplot5<- ggplot(subd8, aes(x=emp.bin, y= balance.mlsto)) + 
        geom_boxplot() +
        scale_y_continuous(breaks = seq(round(min(subd8$balance.mlsto),-5), round(max(subd8$balance.mlsto),-5), by = 20000))+
        theme(legend.position="right", axis.text.x=element_text(angle=45,hjust=1),
              text=element_text(size=12))
# zoom in on 1-500 employees
subd9 <- subd8[subd8$no.employees <= 500,]
subd9$emp.bin<- cut(subd9$no.employees, 20)
bal.bizplot6<- ggplot(subd9, aes(x=emp.bin, y= balance.mlsto)) + 
        geom_boxplot() +
        scale_y_continuous(breaks = seq(round(min(subd9$balance.mlsto),-5), round(max(subd9$balance.mlsto),-5), by = 20000))+
        theme(legend.position="right", axis.text.x=element_text(angle=45,hjust=1),
              text=element_text(size=12))

#position plot
dup1<- all6[,names(all6) %in% c('mlsto')]
sub10<-all6[!all6$Tot.Invoiced==0 & !duplicated(dup1),]
sub10<- melt(sub10 %>% select(code.jobnum, pc.contracttech, pc.director, pc.gradpro, pc.midpro, pc.midtech, pc.seniorpro,
                             pc.seniortech, pc.unknown),value.name='percent.hrs') %>% filter(percent.hrs>.5)
sub10 <- merge(sub10, all6 %>% select(Start.Date, balance.mlsto, Discipline, code.jobnum, return.pdol), by= 'code.jobnum', all.x=TRUE, all.y=FALSE) %>%
        arrange(Start.Date)
sub10$Start.Date<- as.Date(sub10$Start.Date)
sub10$return.cut<- cut_number(sub10$return.pdol, 10, labels= c(1:10)) %>% as.integer()

pos.plot2<- ggplot(sub10, aes(x=return.cut, y= percent.hrs,
                             colour= variable)) + 
        geom_point (shape=1)+ 
        geom_smooth(method=loess, se=TRUE)+
        theme(legend.position="right", axis.text.x=element_text(angle=45,hjust=1),
              text=element_text(size=12))

pos.plot3<- ggplot(sub10, aes(x=return.cut, y= percent.hrs,
                              colour= variable)) + 
        stat_summary(fun.y= mean, geom='line') + 
        stat_summary(fun.y= mean, geom='point') +
        #stat_summary(fun.data= mean_sdl, geom='crossbar', mult =1) +
        theme(legend.position="right", axis.text.x=element_text(angle=45,hjust=1),
              text=element_text(size=12))+
        scale_x_continuous(breaks = seq(1, 10, by = 1), limits=c(1,10)) +
        scale_y_continuous(breaks = seq(0, 100, by = 10), limits=c(0,100)) 

#Visualisatin conference plot

s= ggplot(sub10 %>% filter(variable == 'pc.seniorpro', Discipline == 'Structural'), aes(x=percent.hrs, y= return.cut, colour= 'blue')) + 
  geom_smooth(method=loess, se=TRUE, aes(fill='blue'), alpha=.2) + 
  #                                 stat_summary(fun.x= mean, geom='point', shape = 2) +
  #stat_summary(fun.data= mean_sdl, geom='crossbar', mult =1) +
  theme(legend.position="none", axis.text.x=element_text(angle=45,hjust=1),
        text=element_text(size=12))+
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits=c(0,100)) +
  scale_y_continuous(breaks = seq(1, 10, by = 1), limits=c(1,10)) +
  # facet_wrap(~ variable) + , labeller =  label_value(variable, c('a','b','c','d','e','f','g','h')) +
  geom_point(aes(colour = 'blue'), alpha = 0.2) +
  theme(strip.text = element_text(colour = 'white'), strip.background = element_rect(fill = 'black')) +
  labs(x = '% Contribution by hours of Senior Professional', 
       y = 'Return per Dollar Category', title= 
         'Percent Contribution by a Senior Professional vs Return per Dollar category')


#rising stars mplot rCharts
dup<- all6[,names(all6) %in% c('mlsto', 'Discipline')]
sub11<-all6[!all6$Tot.Invoiced==0 & !duplicated(dup) & all6$client.count>1,]

# need slope column
mods<- dlply(sub11, .(code.client), lm, formula= return.pdol ~ Start.Date)
coefs <- ldply(mods, coef)
colnames(coefs)[names(coefs) %in% 'Start.Date']<-'Risestar.coef'
#coefs %>% arrange(-Start.Date) %>% filter( !is.na(Start.Date)) %>% tail()
sub11a<- merge(sub11, coefs %>% select(code.client, Risestar.coef), by='code.client', all.x=TRUE)
sub11a<- sub11a %>% filter(!is.na(Risestar.coef)) %>% arrange(-Risestar.coef)
#reorder levels of code.client descending according to Risestar.coef
sub11a <- within(sub11a, 
                         code.client <- factor(code.client, 
                                               levels= sub11a[order(-sub11a$Risestar.coef),]$code.client %>% unique()))

#variable to be 'Business'. x axis to be 'Start.Date' y axis to be 'return.pdol'
sub11b<- sub11a %>% filter(Business == 'architect')
star.plot<- ggplot(sub11b, aes(x= Start.Date, y= return.pdol, colour=code.client))+
        geom_point()+
        geom_smooth(method=lm, se=TRUE)
py<-plotly()
py$ggplotly()


#look at JD.Second vs inv.vs.cost
#make df for levevvl order - medians
df.JDS<- ddply(all7, .(JD.Second), summarise, median= median(inv.vs.cost))

all7 <- within(all7, 
                 JD.Second <- factor(JD.Second, 
                                       levels= df.JDS[order(-df.JDS$median),]$JD.Second ))

bal.bizplot3<- ggplot(all7, aes(x=JD.Second, y= inv.vs.cost)) + 
        geom_boxplot(alpha=0) +
        theme(legend.position="none", axis.text.x=element_text(angle=45,hjust=1),
              text=element_text(size=12))





