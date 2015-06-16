setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
# setwd("~/OneDrive/shared files/Bligh Tanner/masters/data")
library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library('magrittr',lib='C:/Progra~1/R/R-3.1.3/library')
library('reshape2',lib='C:/Progra~1/R/R-3.1.3/library')
library("rpart",lib = 'C:/Program Files/R/R-3.2.0/library')

all6a<- read.csv('all6a.csv')[,-1]
all6a$Start.Date<- as.Date(all6a$Start.Date)

#delete milestone rows! sort by highest hours - choose this discipline.!
#sort by hours first
all6a<- all6a %>% arrange(mlsto, -hours)
dup<- all6a[,names(all6a) %in% c('mlsto')]
all7<-all6a[!(all6a$Tot.Invoiced==0 | duplicated(dup)),]

# write.csv(all7, 'all7.csv')
all7<- read.csv('all7.csv')[,-1]
all7$Start.Date<- as.Date(all7$Start.Date)

#is return.pdol and inv.vs.cost normally distributed??????
#use qqnorm and qqline
library('car')
library('e1071', lib = 'C:/Progra~1/R/R-3.2.0/library')

qqnorm(all7$return.pdol)
qqline(all7$return.pdol)
qqPlot(all7$return.pdol)

# up tail at end. try log of variable
qqnorm(log(abs(all7$return.pdol+.001)))
qqline(log(abs(all7$return.pdol+.001)))
#this looks s shaped

qqnorm(log(all7$return.pdol+.8))
qqline(log(all7$return.pdol+.8))

#find sd and mean of return.pdol
sd(all7$return.pdol)
mean(all7$return.pdol)
skewness(all7$return.pdol)
skewness(log(all7$return.pdol+2.15))
#skewness is 27.2 and 2.12 for logged data!!!

#have a look at the shape of return.pdol as bar graph
ggplot(all7, aes(x=return.pdol)) + geom_bar(stat='bin')
ggplot(all7, aes(x=log(all7$return.pdol+.8))) + geom_bar(stat='bin')
#log looks much more 'normal'

# try deleting very high return.pdol values greater than 3
test<- all7 %>% filter(return.pdol <=3 & return.pdol> -2)
qqPlot(test$return.pdol)
qqPlot(log(test$return.pdol+.97))
#have a look at the shape of return.pdol as bar graph
ggplot(test, aes(x=return.pdol)) + geom_histogram(stat='bin', binwidth=.1)
ggplot(test, aes(x=log(test$return.pdol+.97))) + geom_histogram(stat='bin', binwidth=.1)
#definietly don't want to use log - worse.
sd(test$return.pdol)
mean(test$return.pdol)
median(test$return.pdol)
#mode
names(sort(-table(test$return.pdol)))[1]
skewness(test$return.pdol)
skewness(log(test$return.pdol+.97))
# skewness only 0.92!! for unlogged
#skewness is -2.6 for logged data
kurtosis(test$return.pdol)
# kurtosis equals 2.85 - very peakedy - ie not flat. normal distribution is 0.



### delete outliers! and variables that aren't by milestone
all7a<- all7 %>% filter(return.pdol <=3 & return.pdol> -2)
all7a<- all7 %>% select(-Paperless,-Innovation, -JobInvCount,-job.first.inv.email.Sent, 
                        -Total.Costs..AUD., -Stage, -Folders, -Total.Fee, -Disburse, -Subcon.fee,
                        -Job.Size, -Tot.Invoiced, -charge, -cost, -Dis.subcon, -hours,-profit,-balance,
                        -Role)


write.csv(all7a, 'all7a.csv')
# all7 %>% arrange(-inv.vs.cost) %>% select(inv.vs.cost) %>% head(50)
# ggplot(all7, aes(x=inv.vs.cost, y=inv.mlsto)) + geom_point()
# ggplot(all7, aes(x=inv.vs.cost, y=inv.vs.cost)) + geom_boxplot()
# #clculate range before a point is deemed an outlier:
# # mean - 2*IQR and mean+ 2*IQR
# upper<- mean(all7$inv.vs.cost)+ 2.5*IQR(all7$inv.vs.cost)
# lower<- mean(all7$inv.vs.cost)- 2.5*IQR(all7$inv.vs.cost)
# #how many below this range? 82
# all7 %>% filter(inv.vs.cost< lower) %>% select(inv.vs.cost, inv.mlsto, hrs.mlsto, mlsto) %>% arrange(-inv.vs.cost)
# #how many above this range? 96
# all7 %>% filter(inv.vs.cost> upper) %>% select(inv.vs.cost, inv.mlsto, hrs.mlsto, mlsto) %>% arrange(-inv.vs.cost)


#delete anything more than 4 for inv.vs.cost
#delete anything less than .2 for inv.vs.cost
# all7<- all7 %>% filter(!(inv.vs.cost<0.2), !(inv.vs.cost>4)) %>% nrow


#before we try anova, should we use it???
# in all categories, need normal distribution, same spread, significant power
# case study between client business types: architect vs developers
summary(all7a$Business)
#try manufacturer vs developer
test2<- test %>% filter(Business == 'manufacturer/supplier' | Business == 'developer/real estate')
test2$Business<- droplevels(test2$Business)
qqPlot(all7a %>% filter(Business == 'manufacturer/supplier') %>% select(return.pdol))
qqPlot(all7a %>% filter(Business == 'developer/real estate') %>% select(return.pdol))
#ok.... check spread
numSummary(test2 %>% select(return.pdol), groups = test2$Business, statistics = 
             c('mean', 'sd', 'IQR', 'quantiles',
               'skewness', 'kurtosis'), quantiles = c(0,.25,.5,.75,1), type='2' )
#similar spreads

#now check residual plots
test3<- aov(return.pdol~Business, data= test2)
plot(test3)
#check power to detect difference between pop means
pwr.f2.test(u=2, v=50, f2=0.15, sig.level=0.05)
ggplot(test2, aes(x=Business, y= return.pdol)) + geom_boxplot()

#test4 lm
test4<- lm(return.pdol~ Business, data= test2)
#plot residuals
x<- resid(test4)
plot(test2$return.pdol, x)

###### LINEAR REGRESSIONS AND ANOVA
#first linear regression

lin.all7<- lm(inv.vs.cost~  Billing.Type + Discipline + JD.Primary + Job.Type.Secondary + Type + client.count + Business+
                       no.employees + Biz.size + Biz.type + contact.count + 
                      dist + charge.ph + charge.pc + no.users + pc.contracttech +
                      pc.director + pc.midtech + pc.midpro+ pc.gradpro+ pc.seniortech+ pc.seniorpro+ pc.unknown+ pc.pro+ pc.tech+
                      Num.disc +mean.peeps + hours.perday + JD.Second +
                      code.client + code.director + code.ProjEng + ProjEng.Pos+ code.Eng2+ Eng2.Pos                      
                      ,data= all7a)

#the above function doesn't work because almost all of the observations are deleted due to missingness

#try anova

aov(inv.vs.cost~  Billing.Type #+ Discipline + JD.Primary + Job.Type.Secondary + JD.Second + Type + client.count + Business+
#                       no.employees + Biz.size + Biz.type + contact.count + 
#                       dist + charge.ph + charge.pc + no.users + pc.contracttech +
#                       pc.director + pc.midtech + pc.midpro+ pc.gradpro+ pc.seniortech+ pc.seniorpro+ pc.unknown+ pc.pro+ pc.tech+
#                       Num.disc +mean.peeps + hours.perday + JD.Second +
#                       code.client + code.director + code.ProjEng + ProjEng.Pos+ code.Eng2+ Eng2.Pos                      
              ,data= all7a)

test<- aov(inv.vs.cost~ Billing.Type + Discipline + JD.Primary + Job.Type.Secondary + JD.Second +
                   Type, data=all7a)
#similar problem of missing data with anova

##try plotting scatter plots of each variable against 

#try lm again with ONLY complete variables  - also delete charge.ph and cost.ph

lin.a<- lm(inv.vs.cost~ Tot.Invoiced + Discipline + client.count + Business+
                   Biz.size + Biz.type + 
                   no.users + pc.contracttech +
                   pc.director + pc.midtech + pc.midpro+ pc.gradpro+ pc.seniortech+ pc.seniorpro+ pc.unknown+ 
                   pc.pro+ pc.tech + mean.peeps + hours.perday + 
                   code.director + 
                   ProjEng.Pos                   
           ,data= all7a)

df<- as.data.frame(summary(lin.a)$coefficients)
df$var<-rownames(summary(lin.a)$coefficients)
colnames(df)[names(df) %in% 'Pr(>|t|)']<-'Pval'
df %>% arrange(Pval)

aov.a<- aov(inv.vs.cost~ Tot.Invoiced + Discipline + client.count + Business+
                    Biz.size + Biz.type + 
                    no.users + pc.contracttech +
                    pc.director + pc.midtech + pc.midpro+ pc.gradpro+ pc.seniortech+ pc.seniorpro+ #pc.unknown+ 
                    pc.tech + pc.pro + mean.peeps + hours.perday + 
                    code.director + 
                    ProjEng.Pos                   
            ,data= all7a)
summary(aov.a)


# interesting - pc.midtech, no.users, pc.midpro, code.director, mean.peeps, and Discipline are statistically significant

#lets try adding one variable with heaps of NA's!!
#Job.Type.Primary
aov.a<- aov(inv.vs.cost~ inv.mlsto + Discipline + client.count + Business+
                    Biz.size + Biz.type + 
                    no.users + pc.contracttech +
                    pc.director + pc.midtech + pc.midpro+ pc.gradpro+ pc.seniortech+ pc.seniorpro+ #pc.unknown+ 
                    pc.pro + #pc.tech + 
              mean.peeps + hours.perday + 
                    code.director + Job.Type.Primary +
                    ProjEng.Pos                   
            ,data= all7a)
summary(aov.a)

lin.a<- glm(inv.vs.cost~ inv.mlsto + Discipline + client.count + Business+
                    Biz.size + Biz.type + 
                    no.users + pc.contracttech +
                    pc.director + pc.midtech + pc.midpro+ pc.gradpro+ pc.seniortech+ pc.seniorpro+ #pc.unknown+ 
                    pc.pro+ pc.tech + mean.peeps + hours.perday + 
                    code.director + Job.Type.Primary +
                   ProjEng.Pos                   
            ,family = Gamma(link='identity'), data= all7a)
summary(lin.a)

# None of the additinoal variables with lots of NA's are showing up as particularly significant

#lets plot scatter plots and see if any of the numeric variables should be logged

matrix<- spm(~ inv.vs.cost + Tot.Invoiced + client.count +
                     no.users + pc.contracttech 
             ,data=all7a, diagonal="boxplot")

matrix2<- spm(~ inv.vs.cost + pc.director + pc.midtech + pc.midpro+ pc.gradpro
             ,data=all7a, diagonal="boxplot")

matrix3<- spm(~ inv.vs.cost + pc.seniortech+ pc.seniorpro+ pc.unknown+ 
                      pc.pro
             ,data=all7a, diagonal="boxplot")

matrix4<- spm(~ inv.vs.cost + pc.tech + mean.peeps + hours.perday
             ,data=all7a, diagonal="boxplot")
        
#doesn't reveal much

#lets write a function for running these anova and lm models

BT.lin<- function(type= 'lm', data= all7a, add.var = 'Job.Type.Primary') {
        formula = paste("inv.vs.cost~ inv.mlsto + Discipline + client.count + Business+
                              Biz.size + Biz.type + 
                              no.users + pc.contracttech +
                              pc.director + pc.midtech + pc.midpro+ pc.gradpro+ pc.seniortech+ pc.seniorpro+
                              pc.pro + mean.peeps + hours.perday + 
                              code.director + ProjEng.Pos", add.var, sep="+")
        if (type== 'aov') {
            model= aov(as.formula(formula)              
                      ,data= data)
            return(summary(model)) 
        }
        else{
                model=lm(as.formula(formula)                     
                        ,data= data)
                df<- as.data.frame(summary(model)$coefficients)
                df$var<-rownames(summary(model)$coefficients)
                colnames(df)[names(df) %in% 'Pr(>|t|)']<-'Pval'
                return(df %>% arrange(-abs(Estimate)))
                
        }
            
                
}


BT.lin(type= 'lm', data= all7a[1:1313,], add.var = 'Job.Type.Primary') %>% slice(1:20)
BT.lin(type= 'lm', data= all7a, add.var = 'Job.Type.Primary') %>% slice(1:10)
BT.lin(type= 'lm', data= all7a[1314:2283,], add.var = 'Job.Type.Primary') %>% slice(1:10)
BT.lin(type= 'aov', data= all7a, add.var = 'Job.Type.Primary')
BT.lin(type= 'aov', data= all7a[1314:2283,], add.var = 'Job.Type.Primary')





###### DECISION TREES

tree<- rpart(inv.vs.cost~ inv.mlsto + Discipline + client.count + Business+
              Biz.size + Biz.type + 
              no.users + pc.contracttech +
              pc.director + pc.midtech + pc.midpro+ pc.gradpro+ pc.seniortech+ pc.seniorpro+ #pc.unknown+ 
              pc.pro + #pc.tech + 
              mean.peeps + hours.perday + 
              code.director + Job.Type.Primary +
              ProjEng.Pos                   
            ,data= all7a)
par(mfrow = c(1,1), xpd = NA, cex=1.5, mar = rep(2, 4))
prp(tree,extra=1)
plot(tree)
printcp(tree)
text(tree, use.n = TRUE)




########### NEED TO .. 
################# try running models for years 2009 onwards for example - see if sig stats change

#### CORRELATION ANALYSIS
library('corrgram', lib = 'C:/Progra~1/R/R-3.2.0/library')
str(all7a)
num.all<- all7a %>% select(client.count, no.employees, dist, charge.ph, no.users, pc.contracttech,
                           pc.director, pc.gradpro, pc.midpro, pc.midtech, pc.seniorpro, pc.seniortech, 
                           pc.unknown, pc.pro, pc.tech, Year, balance.mlsto,
                           timespan, Num.disc, Num.days, mean.peeps, hours.perday, hrs.mlsto, cost.mlsto, dis.sc.mlsto, 
                           inv.mlsto, return.pdol,
                           num.inv, mean.inv, Inv.freq, num.neginv, client.meaninv, client.invfreq, client.neginv, 
                           client.numinv,
                           client.totinv)
corrgram(num.all %>% select(client.count, charge.ph, no.users, pc.director, pc.gradpro, pc.midpro, 
                            pc.midtech, pc.contracttech, return.pdol,
                            pc.seniorpro, pc.seniortech, balance.mlsto,
                            pc.unknown, pc.pro, pc.tech, Year, Num.days, mean.peeps, hours.perday, hrs.mlsto, cost.mlsto,
                            dis.sc.mlsto, inv.mlsto, num.inv, mean.inv, client.meaninv, client.totinv,
                            timespan) %>% filter(complete.cases(.)), order=T, 
         lower.panel = panel.shade,
         upper.panel = panel.pie,
         text.panel = panel.txt,
         cor.method = 'spearman')
summary(num.all)
num.all[rowSums(is.na(num.all))>0,]


cor.test(all7a$return.pdol, all7a$client.count, method = c('pearson'))
cor.test(all7a$return.pdol, all7a$client.count, method = c('kendall'))

cor.all<- c("client.count", "no.users", "pc.director", "pc.gradpro", "pc.midpro", 
            "pc.midtech", "pc.contracttech", "return.pdol","Inv.freq", 'num.neginv','client.invfreq','client.neginv',
            "pc.seniorpro", "pc.seniortech", "no.employees", "dist", "Num.disc",'client.numinv',
            "pc.unknown", "pc.pro", "pc.tech", "Year", "Num.days", "mean.peeps", "hours.perday", "hrs.mlsto", "cost.mlsto",
            "dis.sc.mlsto", "inv.mlsto", "num.inv", "mean.inv", "client.meaninv", "client.totinv",
            "timespan")

for(i in 1:length(cor.all)) {
        print(cor.all[i])
        print(
                cor.test(all7a$return.pdol, all7a[,cor.all[i]], method = c('pearson'))
        )
        print(
                cor.test(all7a$return.pdol, all7a[,cor.all[i]], method = c('spearman'))
        )
        print(
                cor.test(all7a$return.pdol, all7a[,cor.all[i]], method = c('kendall'))
        )
        
}






