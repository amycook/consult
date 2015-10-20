## prep for bayesian network

#discretise numeric variables and change all 'NA' to blank

library("plyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library('magrittr',lib='C:/Progra~1/R/R-3.2.1/library')
library('reshape2',lib='C:/Progra~1/R/R-3.2.1/library')
library('ggplot2',lib='C:/Progra~1/R/R-3.2.1/library')

# setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
all8a<- read.csv('C:/Users/n9232371/Documents/Consultbusiness/data/all8a.csv')[,-1]
head(all8a)

#discretise numeric columns!!!! pc.pro, timespan.cbrt, inv.mlsto.log, client.totinv.log,
#pc.majpos.log, return.pdol

###
#pc.pro
x<- dist(all8a$pc.pro, method = 'euclidean')
fit<- hclust(x, method = 'ward.D2')
plot(fit)
rect.hclust(fit, k = 5)

#assign a cluster number to all rows in data frame
all8a$b.pc.pro<- as.factor(cutree(fit, k=5))
head(all8a$b.pc.pro)

for(i in 1:5){
        print(
        summary(all8a %>% select(b.pc.pro, pc.pro) %>% filter(b.pc.pro ==i))
        )
}

#create clusters based on summary:
all8a[all8a$pc.pro>100,'pc.pro']<- 100
all8a$b.pc.pro<- cut(all8a$pc.pro, breaks=c(0,15,40,75,95,100), include.lowest=TRUE,
                     labels= c("p0_15", "p15_40", "p40_75", "p75_95", "p100"))

###
#timespan.cbrt
first.vec<- all8a$timespan.cbrt
x<- dist(first.vec, method = 'euclidean')
fit<- hclust(x, method = 'ward.D2')
plot(fit)
k.choose=3
rect.hclust(fit, k = k.choose)

#assign a cluster number to all rows in data frame
all8a$b.timespan.cbrt<- as.factor(cutree(fit, k= k.choose))
head(all8a$b.timespan.cbrt)

for(i in 1:k.choose){
        print(
        summary(all8a %>% select(b.timespan.cbrt, timespan.cbrt) %>% 
                        filter(b.timespan.cbrt ==i))
        )
}

#create clusters based on summary:
man.clust<- function(first.vec=all8a$timespan.cbrt, breaks.man=c(0,4.16,6.558,15),
                     labs = c("t0_70", "t70_280", "t280_")){
        final = cut(first.vec, breaks=breaks.man, include.lowest=TRUE, labels= labs)
        return(final)
}

all8a$b.timespan.cbrt<- man.clust(first.vec=all8a$timespan.cbrt, 
                                  breaks.man=c(0,4.16,6.558,15),
                                  labs = c("t0_70", "t70_280", "t280_"))

summary(all8a$b.timespan.cbrt)

###
#inv.mlsto.log
first.vec=all8a$inv.mlsto.log
x<- dist(first.vec, method = 'euclidean')
fit<- hclust(x, method = 'ward.D2')
plot(fit)
k.choose=4
rect.hclust(fit, k = k.choose)

#assign a cluster number to all rows in data frame
all8a$b.inv.log<- as.factor(cutree(fit, k=k.choose))
head(all8a$b.inv.log)

for(i in 1:k.choose){
        print(
                summary(all8a %>% select(b.inv.log, inv.mlsto.log) %>% 
                                filter(b.inv.log ==i))
        )
}

#create clusters based on summary:

all8a$b.inv.log<- man.clust(first.vec=all8a$inv.mlsto.log, 
                                  breaks.man=c(4.5, 7.81, 9, 11, 14.4),
                            labs = c("i_0_2_5k", "i_2_5k_8_1k", "i_8_1k_60k", "i_60k_1_6m"))

summary(all8a$b.inv.log)


###
#client.totinv.log
first.vec=all8a$client.totinv.log
x<- dist(first.vec, method = 'euclidean')
fit<- hclust(x, method = 'ward.D2')
plot(fit)
k.choose=3
rect.hclust(fit, k = k.choose)

#assign a cluster number to all rows in data frame
all8a$b.client.totinv<- as.factor(cutree(fit, k=k.choose))
head(all8a$b.client.totinv)

for(i in 1:k.choose){
        print(
                summary(all8a %>% select(b.client.totinv, client.totinv.log) %>% 
                                filter(b.client.totinv ==i))
        )
}

#create clusters based on summary:

all8a$b.client.totinv <- man.clust(first.vec=all8a$client.totinv.log, 
                            breaks.man=c(5, 8.75, 10, 14.4),
                            labs= c("ci_100_6k", "ci_6k_22k", "ci_22k_1m"))

summary(all8a$b.client.totinv)


###
#pc.majpos.log
first.vec=all8a$pc.majpos.log[!is.na(all8a$pc.majpos.log)]
x<- dist(first.vec, method = 'euclidean')
fit<- hclust(x, method = 'ward.D2')
plot(fit)
k.choose=3
rect.hclust(fit, k = k.choose)

#assign a cluster number to all rows in data frame
temp<- all8a[!is.na(all8a$pc.majpos.log),]
temp$b.pcmajpos<- as.factor(cutree(fit, k=k.choose))
head(temp$b.pcmajpos)

for(i in 1:k.choose){
        print(
                summary(temp %>% select(b.pcmajpos, pc.majpos.log) %>% 
                                filter(b.pcmajpos ==i))
        )
}

#create clusters based on summary:

all8a$b.pcmajpos <- man.clust(first.vec=all8a$pc.majpos.log, 
                                   breaks.man=c(3, 3.9, 4.1, 4.5, 4.7),
                              labs= c("pm_20_50", "pm_50_60", "pm60_90","pm_90_100"))

summary(all8a$b.pcmajpos)


###
#return.pdol
# split into profit/loss

first.vec=all8a$return.pdol
x<- dist(first.vec, method = 'euclidean')
fit<- hclust(x, method = 'ward.D2')
plot(fit)
k.choose=2
rect.hclust(fit, k = k.choose)

#assign a cluster number to all rows in data frame
all8a$b.rpdol<- as.factor(cutree(fit, k=k.choose))
head(all8a$b.rpdol)

for(i in 1:k.choose){
        print(
                summary(all8a %>% select(b.rpdol, return.pdol) %>% 
                                filter(b.rpdol ==i))
        )
}

#create clusters based on summary:

all8a$b.rpdol<- all8a$return.pdol 
all8a$b.rpdol<- ifelse(all8a$return.pdol<=0, "loss", "profit")
all8a$b.rpdol<- as.factor(all8a$b.rpdol)

summary(all8a$b.rpdol)

####
##users
all8a$no.users<- as.factor(all8a$no.users)
summary(all8a$no.users)
all8a$no.users<- as.numeric(all8a$no.users)
#create clusters based on summary:
all8a$b.no.users<- man.clust(first.vec=all8a$no.users, 
                           breaks.man=c(0,1,2,4,35),
                           labs= c("u1", "u2", "u3_4", "u_5_30"))

summary(all8a$b.no.users)

#prepare all8b as dataframe to go into Genie Smile

all8b<- all8a %>% select(-return.pdol, -timespan.cbrt, -pc.pro, -inv.mlsto.log,
                         -client.totinv.log, -pc.majpos.log, -no.users)

#replace all NA values with blank
all8b[]<- lapply(all8b, as.character)
str(all8b)
all8b[is.na(all8b)]<- ""
all8b[]<- lapply(all8b, as.factor)
summary(all8b)

#remove spaces, '.' and '/' from column titles
names(all8b)<- gsub("\\.|\\/|\\s", "_", names(all8b))

#remove spaces, '.' and '/' from all variable levels
all8b[]<- sapply(all8b, function(x) {gsub("\\.|\\/|\\s", "_", x)} )
all8b[]<- lapply(all8b, factor)

write.csv(all8b,'C:/Users/n9232371/Documents/Consultbusiness/data/all8b.csv' )

#look at complete cases for no.users, discipline, pc.pro, business, code.client,
#JD.Second, majority.pos, timespan.cbrt, inv.mlsto.log, pc.majpos.log

all8b<- read.csv('C:/Users/n9232371/Documents/Consultbusiness/data/all8b.csv',
                 na.strings = "")[,-1]
all8c<- all8b %>% select(b_no_users, Discipline, b_pc_pro, Business, code_client,
                         JD_Second, majority_pos, b_timespan_cbrt,
                         b_inv_log, b_pcmajpos, b_rpdol)


all8c<- all8c[complete.cases(all8c),]
dim(all8c)

write.csv(all8c,'C:/Users/n9232371/Documents/Consultbusiness/data/all8c.csv' )

summary(all8c)

# 