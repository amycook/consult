## prep for bayesian network

#discretise numeric variables and change all 'NA' to blank

library("plyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library('magrittr',lib='C:/Progra~1/R/R-3.2.1/library')
library('reshape2',lib='C:/Progra~1/R/R-3.2.1/library')
library('hclust',lib='C:/Progra~1/R/R-3.2.1/library')

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
rect.hclust(fit, k = 6)

#assign a cluster number to all rows in data frame
all8a$b.pc.pro<- as.factor(cutree(fit, k=6))
head(all8a$b.pc.pro)

for(i in 1:6){
        print(
        summary(all8a %>% select(b.pc.pro, pc.pro) %>% filter(b.pc.pro ==i))
        )
}

#create clusters based on summary:
all8a[all8a$pc.pro>100,'pc.pro']<- 100
all8a$b.pc.pro<- cut(all8a$pc.pro, breaks=c(0,15,40,55,75,95,100), include.lowest=TRUE)

###
#timespan.cbrt
x<- dist(first.vec, method = 'euclidean')
fit<- hclust(x, method = 'ward.D2')
plot(fit)
k.choose=8
rect.hclust(fit, k = k.choose)

#assign a cluster number to all rows in data frame
all8a$b.timespan.cbrt<- as.factor(cutree(fit, k=8))
head(all8a$b.timespan.cbrt)

for(i in 1:k.choose){
        print(
        summary(all8a %>% select(b.timespan.cbrt, timespan.cbrt) %>% 
                        filter(b.timespan.cbrt ==i))
        )
}

#create clusters based on summary:
man.clust<- function(first.vec=all8a$timespan.cbrt, breaks.man=c(0,1,3,4,5,6.5,8,11,14.02)){
        final = cut(first.vec, breaks=breaks.man, include.lowest=TRUE)
        return(final)
}

all8a$b.timespan.cbrt<- man.clust(first.vec=all8a$timespan.cbrt, 
                                  breaks.man=c(0,1,3,4,5,6.5,8,11,14.02))

summary(all8a$b.timespan.cbrt)

###
#inv.mlsto.log
first.vec=all8a$inv.mlsto.log
x<- dist(first.vec, method = 'euclidean')
fit<- hclust(x, method = 'ward.D2')
plot(fit)
k.choose=9
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
                                  breaks.man=c(4.7,6.5,7,8,8.3,9,10,11,12,14.5))

summary(all8a$b.inv.log)


###
#client.totinv.log
first.vec=all8a$client.totinv.log
x<- dist(first.vec, method = 'euclidean')
fit<- hclust(x, method = 'ward.D2')
plot(fit)
k.choose=9
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
                            breaks.man=c(5,7.5,8,8.75,9,10,11,12,14.2))

summary(all8a$b.client.totinv)


###
#pc.majpos.log
first.vec=all8a$pc.majpos.log[!is.na(all8a$pc.majpos.log)]
x<- dist(first.vec, method = 'euclidean')
fit<- hclust(x, method = 'ward.D2')
plot(fit)
k.choose=5
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
                                   breaks.man=c(3.1,3.9,4.1,4.3,4.5,4.6))

summary(all8a$b.pcmajpos)


###
#return.pdol
first.vec=all8a$return.pdol
x<- dist(first.vec, method = 'euclidean')
fit<- hclust(x, method = 'ward.D2')
plot(fit)
k.choose=9
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

all8a$b.rpdol <- man.clust(first.vec=all8a$return.pdol, 
                                   breaks.man=c(-1,-.5,0,.15,0.3,0.6,0.85,1.3,1.76,3))

summary(all8a$b.rpdol)

####
##users
all8a$no.users<- as.factor(all8a$no.users)
summary(all8a$no.users)
all8a$no.users<- as.numeric(all8a$no.users)
#create clusters based on summary:
all8a$b.no.users<- man.clust(first.vec=all8a$no.users, 
                           breaks.man=c(0,1,2,3,4,5,6,50))

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

write.csv(all8b,'C:/Users/n9232371/Documents/Consultbusiness/data/all8b.csv' )

#what happens to missing values if i look at post 2008?
all8b<- merge(all8b, all8a %>% select(Year, mlsto), by='mlsto')


