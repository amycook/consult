library('plotrix', lib = 'C:/Progra~1/R/R-3.2.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library('magrittr',lib='C:/Progra~1/R/R-3.2.1/library')
library('ggplot2',lib='C:/Progra~1/R/R-3.2.1/library')

all7d<- read.csv('C:/Users/n9232371/Documents/Consultbusiness/data/all7d.csv')[,-1]
all7d$Post.Code<- as.factor(all7d$Post.Code)

#lets look at case 960
#most important variables were:
# no.users, inv.mlsto.log, pc.pro, timespan, majority.pos, Discipline
#make a radar chart with no.users, inv.mlsto, pc.pro, client.code, majority.pos

#create a new column for profitable or not

all7d$good<- ifelse(all7d$return.pdol<0, 'bad','good')
all7d$good<- as.factor(all7d$good)

#create invoiced amount categories
all7d$Inv.cat<- rep(NA, nrow(all7d))
all7d$Inv.cat<- ifelse(all7d$inv.mlsto <5000 , '<$5k', all7d$Inv.cat)
all7d$Inv.cat<- ifelse(all7d$inv.mlsto <20000 & all7d$inv.mlsto >=5000, '$5-20k', all7d$Inv.cat)
all7d$Inv.cat<- ifelse(all7d$inv.mlsto <60000 & all7d$inv.mlsto >=20000, '$20-60k', all7d$Inv.cat)
all7d$Inv.cat<- ifelse(all7d$inv.mlsto >=60000 , '>$60k', all7d$Inv.cat)
all7d$Inv.cat<- as.factor(all7d$Inv.cat)
all7d <- within(all7d, 
                Inv.cat <- factor(Inv.cat,levels=
                                          c('<$5k', '$5-20k', '$20-60k', '>$60k')))

#create pc.pro categories, use hierarchical clustering to find groups
#Ward's clustering

x<- dist(all7d %>% select(pc.pro), method = 'euclidean')
fit<- hclust(x, method = 'ward.D2')
plot(fit)

rect.hclust(fit, k=6, border='red')
# 6 clusters looks pretty good

# how many points in each cluster?
all7d$pcpro.cat<- cutree(fit,k=6)
all7d$pcpro.cat<- as.factor(all7d$pcpro.cat)
summary(all7d$pcpro.cat)
all7d %>% select(pcpro.cat, pc.pro) %>% filter(pcpro.cat == '1') %>% summary #0-15%
all7d %>% select(pcpro.cat, pc.pro) %>% filter(pcpro.cat == '2') %>% summary #39-55%
all7d %>% select(pcpro.cat, pc.pro) %>% filter(pcpro.cat == '3') %>% summary #15-39%
all7d %>% select(pcpro.cat, pc.pro) %>% filter(pcpro.cat == '4') %>% summary #93-100%
all7d %>% select(pcpro.cat, pc.pro) %>% filter(pcpro.cat == '5') %>% summary #55-74%
all7d %>% select(pcpro.cat, pc.pro) %>% filter(pcpro.cat == '6') %>% summary #75-94%

#rationalise to 0-15, 15-40, 40-55, 55-75,75-90, 90-100
all7d$pcpro.cat<- ifelse(all7d$pc.pro <15 , '<15%', all7d$pcpro.cat)
all7d$pcpro.cat<- ifelse(all7d$pc.pro <40 & all7d$pc.pro >=15, '15-40%', all7d$pcpro.cat)
all7d$pcpro.cat<- ifelse(all7d$pc.pro <55 & all7d$pc.pro >=40, '40-55%', all7d$pcpro.cat)
all7d$pcpro.cat<- ifelse(all7d$pc.pro <75 & all7d$pc.pro >=55, '55-75%', all7d$pcpro.cat)
all7d$pcpro.cat<- ifelse(all7d$pc.pro <90 & all7d$pc.pro >=75, '75-90%', all7d$pcpro.cat)
all7d$pcpro.cat<- ifelse(all7d$pc.pro >=90, '>90%', all7d$pcpro.cat)
all7d$pcpro.cat<- as.factor(all7d$pcpro.cat)


all7d$good<- ifelse(all7d$return.pdol<.1, 'bad','good')
all7d$good<- as.factor(all7d$good)




#try barchart and segment return.pdol into categories

#create pc.pro categories, use hierarchical clustering to find groups
#Ward's clustering

x2<- dist(all7d %>% select(return.pdol), method = 'euclidean')
fit2<- hclust(x2, method = 'ward.D2')
plot(fit2)

rect.hclust(fit2, k=9, border='red')
# 6 clusters looks pretty good
# how many points in each cluster?
all7d$rpdol.cat<- cutree(fit2,k=9)
all7d$rpdol.cat<- as.factor(all7d$rpdol.cat)
summary(all7d$rpdol.cat)

clust.sum<- function(clust.var = 'rpdol.cat', var = 'return.pdol', clust.num = c(1:9), df= all7d){
        for(i in 1:length(clust.num)){
                print(
                        df[df[,clust.var] %in% clust.num[i],c(clust.var,var)] %>% summary
        )
        }
        }

#clust 1: 0.33- 0.58 ----
#clust 2: -.06 to -.44 --------
#clust 3: -0.45 to -1 ------
#clust 4: 0.17 to 0.33 ------
#clust 5: -0.05 to 0.17 -------
#clust 6: 0.58- 0.84 ------
#clust 7: 1.8- 3.0
#clust 8: 0.83- 1.3 ----
#clust 9: 1.3- 1.75 ----

all7d$rpdol.cat<- cut(all7d$return.pdol, breaks = c(-1,-0.5, 0, 0.15, 0.3, 0.6, 0.8, 1.25, 1.75,3))
summary(all7d$rpdol.cat)

case= 960

vars= c('no.users','code.client', 'majority.pos', 'Inv.cat', 'pcpro.cat')
tot=NULL
for(i in 1:5){
        reduced = all7d[all7d[,vars[i]] %in% all7d[case, vars[i]],]
        pc.rcat = round(prop.table(table(reduced$rpdol.cat))*100,0) %>% as.data.frame
        pc.rcat$var = rep(vars[i],9)
        tot = rbind(tot, pc.rcat)
}

tot

ggplot(tot, aes(x = var, y= Freq, fill = Var1)) + geom_bar(stat='identity')

