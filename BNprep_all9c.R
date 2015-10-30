## prep for bayesian network

#discretise numeric variables and change all 'NA' to blank

library("plyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library('magrittr',lib='C:/Progra~1/R/R-3.2.1/library')
library('reshape2',lib='C:/Progra~1/R/R-3.2.1/library')
library('ggplot2',lib='C:/Progra~1/R/R-3.2.1/library')

# setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
all9c<- read.csv('C:/Users/n9232371/Documents/Consultbusiness/data/all9c.csv')[,-1]
head(all9c)

# write a function to
# discretise numeric columns!!!! pc.pro, timespan.cbrt, inv.mlsto.log, client.totinv.log,
# pc.majpos.log, return.pdol

discretise<- function(df = all9c, extra.var = ('code.client', 'code.contact')){
        
        #pc.pro
        df[df$pc.pro>100 & !is.na(df$pc.pro),'pc.pro']<- 100
        df$b.pc.pro<- cut(df$pc.pro, breaks=c(0,15,40,75,95,100), include.lowest=TRUE,
                             labels= c("p0_15", "p15_40", "p40_75", "p75_95", "p100"))
        
        ###
        #timespan.cbrt
        #done already
        
        #create clusters based on summary:
        man.clust<- function(first.vec=all9c$timespan.cbrt, breaks.man=c(0,4.16,6.558,15),
                             labs = c("t0_70", "t70_280", "t280_")){
                final = cut(first.vec, breaks=breaks.man, include.lowest=TRUE, labels= labs)
                return(final)
        }
        
        ###
        #inv.mlsto.log
        
        all9c$b.inv.log<- man.clust(first.vec=all9c$inv.mlsto.log, 
                                    breaks.man=c(4.5, 7.81, 9, 11, 14.4),
                                    labs = c("i_0_2_5k", "i_2_5k_8_1k", "i_8_1k_60k", "i_60k_1_6m"))
        
        ###
        #client.totinv.log
        
        all9c$b.client.totinv <- man.clust(first.vec=all9c$client.totinv.log, 
                                           breaks.man=c(5, 8.75, 10, 14.4),
                                           labs= c("ci_100_6k", "ci_6k_22k", "ci_22k_1m"))
        
        ###
        #pc.majpos.log
        
        all9c$b.pcmajpos <- man.clust(first.vec=all9c$pc.majpos.log, 
                                      breaks.man=c(3, 3.9, 4.1, 4.5, 4.7),
                                      labs= c("pm_20_50", "pm_50_60", "pm60_90","pm_90_100"))
        
        
        ###
        #return.pdol
        # split into profit/loss
        
        all9c$f.rpdol<- all9c$return.pdol 
        all9c$f.rpdol<- ifelse(all9c$return.pdol<=0, "loss", "profit")
        all9c$f.rpdol<- as.factor(all9c$f.rpdol)
        
        ####
        ##users
        all9c$b.no.users<- man.clust(first.vec=all9c$no.users, 
                                     breaks.man=c(0,1,2,4,35),
                                     labs= c("u1", "u2", "u3_4", "u_5_30"))
        
        #prepare all8b as dataframe to go into Genie Smile
        
        vars = c()
        df<- df %>% select(-return.pdol, -timespan.cbrt, -pc.pro, -inv.mlsto.log,
                                 -client.totinv.log, -pc.majpos.log, -no.users)
        
        
        
        
}


#replace all NA values with blank
all9d<- all9c
all9d[]<- lapply(all9d, as.character)
str(all9d)
all9d[is.na(all9d)]<- ""
all9d[]<- lapply(all9d, as.factor)
summary(all9d)

#remove spaces, '.' and '/' from column titles
names(all9d)<- gsub("\\.|\\/|\\s", "_", names(all9d))

#remove spaces, '.' and '/' from all variable levels
all9d[]<- sapply(all9d, function(x) {gsub("[>]|\\-|\\.|\\/|\\s", "_", x)} )
all9d[]<- lapply(all9d, factor)
summary(all9d)

write.csv(all9d,'C:/Users/n9232371/Documents/Consultbusiness/data/all9d.csv' )

#mini complete.cases set
#remove spaces, '.' and '/' from column titles
names(all9c)<- gsub("\\.|\\/|\\s", "_", names(all9c))
all9c<- all9c %>% select(b_no_users, Discipline, b_pc_pro, Business, code_client,
                         JD_Second, majority_pos, b_timespan_cbrt,
                         b_inv_log, b_pcmajpos, b_rpdol)


all9e<- all9c[complete.cases(all9c),]
dim(all9e)

write.csv(all9e,'C:/Users/n9232371/Documents/Consultbusiness/data/all9e.csv' )


# 