## prep for bayesian network

#discretise numeric variables and change all 'NA' to blank

library("plyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library('magrittr',lib='C:/Progra~1/R/R-3.2.1/library')
library('reshape2',lib='C:/Progra~1/R/R-3.2.1/library')
library('ggplot2',lib='C:/Progra~1/R/R-3.2.1/library')

# setwd("C:/Users/n9232371/Documents/Consultbusiness/data")


# write a function to
# discretise numeric columns!!!! pc.pro, timespan.cbrt, inv.mlsto.log, client.totinv.log,
# pc.majpos.log, return.pdol

discretise<- function(df.string = 'all10rf', extra.var = NULL){
        
        df = get(df.string)
        
        #pc.pro
        df[df$pc.pro>100 & !is.na(df$pc.pro),'pc.pro']<- 100
        df$b.pc.pro<- cut(df$pc.pro, breaks=c(0,15,40,75,95,100), include.lowest=TRUE,
                             labels= c("p0_15", "p15_40", "p40_75", "p75_95", "p100"))
        
        ###
        #timespan.cbrt
        #done already
        
        #create clusters based on summary:
        man.clust<- function(first.vec=df$timespan.cbrt, breaks.man=c(0,4.16,6.558,15),
                             labs = c("t0_70", "t70_280", "t280_")){
                final = cut(first.vec, breaks=breaks.man, include.lowest=TRUE, labels= labs)
                return(final)
        }
        
        ###
        #inv.mlsto.log
        
        df$b.inv.log<- man.clust(first.vec=df$inv.mlsto.log, 
                                    breaks.man=c(4.5, 7.81, 9, 11, 14.4),
                                    labs = c("i_0_2_5k", "i_2_5k_8_1k", "i_8_1k_60k", "i_60k_1_6m"))
        
        ###
        #client.totinv.log
        
        df$b.client.totinv <- man.clust(first.vec=df$client.totinv.log, 
                                           breaks.man=c(5, 8.75, 10, 14.4),
                                           labs= c("ci_100_6k", "ci_6k_22k", "ci_22k_1m"))
        
        ###
        #pc.majpos.log
        
        df$b.pcmajpos <- man.clust(first.vec=df$pc.majpos.log, 
                                      breaks.man=c(3, 3.9, 4.1, 4.5, 4.7),
                                      labs= c("pm_20_50", "pm_50_60", "pm60_90","pm_90_100"))
        
        
        ###
        #return.pdol
        # split into profit/loss
        
        if('return.pdol' %in% names(df)){
                df$f.rpdol<- df$return.pdol 
                df$f.rpdol<- ifelse(df$return.pdol<=0, "loss", "profit")
                df$f.rpdol<- as.factor(df$f.rpdol)
        } else{
                if('b.rpdol' %in% names(df)){
                        df$f.rpdol<- as.factor(df$b.rpdol)
                        levels(df$f.rpdol)[levels(df$f.rpdol)=="0"] <- "profit"
                        levels(df$f.rpdol)[levels(df$f.rpdol)=="1"] <- "loss"
                }
        }

        
        ####
        ##users
        df$b.no.users<- man.clust(first.vec=df$no.users, 
                                     breaks.man=c(0,1,2,4,35),
                                     labs= c("u1", "u2", "u3_4", "u_5_30"))
        
        #prepare all8b as dataframe to go into Genie Smile
        
        vars = c("f.rpdol", "Discipline", "b.pc.pro", "b.timespan.cbrt",
                 "b.no.users", "b.inv.log", "b.client.totinv", "Business", 
                 "majority.pos", "JD.Second", "b.pcmajpos", "Billing.Type")
        vars.combo = c(vars, extra.var)
        
        df<- df[, names(df) %in% vars.combo]
        
        #replace all NA values with blank
        df[]<- lapply(df, as.character)
        df[is.na(df),]<- ""
        df[]<- lapply(df, as.factor)
        
        #remove spaces, '.' and '/' from column titles
        names(df)<- gsub("\\.|\\/|\\s", "_", names(df))
        
        #remove spaces, '.' and '/' from all variable levels
        df[]<- sapply(df, function(x) {gsub("[>]|\\-|\\.|\\/|\\s", "_", x)} )
        df[]<- lapply(df, factor)

        assign(paste(df.string, "BN", sep=""), df, envir = .GlobalEnv)
        
        write.csv(df, paste('C:/Users/n9232371/Documents/Consultbusiness/data/',df.string,"BN.csv",
                            sep = ""))
        
}

all10mice<- read.csv('C:/Users/n9232371/Documents/Consultbusiness/data/all10mice.csv')[,-1]
all10rf<- read.csv('C:/Users/n9232371/Documents/Consultbusiness/data/all10rf.csv')[,-1]
all10micec4<- read.csv('C:/Users/n9232371/Documents/Consultbusiness/data/all10mice_c4.csv')[,-1]

discretise(df.string = 'all10rf', extra.var = NULL)
discretise(df.string = 'all10mice', extra.var = NULL)
discretise(df.string = 'all10micec4', extra.var = c('reduc.client', 'reduc.contact'))




# 