
## prep for boosted trees

#discretise numeric variables and change all 'NA' to blank

library("plyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library('magrittr',lib='C:/Progra~1/R/R-3.2.1/library')
library('reshape2',lib='C:/Progra~1/R/R-3.2.1/library')
library('ggplot2',lib='C:/Progra~1/R/R-3.2.1/library')

# setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
all8a<- read.csv('C:/Users/n9232371/Documents/Consultbusiness/data/all8a.csv')[,-1]
head(all8a)

#try to predict profit/loss for return per dollar!
###
#return.pdol
# split into profit/loss

all8a$b.rpdol<- all8a$return.pdol 
all8a$b.rpdol<- ifelse(all8a$return.pdol<=0, "loss", "profit")
all8a$b.rpdol<- as.factor(all8a$b.rpdol)

summary(all8a$b.rpdol)

#build boosted tree


