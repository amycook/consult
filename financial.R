---
title: "Financial_Analysis"
author: "Amy Cook"
date: "October 21, 2015"
output: word_document
---

#Assess financial implications for a predictive model

We typically get a confusion matrix built from probabilities


```{r_loadpackages, echo=FALSE, include=FALSE}
library("plyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library('magrittr',lib='C:/Progra~1/R/R-3.2.1/library')
library('reshape2',lib='C:/Progra~1/R/R-3.2.1/library')
library('ggplot2',lib='C:/Progra~1/R/R-3.2.1/library')
library('gbm', lib = 'C:/Progra~1/R/R-3.2.2/library')
library('caret', lib = 'C:/Progra~1/R/R-3.2.2/library')
library('ROCR',lib='C:/Progra~1/R/R-3.2.2/library')

# setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
```

Load data

```{r, echo=FALSE}
all9a<- read.csv('C:/Users/n9232371/Documents/Consultbusiness/data/all9a.csv')[,-1]
all9a$b.timespan.cbrt<- cut(all9a$timespan.cbrt, 
                            breaks= c(0,2.7589, 4.2358, 6.5, 8.183, 10.307, 14.1), 
                            include.lowest=TRUE, 
                            labels= c("1d-3wk","3wk-2.5m", "2.5m-9m","9m-1.5y","1.5y-3y",">3y"))

set.seed(300)
sample<- sample(1:nrow(all9a), 2/3*nrow(all9a), replace=F)
train<- all9a[sample,]
test<- all9a[-sample,]
```

Run boosted tree

```{r, echo=FALSE}

formula<- "b.rpdol~.- mlsto - return.pdol-Year-code.client - code.contact-JD.Second - Business - timespan.cbrt"

fit<-gbm(as.formula(formula), 
         data=train, distribution="bernoulli", n.trees= 6000,
         shrinkage=0.001,interaction.depth=3,
         n.minobsinnode = 10)
pred<- predict(fit, test, n.tree=4000, type="response")
ans<- test$b.rpdol


```

Create vector of profit points

```{r, echo=FALSE}

threshold<- seq(0,1, by=.05)
fin.prof<- rep(0, length(threshold))

for(i in 1:length(threshold)){
        prof<- data.frame("invoiced" = exp(test$inv.mlsto.log), 
                  "pred" = pred,
                  "ans" = test$b.rpdol)
        #pred = probabilities
        prof$conf <- rep(0, nrow(prof))
        prof$conf <- ifelse(prof$pred >= threshold[i] & prof$ans == 1, 'TP', prof$conf)
        prof$conf <- ifelse(prof$pred >= threshold[i] & prof$ans == 0, 'FP', prof$conf)
        prof$profit<- rep(0, nrow(prof))
        prof$profit<- ifelse(prof$conf %in% "TP", prof$invoiced*0.2, prof$profit)
        prof$profit<- ifelse(prof$conf %in% "FP", -prof$invoiced, prof$profit)

        fin.prof[i] = sum(prof$profit)
        
}

prof.plot<- data.frame("threshold" = threshold, "final.profit" = fin.prof)

ggplot(prof.plot, aes(x=threshold, y=final.profit)) + geom_point() + geom_line()

```

