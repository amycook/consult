# impute missing values

library('mice', lib = 'C:/Progra~1/R/R-3.2.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library('magrittr',lib='C:/Progra~1/R/R-3.2.1/library')
library('reshape2',lib='C:/Progra~1/R/R-3.2.1/library')
library('ggplot2',lib='C:/Progra~1/R/R-3.2.1/library')
library('gbm', lib = 'C:/Progra~1/R/R-3.2.2/library')
library('caret', lib = 'C:/Progra~1/R/R-3.2.2/library')
library('pROC',lib='C:/Progra~1/R/R-3.2.2/library')

library('randomForest', lib = 'C:/Progra~1/R/R-3.2.2/library')
all9c<- read.csv('C:/Users/n9232371/Documents/Consultbusiness/data/all9c.csv')[,-1]
all9c$b.rpdol<- as.factor(all9c$b.rpdol)

ab<- rfImpute(b.rpdol ~ Discipline + pc.pro + b.timespan.cbrt + no.users + inv.mlsto.log +
                 client.totinv.log + pc.majpos.log + majority.pos + Business + JD.Second, data= all9c,
              ntree= 500)

experiment<- mice.impute.rf(all9c$pc.pro, ry = 
                            x= all9c[,names(all9c) %in% c('no.users', 'Discipline', 'Business', "timespan.cbrt",
                                                      "inv.mlsto.log", 'client.totinv.log', 'return.pdol','b.rpdol',
                                                      'Year')], 
                            ntree= 500)

testing<- mice(all9c, meth = 'rf', ntree =10)

## test if it helps boosted trees

#convert predictor variable back to numeric 0,1

levels(ab$b.rpdol)<- c("0", "1")
ab$b.rpdol<- as.character(ab$b.rpdol)
ab$b.rpdol<- as.integer(ab$b.rpdol)
all9c<- read.csv('C:/Users/n9232371/Documents/Consultbusiness/data/all9c.csv')[,-1]


#make fresh random sample of data
conf.matrix<- list('1' = data.frame(c(0,0), c(0,0)), '2' = data.frame(c(0,0), c(0,0)))
auc<- double(2)
colours <- c("red", "yellow")

set.seed(500)
sample<- sample(1:nrow(all9c), 2/3*nrow(all9c), replace=F)
train<- list(all9c[sample,], ab[sample,])
test<- list(all9c[-sample,], ab[-sample,])


for(i in 1:2){
        
        formula<- paste("b.rpdol ~ Discipline + pc.pro + b.timespan.cbrt + no.users + inv.mlsto.log +
                 client.totinv.log + pc.majpos.log + majority.pos + Business")
        
        fit<-gbm(as.formula(formula), 
                 data=train[[i]], distribution="bernoulli", n.trees= 10000,
                 shrinkage=0.001,interaction.depth=4,
                 n.minobsinnode = 20)
        pred<- predict(fit, test[[i]], n.tree=10000, type="response")
        
        #pROC
        pred.p<- roc(test[[i]]$b.rpdol, pred)
        
        if(i==1){
                plot.roc(pred.p, col= colours[i], print.thres = T)
                
        } else{
                plot.roc(pred.p, col= colours[i], add=TRUE , print.thres = T)
        }
        
        auc[i]<- auc(pred.p)
        
        #confusion matrix
        conf.mat<- coords(pred.p, "best")
        pred1<- ifelse(pred>= conf.mat[1], 1, 0)
        temp2<- data.frame('pred' = pred1, 'ans' = test[[i]]$b.rpdol)
        conf.matrix[[i]] = as.data.frame.matrix(table(temp2$pred, temp2$ans))
        cat(i," ")
}

conf.matrix
auc

# imputing does NOT help boosted trees!! ever so slightly worse AUC

