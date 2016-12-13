setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library('magrittr',lib='C:/Progra~1/R/R-3.1.3/library')
library('reshape2',lib='C:/Progra~1/R/R-3.1.3/library')
library("rpart",lib = 'C:/Program Files/R/R-3.2.0/library')

theData <- data.frame('category' = sample(LETTERS[1:3], 1000, replace = T),
                      'value' = rnorm(1000))
theData<-theData %>% filter(category=='A') 

ggplot(theData, aes(x = category, y = value)) +
        stat_ydensity(geom="segment", aes(xend=..x..+..scaled../3, 
                                          yend=..y.., alpha=..scaled..), size=1, trim=FALSE) +
        stat_ydensity(geom="segment", aes(xend=..x..-..scaled../3, 
                                          yend=..y.., alpha=..scaled..), size=1, trim=FALSE) +
        scale_alpha_continuous(range= c(0, 1)) +
        theme_bw()

p <- ggplot(theData, aes(category, value)) + 
        geom_tile(aes(fill = value)) + 
        scale_fill_gradient(low = "white", high = "steelblue")

p <- ggplot(theData, aes(category, value)) + 
        stat_ydensity(aes(xend=..x..+..scaled../3, 
                          yend=..y.., alpha=..scaled../3), geom="tile", position="identity")+
        geom_violin(aes(alpha=0))


ggplot(mtcars, aes(factor(cyl), mpg))+ 
        geom_violin(fill = wt, colour = "#3366FF")

### TRY CUSTOMISING BOXPLOT TO CREATE FAN LOOK



pos.plot3<- ggplot(theData, aes(x=category, y= value,
                              colour= category)) +
        
        stat_summary(fun.y= mean, geom='line') + 
        stat_summary(fun.y= mean, geom='point') +
        #stat_summary(fun.data= mean_sdl, geom='crossbar', mult =1) +
        theme(legend.position="right", axis.text.x=element_text(angle=45,hjust=1),
              text=element_text(size=12))
#         + scale_y_continuous(breaks = seq(0, 100, by = 10), limits=c(0,100))

ggplot(df[1,]) +
        geom_crossbar(aes(ymin = 146, ymax = 155, x = id, y = min),
                      fill = "blue", colour='blue',fatten = 2, alpha=0.5, linetype='blank', width=0.5)+
        geom_crossbar(aes(ymin = 139, ymax = 158, x = id, y = min),
                      fill = "blue", colour='blue',fatten = 2, alpha=0.3, linetype='blank', width=0.5) +
        geom_crossbar(aes(ymin = 130, ymax = 170, x = id, y = min),
                      fill = "blue", colour='blue',fatten = 2, alpha=0.1, linetype='blank', width=0.5) +
        geom_errorbar(aes(ymax=150, ymin=150, x=id, y=min), width=0.5, colour='navyblue', size=1.5) +
        geom_errorbar(aes(ymax=100, ymin=100, x=id, y=min), width=0.5, colour='red') +
        scale_y_continuous(breaks = seq(50, 170, by = 10), limits=c(50,170)) +
        labs(y="Likely Spend Over Quote (%)", x="Result", title= "Spend vs Quote Prediction") +
        theme(axis.text.x = element_blank())





### SETTLED ON THIS ONE!! ####### 

df <- read.table(text = " id  min  max 
    Sp1     150          151     ", header=TRUE)

#make a new dataframe with information about crossbar sizes
df2<- data.frame(id=c(rep("Sp1",3)), ymin= c(130, 139,146), ymax=c(170, 158, 155), probability=c('95%','75%','50%'))
df2 <- within(df2, 
                         probability <- factor(probability, 
                                               levels=c('95%','75%','50%')))




ggplot(df[1,]) +
        geom_crossbar(data=df2, aes(ymin = ymin, ymax = ymax, x = id, y=ymin, alpha=probability),
                      fill='blue', linetype='blank', width=0.25)+
        geom_errorbar(aes(ymax=150, ymin=150, x=id, y=min), width=0.25, colour='navyblue', size=1.5) +
        geom_errorbar(aes(ymax=100, ymin=100, x=id, y=min), width=0.25, colour='red') +
        scale_y_continuous(breaks = seq(60, 170, by = 10), limits=c(60,170)) +
        labs(y="Final Spend divided by Quote (%)", x="Result", title= "Prediction of Final Spend vs Initial Quote") +
        theme(axis.text.x = element_blank()) +
        annotate("text", x = 1.3, y = 101, label = "Final Spend = Initial Quote", colour='red', size=4)




p <- ggplot(theData, aes(category, value)) + 
        stat_ydensity(aes(xend=..x..+..scaled../3, 
                          yend=..y.., alpha=..scaled../3), geom="tile", position="identity")+
        geom_violin(aes(alpha=0))


p <- ggplot(theData, aes(category, value)) + 
        geom_violin(aes(alpha=value))
p


p <- ggplot(mtcars, aes(factor(cyl), mpg))

p + geom_violin(fill = 'grey')

p + geom_jitter(position = position_jitter(width = .15))

head(mtcars)


