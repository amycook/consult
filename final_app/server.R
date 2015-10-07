setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
# setwd("~/OneDrive/shared files/Bligh Tanner/masters/data")
library('ggplot2', lib = 'C:/Progra~1/R/R-3.2.2/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.2.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.2.2/library')
library('magrittr',lib='C:/Progra~1/R/R-3.2.2/library')
library('reshape2',lib='C:/Progra~1/R/R-3.2.2/library')
library('shiny',lib='C:/Progra~1/R/R-3.2.2/library')
library('shinythemes',lib='C:/Progra~1/R/R-3.2.2/library')
library('FNN',lib='C:/Progra~1/R/R-3.2.2/library')
# library('ggplot2')
# library("dplyr")
# library("plyr")
# library('magrittr')
# library('reshape2')
# library('shiny')
# library('shinythemes')


all7<- read.csv('C:/Users/n9232371/Documents/Consultbusiness/data/all7.csv')[,-1]
all7$Start.Date<- as.Date(all7$Start.Date)
#delete anything more than 4 for inv.vs.cost
#delete anything less than .2 for inv.vs.cost
all7<- all7 %>% filter(!(inv.vs.cost<0.2), !(inv.vs.cost>4))

#data for final plot
df <- read.table(text = " id  min  max 
    Sp1     150          151     ", header=TRUE)

#make a new dataframe with information about crossbar sizes
df2<- data.frame(id=c(rep("Sp1",3)), ymin= c(19000, 22000,24000),
                 ymax=c(33000, 30000, 28000), probability=c('95%','75%','50%'))
df2 <- within(df2, 
              probability <- factor(probability, 
                                    levels=c('95%','75%','50%')))

#### CREATE TABLE DATA FRAME FOR NEAREST NEIGHBOURS ####

all7d<- read.csv('all7d.csv')[,-1]
all7d$mlsto<- as.character(all7d$mlsto)

# test.case<- all7d %>% select(Discipline, Business, Biz.type, code.contact, code.client, JD.Second, Billing.Type)

#function for retrieving nearest neighbours by inv.mlsto:
inv.knn<- function(df= all7d, predict= 'inv.mlsto', new.cases= c(1000), k=3){
        invo.knn = knn(df[,predict], df[new.cases,predict],
                       df$pc.pro, k=k)
        indices<- attr(invo.knn,  "nn.index")
        #checkout the nearest neigbours!
        df %>% slice(c(new.cases, indices[1,])) %>% return
        
}

#function for retrieving nearest neighbours by category - must first build binary matrix
#call binary matrix 'knn.binary'
vars.vec<- c('Discipline','Business','Biz.type','code.contact', 'code.client', 'JD.Second', 'Billing.Type')

binary.build<- function(vars= vars.vec, df= all7d){
        options("contrasts")
        #create final df bin.knn
        bin.knn= NULL
        #loop through each variable in vars vector
        for(i in 1:length(vars.vec)){
                temp = data.frame(df[, vars[i]])
                colnames(temp)<- c(vars[i])
                formula = paste('~',vars[i],'-1')
                a= model.matrix( as.formula(formula), temp)
                a= a[match(rownames(temp), rownames(a)),]
                bin.knn= cbind(bin.knn,a)
        }
        
        return(bin.knn)
}

knn.binary<- binary.build(vars= vars.vec, df= all7d)
knn.binary<- as.data.frame(knn.binary)

#NA values don't work in knn function so turn all NA into zeroes
knn.binary[is.na(knn.binary)]<- 0
knn.binary$pc.pro<- all7d$pc.pro
knn.binary$mlsto<- all7d$mlsto

bin<- knn.binary %>% select(-mlsto)

#knn function for categorical retrieving:

cat.knn<- function(df= all7d, nn.by= 'inv.mlsto', predict='pc.pro', case= c(1000), k=3, result.df= all7d){
        invo.knn = knn(df[,nn.by], df[case,nn.by],
                       df[,predict], k=k)
        indices= attr(invo.knn,  "nn.index")
        dists= attr(invo.knn, 'nn.dist') %>% t %>% as.data.frame %>% round(2)
        #checkout the nearest neigbours!
        final = cbind(result.df %>% slice(c(indices[1,])), dists)
        colnames(final)[names(final) %in% 'V1']<-'knn.dist'
        
        #final actions
        assign('index', indices, envir=.GlobalEnv)
        return(final)
}




########SHINY FUNCTION #########

shinyServer(
        function(input,output){
                
                # Compute the forumla text in a reactive function since it is 
                # shared by the output$caption and output$mpgPlot functions
#                 formulaText<- reactive({
#                         paste("Spend vs Quote Prediction for", input$disc, "Job", sep=" ")
#                 })
#                 
#                 # Return the formula text for printing as a caption
#                 output$caption <- renderText( {
#                         formulaText()
#                 })
                

                
                #generate a plot of requested variable
                
                #produce table dataframe
                b<- reactive({
                        cat.knn(df= bin, nn.by= colnames(bin)[!grepl('pc.pro', colnames(bin))], 
                            predict = 'pc.pro', case= input$case, k=10, result.df= all7d)
                })
                
                c<- reactive({
                        inv.knn(df= b(), predict= 'inv.mlsto', new.cases= 1, k=input$ks) %>% slice(-1)
                })
                
#                 e<- reactive({
#                         b()[,input$column, drop=FALSE]
#                 })


                e= reactive({
                        if(input$column == 'NA'){
                                test= c()
                        }
                        else {
                                f=b()
#                                 g= f[,input$column,drop=FALSE] %in% f[,input$column,drop=FALSE][1]
                                test= f[f[,input$column] %in% f[,input$column][1],,drop=FALSE]
                                test= inv.knn(df= test, predict= 'inv.mlsto', new.cases= 1, k= input$ks) %>% slice(-1)
                        }    
                })
                        
                                
                output$fee.plot<- renderPlot( {

                        
                        s= ggplot(df[1,]) +
                                geom_crossbar(data=df2, aes(ymin = ymin, ymax = ymax, x = id, y=ymin, alpha=probability),
                                              fill='blue', linetype='blank', width=0.25)+
                                geom_errorbar(aes(ymax=26000, ymin=26000, x=id, y=min), width=0.25, colour='navyblue', size=1.5) +
                                geom_errorbar(aes(ymax=20000, ymin=20000, x=id, y=min), width=0.25, colour='red', linetype = 2) +
                                scale_y_continuous(breaks = seq(15000, 40000, by = 5000)) +
                                ylim(15000,40000) +
                                labs(y="Final Spend ($)", x="New Project", title= "Prediction of Final Spend") +
                                theme(axis.text.y = element_blank(),
                                      plot.title = element_text(size=18, face='plain'),
                                      axis.title.y = element_text(size = 16),
                                      axis.title.x = element_text(size=16),
                                      axis.text = element_text(size = 16)) +
                                annotate("text", x = 1.3, y = 20000, label = "Final Spend = Initial Quote", colour='red', size=4)+
                                coord_flip()
                        
                        print(s)

                })
                #Job Details
                output$knn.table<- renderTable({
                        k = e() %>% select(mlsto, Discipline, JD.Second, Num.disc, timespan, knn.dist
                                           
                                           )
                        
                        print(k)
                })
                #Finances
                output$knn.table1<- renderTable({
                        k1 = e() %>% select(mlsto, Billing.Type, inv.mlsto, return.pdol, cost.mlsto, balance.mlsto
                                           
                                           )
                        
                        print(k1)
                })
                #Client details
                output$knn.table2<- renderTable({
                        k2 = e() %>% select(mlsto, code.client, code.contact, Business, Biz.type, client.totinv
                                           
                                           )
                        
                        print(k2)
                })
                #staff details
                output$knn.table3<- renderTable({
                        k3 = e() %>% select(mlsto, code.director, pc.pro, majority.pos, pc.majpos, code.ProjEng 
                                           
                                           )
                        
                        print(k3)
                })
                
        }
)