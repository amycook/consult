setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
# setwd("~/OneDrive/shared files/Bligh Tanner/masters/data")
library('ggplot2', lib = 'C:/Progra~1/R/R-3.2.3/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.2.3/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.2.3/library')
library('magrittr',lib='C:/Progra~1/R/R-3.2.3/library')
library('reshape2',lib='C:/Progra~1/R/R-3.2.3/library')
library('shiny',lib='C:/Progra~1/R/R-3.2.3/library')
library('shinythemes',lib='C:/Progra~1/R/R-3.2.3/library')
library('FNN',lib='C:/Progra~1/R/R-3.2.3/library')
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
df2<- data.frame(id=c(rep("Sp1",1)), ymin= c(4), y.mean = c(13),
                 ymax=c(22), confidence=c('90%'))
df2 <- within(df2, 
              confidence <- factor(confidence, 
                                    levels=c('90%')))

#### CREATE TABLE DATA FRAME FOR NEAREST NEIGHBOURS ####

all7d<- read.csv('C:/Users/n9232371/OneDrive/shared files/Bligh Tanner/masters/data/all7d.csv')[,-1]
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

cat.knn<- function(df= bin, nn.by= 'inv.mlsto', predict='pc.pro', case= c(1000), k=3, result.df= all7d){
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
                            predict = 'pc.pro', case= round(input$pc.pro/100*2364,0), k=10, result.df= all7d)
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
                                test= f[f[,input$column] %in% f[,input$column][1],drop=FALSE]
                                test= inv.knn(df= test, predict= 'inv.mlsto', new.cases= 1, k= input$ks) %>% slice(-1)
                        }
                        test
                })
                        
                                
                output$fee.plot<- renderPlot( {
                        
                        x.height = 0.2 
                        ticks = data.frame(xs = rep(x.height,11), ys = seq(0,100, by = 10))
                        minor.ticks = data.frame(xs = rep(x.height, length(seq(0,100, by = 2.5))), 
                                                 ys = seq(0,100, by = 2.5))

                        
                        s= ggplot(df[1,]) +
                                geom_pointrange(data = df2, aes(y = y.mean, ymin = ymin, ymax = ymax), 
                                                x = x.height, size= 2.5, alpha = .7,
                                                shape = 16, colour = 'royalblue3') +
                                geom_segment(x = x.height, xend = x.height, y = 0, yend = 100, alpha = 0.75, size = 0.3) +
                                geom_point(data = ticks, aes(x= xs, y= ys), size = 2) + 
                                geom_point(data = minor.ticks, aes(x= xs, y= ys), size = 0.1) + 
                                scale_y_continuous(limit = seq(0,100, by = 100),breaks = seq(0, 100, by = 10)) +
                                labs(y="Probability (%)", x="", title= "Probability of Project Making a Loss") +
                                xlim(0,1) +
                                theme(axis.text.y = element_blank(),
                                      plot.title = element_text(size=18, face='plain'),
                                      axis.title.y = element_text(size = 16),
                                      axis.title.x = element_text(size=16),
                                      axis.text = element_text(size = 16),
                                      legend.text = element_text(size = 12),
                                      legend.title = element_text(size = 12),
                                      panel.background = element_rect(fill = "white"),
                                      axis.ticks.x=element_blank(),
                                      axis.ticks.y=element_blank()) +
                                # annotate("text", x = 1.3, y = 20000, label = "Final Spend = Initial Quote", colour='red', size=4)+
                                scale_colour_brewer(palette = 'GnBu', direction = -1) +
                                coord_flip()
                        
                        # s$layers = c(geom_segment(), s$layers)
                        
                        print(s)

                },
                height = 150, width = 700)
                
                # knn plot
                        
                output$knn.plot <- renderPlot( {
                        if(input$colour %in% 'JD.Second'){
                                colour.title = 'Job Type'
                        } else {if( input$colour %in% 'code.client'){
                                colour.title = "Client"
                        } else {
                                colour.title = "Majority Employee"
                        }}
                        
                        k4 = ggplot(e(), aes(x = inv.mlsto, y = return.pdol)) + 
                                geom_point(aes_string(colour = input$colour, size = input$size), 
                                           alpha = 0.6, shape = 16) +
                                labs(title = "Similar Past Projects", y = "Return per Dollar", x = "Invoiced Amount ($)",
                                     colour = colour.title, size = input$size) +
                                # geom_text(aes(label = mlsto), check_overlap = TRUE, nudge_x = 0.1,
                                          # nudge_y = -0.1, colour = 'gray50') +
                                scale_colour_brewer(palette = "Set1", na.value = 'azure4') +
                                scale_size(range = seq(5,12, by = 7)) +
                                geom_hline(yintercept = 0, alpha = 0.6, colour = 'gray70', size =1) + 
                                theme(panel.background = element_rect(fill = "antiquewhite"),
                                      text=element_text(size=16)) +
                                scale_y_continuous(limits = seq(-1, 1.2, by = 2.2),
                                                   breaks = seq(-1, 1.2, by = 0.25))
                                
                        plot(k4)
                },
                height = 550, width = 900
                )
                
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