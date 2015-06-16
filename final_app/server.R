setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
# setwd("~/OneDrive/shared files/Bligh Tanner/masters/data")
# library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
# library("dplyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
# library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
# library('magrittr',lib='C:/Progra~1/R/R-3.1.3/library')
# library('reshape2',lib='C:/Progra~1/R/R-3.1.3/library')
# library('shiny')
# library('shinythemes',lib='C:/Progra~1/R/R-3.2.0/library')


all7<- read.csv('all7.csv')[,-1]
all7$Start.Date<- as.Date(all7$Start.Date)
#delete anything more than 4 for inv.vs.cost
#delete anything less than .2 for inv.vs.cost
all7<- all7 %>% filter(!(inv.vs.cost<0.2), !(inv.vs.cost>4))

#data for final plot
df <- read.table(text = " id  min  max 
    Sp1     150          151     ", header=TRUE)

#make a new dataframe with information about crossbar sizes
df2<- data.frame(id=c(rep("Sp1",3)), ymin= c(130, 139,146), ymax=c(170, 158, 155), probability=c('95%','75%','50%'))
df2 <- within(df2, 
              probability <- factor(probability, 
                                    levels=c('95%','75%','50%')))



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
                
                
                output$fee.plot<- renderPlot( {

                        
                        s= ggplot(df[1,]) +
                                geom_crossbar(data=df2, aes(ymin = ymin, ymax = ymax, x = id, y=ymin, alpha=probability),
                                              fill='blue', linetype='blank', width=0.25)+
                                geom_errorbar(aes(ymax=150, ymin=150, x=id, y=min), width=0.25, colour='navyblue', size=1.5) +
                                geom_errorbar(aes(ymax=100, ymin=100, x=id, y=min), width=0.25, colour='red') +
                                scale_y_continuous(breaks = seq(80, 170, by = 10), limits=c(80,170)) +
                                labs(y="Final Spend divided by Quote (%)", x="New Project", title= "Prediction of Final Spend vs Initial Quote") +
                                theme(axis.text.y = element_blank(),
                                      plot.title = element_text(size=18, face='plain'),
                                      axis.title.y = element_text(size = 16),
                                      axis.title.x = element_text(size=16),
                                      axis.text = element_text(size = 16)) +
                                annotate("text", x = 1.3, y = 101, label = "Final Spend = Initial Quote", colour='red', size=4)+
                                coord_flip()
                        
                        print(s)

                })
                
        }
)