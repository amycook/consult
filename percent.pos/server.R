setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library('magrittr',lib='C:/Progra~1/R/R-3.1.3/library')
library('reshape2',lib='C:/Progra~1/R/R-3.1.3/library')
library('shiny')

shinyServer(
        function(input,output){
                
                # Compute the forumla text in a reactive function since it is 
                # shared by the output$caption and output$mpgPlot functions
                formulaText<- reactive(function(){
                        paste("Position percentages in", input$disc, sep=" ")
                })
                
                # Return the formula text for printing as a caption
                output$caption <- renderText(function() {
                        formulaText()
                })
                
                #generate a plot of requested variable
                
                output$pos.plot<- renderPlot(function() {
                        #check for the input variable
                        if(input$disc == 'All') {
                                posData<- sub10}

                        else{
                                posData<- sub10 %>% filter(Discipline == input$disc)
                        }
                        
                        p<- ggplot(posData, aes(x=variable, y= percent.hrs,
                                              colour= variable)) + 
                                stat_summary(fun.y= mean, geom='point') + 
                                stat_summary(fun.data= mean_sdl, geom='crossbar', mult =1) +
                                geom_jitter(alpha= I(1/2)) +
                                theme(legend.position="none", axis.text.x=element_text(angle=45,hjust=1),
                                      text=element_text(size=12))
                        print(p)

                })
                
        }
)


