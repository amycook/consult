setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library('magrittr',lib='C:/Progra~1/R/R-3.1.3/library')
library('reshape2',lib='C:/Progra~1/R/R-3.1.3/library')
library('shiny')

#load Data
all6<- read.csv('all6.csv')[,-1]
all6$Start.Date<- as.Date(all6$Start.Date)
dup1<- all6[,names(all6) %in% c('mlsto')]
sub10<-all6[!all6$Tot.Invoiced==0 & !duplicated(dup1),]
sub10<- melt(sub10 %>% select(code.jobnum, pc.contracttech, pc.director, pc.gradpro, pc.midpro, pc.midtech, pc.seniorpro,
                              pc.seniortech, pc.unknown),value.name='percent.hrs') %>% filter(percent.hrs>.5)
sub10 <- merge(sub10, all6 %>% select(Start.Date, balance.mlsto, Discipline, code.jobnum, return.pdol), by= 'code.jobnum', all.x=TRUE, all.y=FALSE) %>%
        arrange(Start.Date)
sub10$Start.Date<- as.Date(sub10$Start.Date)
sub10$return.cut<- cut_number(sub10$return.pdol, 10, labels= c(1:10)) %>% as.integer()

#shiny function

shinyServer(
        function(input,output){
                
                # Compute the forumla text in a reactive function since it is 
                # shared by the output$caption and output$mpgPlot functions
                formulaText<- reactive({
                        paste("Position percentages in", input$disc, sep=" ")
                })
                
                # Return the formula text for printing as a caption
                output$caption <- renderText( {
                        formulaText()
                })
                
                #check for the input variable
                data.r= reactive ({
                        if(input$disc == 'All') {
                                posData<- sub10}
                        
                        else{
                                posData<- sub10 %>% filter(Discipline == input$disc)
                        }
                        
                })
        
                
                #subset data based on slider inputs for Date.

                data.r2 = reactive({
                        a = subset(data.r(), Start.Date >= input$date[1] & Start.Date <= input$date[2])
                        return(a)
                })

                
                #generate a plot of requested variable
                
                
                output$pos.plot<- renderPlot( {

                                               
                        dd= data.r2()
                        
                        s= ggplot(dd, aes(x=percent.hrs, y= return.cut, colour=variable)) + 
                                geom_smooth(method=loess, se=TRUE, aes(fill=variable), alpha=.2) + 
#                                 stat_summary(fun.x= mean, geom='point', shape = 2) +
                                #stat_summary(fun.data= mean_sdl, geom='crossbar', mult =1) +
                                theme(legend.position="none", axis.text.x=element_text(angle=45,hjust=1),
                                      text=element_text(size=12))+
                                scale_x_continuous(breaks = seq(0, 100, by = 10), limits=c(0,100)) +
                                scale_y_continuous(breaks = seq(1, 10, by = 1), limits=c(1,10)) +
                                facet_wrap(~ variable) + #, labeller =  label_value(variable, c('a','b','c','d','e','f','g','h')) +
                                geom_point(aes(colour = variable), alpha = 0.2) +
                                theme(strip.text = element_text(colour = 'white'), strip.background = element_rect(fill = 'black')) +
                                labs(x = '% Contribution', y = 'Return Category')
                        
                        print(s)

                })
                
        }
)