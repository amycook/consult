# setwd("~/OneDrive/shared files/Bligh Tanner/masters/data")
library('ggplot2')
library("dplyr")
library("plyr")
library('magrittr')
library('reshape2')
library('shiny')



# month.a<- read.csv('monthly.csv')
month.a$Date<- as.Date(month.a$Date)

levels(month.a$return.cut) =c("< -&#xfe69;0.51", "-&#xfe69;0.51 to -&#xfe69;0.24", "-&#xfe69;0.24 to -&#xfe69;0.02","-&#xfe69;0.02 to &#xfe69;0.18",
                              "&#xfe69;0.18 to &#xfe69;0.33", "&#xfe69;0.33 to &#xfe69;0.45", "&#xfe69;0.45 to &#xfe69;0.50", "&#xfe69;0.50 to &#xfe69;0.61",
                              "&#xfe69;0.61 to &#xfe69;0.75", "&#xfe69;0.75 to &#xfe69;0.92", "&#xfe69;0.92 to &#xfe69;1.20", "&#xfe69;1.20 to &#xfe69;1.68", 
                              "&#xfe69;1.68 to &#xfe69;2.47", "&#xfe69;2.47 to &#xfe69;5.24", "> &#xfe69;5.24")
#order month.a according to return.cut levels
month.a<- month.a %>% arrange(return.cut)




#                         gg$layout<- list(fileopt='overwrite', filename='R-Cookbook/axes/reversed ordered axes - 1')

#shiny function

shinyServer(
        function(input,output, session){
                
                # Compute the forumla text in a reactive function since it is 
                # shared by the output$caption and output$mpgPlot functions
                formulaText<- reactive({
                        paste("Monthly Summary", input$disc, sep=" ")
                })
                
                # Return the formula text for printing as a caption
                output$caption <- renderText( {
                        formulaText()
                })
                
                #check for the input variable
                data.r= reactive ({
                        if(input$disc == 'All') {
                                posData<- month.a}
                        
                        else{
                                posData<- month.a %>% filter(Discipline == input$disc)
                        }
                        
                })
        
                #subset data based on slider inputs for Date.

                data.r2 = reactive({
                        a = subset(data.r(), Date >= input$date[1] & Date <= input$date[2])
                        return(a)
                })

                #generate a plot of requested variable
                
                
                output$pos.plot<- renderGraph( {

                                               
                        dd= data.r2()
                        dd<-dd %>% arrange(return.cut)
                                                      
                        s<- ggplot(dd, aes_string(x='return.cut', y= input$y, fill= input$variable)) +
                                geom_bar(stat='identity')+
                                theme(legend.position="right", axis.text.x=element_text(angle=45,hjust=1),
                                      text=element_text(size=12)) +
                                labs(x = 'Return per Dollar') 
                        
                        
                        # convert from ggplot->plotly
                        gg <- gg2list(s)
                        
#                         gg$layout<- list(hovermode='closest')
#                         gg$layout$xaxis <- list(tickangle = -45, autorange= FALSE)
#                         gg$layout$barmode <- 'stack'
                        
                        
                        
                        # Send this message up to the browser client, which will get fed through to
                        # Plotly's javascript graphing library embedded inside the graph
                        return(list(
                          list(
                            id = "pos.plot",
                            task = "newPlot",
                            data = gg$data,
                            layout= gg$layout
                          )
                        ))







                })
                
        }
)

