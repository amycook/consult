# setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
# setwd("~/OneDrive/shared files/Bligh Tanner/masters/data")
# library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
# library("dplyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
# library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
# library('magrittr',lib='C:/Progra~1/R/R-3.1.3/library')
# library('reshape2',lib='C:/Progra~1/R/R-3.1.3/library')
# library('shiny')


# month.a<- read.csv('monthly.csv')[,-1]
# month.a$Date<- as.Date(month.a$Date)
# month.a$return.cut <- cut_number(month.a$return.mon,15)
levels(month.a$return.cut) =c("< -$0.51", "-$0.51 to -$0.24", "-$0.24 to -$0.02","-$0.02 to $0.18",
                              "$0.18 to $0.33", "$0.33 to $0.45", "$0.45 to $0.50", "$0.50 to $0.61",
                              "$0.61 to $0.75", "$0.75 to $0.92", "$0.92 to $1.20", "$1.20 to $1.68", 
                              "$1.68 to $2.47", "$2.47 to $5.24", "> $5.24")

#shiny function

shinyServer(
        function(input,output){
                
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
                
                
                gv<- reactive( {

                                               
                        dd= data.r2()
                        
                        dd %>% ggvis(~return.cut, ~input$y, fill=input$variable) %>%
                          layer_bars(stack=TRUE) %>%
                          bind_shiny('pos.plot')
                          

                })
                
        }
)