setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library('magrittr',lib='C:/Progra~1/R/R-3.1.3/library')
library('reshape2',lib='C:/Progra~1/R/R-3.1.3/library')
library('shiny')
library('rCharts',lib='C:/Progra~1/R/R-3.2.0/library')

#load dataset
all6<- read.csv('all6.csv')[,-1]
all6$Start.Date<- as.Date(all6$Start.Date)


#rising stars mplot rCharts
dup<- all6[,names(all6) %in% c('mlsto', 'Discipline')]
sub11<-all6[!all6$Tot.Invoiced==0 & !duplicated(dup) & all6$client.count>1,]

# need slope column
mods<- dlply(sub11, .(code.client), lm, formula= return.pdol ~ Start.Date)
coefs <- ldply(mods, coef)
colnames(coefs)[names(coefs) %in% 'Start.Date']<-'Risestar.coef'
#coefs %>% arrange(-Start.Date) %>% filter( !is.na(Start.Date)) %>% tail()
sub11a<- merge(sub11, coefs %>% select(code.client, Risestar.coef), by='code.client', all.x=TRUE)
sub11a<- sub11a %>% filter(!is.na(Risestar.coef)) %>% arrange(-Risestar.coef)
#reorder levels of code.client descending according to Risestar.coef
sub11a <- within(sub11a, 
                 code.client <- factor(code.client, 
                                       levels= sub11a[order(-sub11a$Risestar.coef),]$code.client %>% unique()))



#shiny function
pagerange = list(c(1,6),c(7,12),c(13,18))

shinyServer(
        function(input,output){
                
                # Compute the forumla text in a reactive function since it is 
                # shared by the output$caption and output$mpgPlot functions
                formulaText<- reactive({
                        paste("Rising performers in", input$business, sep=" ")
                })
                
                # Return the formula text for printing as a caption
                output$caption <- renderText( {
                        formulaText()
                })
                
                #check for the input variable
                data.r= reactive ({
                        if(input$business == 'All') {
                                posData<- sub11a}
                        
                        else{
                                posData<- sub11a %>% filter(Business == input$business)
                        }
                        return(posData)
                        
                })
        
                
                #subset data based on slider inputs for Date.

                data.r2 = reactive({
                        a = subset(data.r(), Start.Date >= input$date[1] & Start.Date <= input$date[2])
                        return(a)
                })
                
                #subset data based on 'page'.
                
                data.r3 = reactive({
                        #create list of unique client codes from subsetted dataframe above
                        client.list<- data.r2() %>% select(code.client) %>% unique()
                        #turn this data frame into a vector
                        client.list<- client.list[,1]
                        #subset client.list to match 'page'
                        client.list<- client.list[pagerange[[as.numeric(input$teehee)]][1]:
                                                          pagerange[[as.numeric(input$teehee)]][2]]
                        a = data.r2() %>% filter(code.client %in% client.list)
                        return(a)
                })

                
                #generate a plot of requested variable
                
                
                output$star.plot<- renderPlot( {

                                               
                        dd= data.r3()
                        
                        s= ggplot(dd, aes(x= Start.Date, y= return.pdol, colour=code.client)) + 
                                geom_smooth(method=lm, se=TRUE, aes(fill=code.client), alpha=.2) + 
                                geom_point(alpha= 0.5) +
                                theme(legend.position="right", axis.text.x=element_text(angle=45,hjust=1),
                                      text=element_text(size=12))+
                                facet_wrap(~ code.client) + #, labeller =  label_value(variable, c('a','b','c','d','e','f','g','h')) +
                                theme(strip.text = element_text(colour = 'white'), strip.background = element_rect(fill = 'black')) +
                                labs(x = 'Job Start Date', y = 'Return per dollar')
                        
                        print(s)

                })
                
        }
)