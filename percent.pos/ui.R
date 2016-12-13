setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library('magrittr',lib='C:/Progra~1/R/R-3.1.3/library')
library('reshape2',lib='C:/Progra~1/R/R-3.1.3/library')
library('shiny')

shinyUI(pageWithSidebar(
        #theme="amelia.css",
        
        #Application title
        headerPanel("Team Ratios"),
        
        # Sidebar with controls to select the variable to plot against positions
        sidebarPanel(
                selectInput("disc", "Discipline:",
                            list("Civil" = "Civil", 
                                 "Structural" = "Structural", 
                                 "Environmental Planning" = "Environmental Planning",
                                 "Water" = "Water",
                                 "All"= "All"))
                
        ),
        
        #main panel. show plot of each position and percentage within selected variable
        mainPanel(
                h3(textOutput("caption")),
                
                plotOutput("pos.plot")
                
        )
        
                          
))