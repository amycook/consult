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
        headerPanel("Rising Stars"),
        
        # Sidebar with controls to select the variable to plot against positions
        sidebarPanel(
                selectInput("business", "Client Industry:",
                            list("Architect" = "architect", 
                                 "Artist" = "artist", 
                                 "Builder" = "builder",
                                 "Building Services"= 'building services',
                                 "Business" = "business",
                                 "Council" = 'council',
                                 "Developer/Real Estate"= 'developer/real estate',
                                 "Engineer"= 'engineer',
                                 "Environment" = 'environment',
                                 "Friend" = 'friend',
                                 "Government"='gov',
                                 "Health"= 'health',
                                 "Hospital" = 'hospital',
                                 "Institution" = 'institution',
                                 "Interior Design"= 'interior design',
                                 "Internal" = 'internal',
                                 "Landscape Architect" = 'landscape arch',
                                 "Solicitor"= 'lawyer',
                                 "Manufacturer/Supplier"='manufacturer/supplier',
                                 'Membrane Fabricator'= 'membrane fabricator',
                                 "Person" ='person',
                                 "Resources"= 'resources',
                                 "Roads/Rail"= 'roads/rail',
                                 "School"='school',
                                 "signs"='signs',
                                 "Town Planner"= 'town planner',
                                 "University"= 'university',
                                 'Utilities Provider'= 'utilities provider',
                                 "Water"= 'water',
                                 "All"= "All")),
                
                dateRangeInput('date', 'Date', start = '2003-10-01', end = NULL, min = '2003-10-01', max = '2014-12-15', 
                               format = "dd-mm-yyyy", startview = "month"),
                
                selectInput("teehee", "Page:",
                            list("1"= 1,
                                 "2"= 2,
                                 "3"= 3)),
                
                submitButton(text="Update")
                
        ),
        
        #main panel. show plot of each position and percentage within selected variable
        mainPanel(
                h3(textOutput("caption")),
                
                plotOutput("star.plot")
                
        )
        
                          
))