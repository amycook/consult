# setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
# library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
# library("dplyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
# library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
# library('magrittr',lib='C:/Progra~1/R/R-3.1.3/library')
# library('reshape2',lib='C:/Progra~1/R/R-3.1.3/library')
# library('shiny')

shinyUI(fluidPage(
        theme=shinytheme("journal"),
        
        #Application title
        titlePanel("Quote Assist"),
        
        # Sidebar with controls to select the variable to plot against positions
        wellPanel(
        fluidRow(
                column(3,
        
                selectInput("disc", "Discipline:",
                            list("Civil" = "Civil", 
                                 "Structural" = "Structural", 
                                 "Environmental Planning" = "Environmental Planning",
                                 "Water" = "Water",
                                 "All"= "All")),
                
                selectInput("billing", "Billing Type:",
                            list('Hourly Rate' = 'Hourly Rate',
                                 "Fixed Fee"= 'Fixed Quote'
                                 )),
                
                selectizeInput(
                        'JD.Second', 'Job Type:', 
                        choices = levels(all7$JD.Second),
                        options = list(
                                placeholder = 'Please start typing',
                                onInitialize = I('function() { this.setValue(""); }'),
                                maxItems=1
                        ))
                
                ),
                
                column(3,
                       
                       selectizeInput(
                               'code.client', 'Client:', choices = levels(all7$code.client),
                               options = list(
                                       placeholder = 'Please start typing',
                                       onInitialize = I('function() { this.setValue(""); }'),
                                       maxItems=1
                               )),
                       
                       selectizeInput(
                               'Business', 'Client Industry:', 
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
                                    "All"= "All"),
                               options = list(
                                       placeholder = 'Please start typing',
                                       onInitialize = I('function() { this.setValue(""); }'),
                                       maxItems=1
                               )),
                       
                       selectInput("Biz.type", "Client sector:",
                                   list("Government" = "gov", 
                                        "Not for Profit" = "NFP", 
                                        "Private" = "private",
                                        "Public" = "public"
                                   ))

                ),
                
                column(3,
                       
                       selectInput("majority.pos", "Main Contributor of Time:",
                                   list("Director" = "Director", 
                                        "Senior Professional" = "Senior Professional", 
                                        "Mid Professional" = "Mid Professional",
                                        "Grad Professional" = "Grad Professional",
                                        "Senior Technical" = "Senior Technical",
                                        "Mid Technical" = "Mid Technical")),
                       
                       sliderInput(
                               'pc.pro', '% Hours by Main Employee:', 
                               min=0, max=100, value=50, step=.5, round=0),

                       sliderInput("no.user", "Team Size:",
                                   min=1, max=6, value=1, step=1, round=0
                       )
                            
                       ),
                
                
                column(3,
                       
                       sliderInput("timespan", "Timespan (days):",
                                   min=0, max=1000, value=60, step=20, round=0
                       ),
                       
                       sliderInput("inv.mlsto", "Approximate Fee:",
                                   min=500, max=250000, value=20000, step=500, round=2
                                   ),
                       
                       submitButton(text="Calculate")
                       
                       
                       )
                )),
                
        
        fluidRow(

                column(12,
                       #main panel. show plot of each position and percentage within selected variable
                       tabsetPanel(type='tabs',
                                   tabPanel('Analysis',
                                           
                                            tags$div(
                                                    HTML(paste(tags$span(style="color:white; font-size: 32px", "red"), sep = ""))
                                            ), 
                                             
                                           fluidRow(
                                                   
                                           column(1,
                                                  h1("")
                                           ),
                                           
                                           column(11,
                                           plotOutput("fee.plot")
                                           )
                                           ),
                                           
                                           tabsetPanel(type = 'tabs',
                                                       
                                                       tabPanel('Timespan',
                                                                img(src = "timespan-1.png", height = "600px")
                                                                ),
                                                       
                                                       tabPanel('Team Size',
                                                                img(src = "no_users-1.png", height = "600px")
                                                                ),
                                                       
                                                       tabPanel('Total Invoiced',
                                                                img(src = "invoiced-1.png", height = "600px")
                                                                ),
                                                       
                                                       tabPanel('% Professional Hours',
                                                                img(src = "pc_pro-1.png", height = "600px")
                                                                )
                                                       )
                                           
                                           ),
                                           
                                   tabPanel('Similar Jobs', 
                                            titlePanel(""),
                                            
                                            wellPanel(
                                            fluidRow(
                                                    column(6,
                                                           numericInput('ks','Number of similar jobs:', 8, min=2, max=12),
                                                           selectInput("column", "Narrow search by:",
                                                                       list('none'='NA',
                                                                            "Client" = "code.client", 
                                                                            "Contact" = "code.contact", 
                                                                            "Billing Type" = "Billing.Type",
                                                                            "Discipline" = "Discipline",
                                                                            'Business' = 'Business',
                                                                            'Job Type'='JD.Second'
                                                                       ))
                                                    ),
                                                    
                                                    column(6,
                                                           selectInput("size", "Size points by:",
                                                                       list('Timespan (days)' = 'timespan',
                                                                            'highest % by employee' = 'pc.majpos')),
                                                           
                                                           selectInput("colour", "Colour points by:",
                                                                       list('Client' = 'code.client',
                                                                            'Majority Employee'='majority.pos',
                                                                            'Job Type' = 'JD.Second'
                                                                            )),
                                                           submitButton()
                                                           )
                                                    
                                                    )),
                                            
                                            fluidRow(
                                                    column(1,
                                                           h1("")
                                                    ),
                                                    
                                                    column(11,
                                                           plotOutput('knn.plot', height = "100%", inline = TRUE),
                                                           h4('Job Details'),
                                                           tableOutput('knn.table'),
                                                           h4('Finances'),
                                                           tableOutput('knn.table1'),
                                                           h4('Client Details'),
                                                           tableOutput('knn.table2'),
                                                           h4('Staff Details'),
                                                           tableOutput('knn.table3'))
                                                    
                                                    )
                                   )
          
                       )
                )
                                  
)))



