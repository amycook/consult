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
                                 ))
                ),
                
                column(3,
                       
                       selectInput("jobtype", "Primary Job Type:",
                                   list("Building Structures" = "1. BuildingStructures", 
                                        "Land Infrastructure" = "2. LandInfrastructure", 
                                        "Environmental Planning" = "3. EnvironmentalPlanning",
                                        "Integrated Water Management" = "4. IntegratedWaterManagement",
                                        "Special Structures"= "5. Special Structures",
                                        "Other" = "Other")),
                       
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
                               ))
                       
                        
                ),
                
                column(3,
                       
                       selectInput("bulk.pos", "Bulk of Work by:",
                                   list("Director" = "Director", 
                                        "Senior Professional" = "Senior Professional", 
                                        "Mid Professional" = "Mid Professional",
                                        "Grad Professional" = "Grad Professional",
                                        "Senior Technical" = "Senior Technical",
                                        "Mid Technical" = "Mid Technical")),
                       

                       sliderInput("no.user", "Team Size:",
                                   min=1, max=6, value=1, step=1, round=0
                       )
                            
                       ),
                
                
                column(3,
                       
                       selectizeInput(
                               'JD.Second', 'Secondary Job Type:', 
                               choices = levels(all7$JD.Second),
                               options = list(
                                       placeholder = 'Please start typing',
                                       onInitialize = I('function() { this.setValue(""); }'),
                                       maxItems=1
                               )),
                       
                       sliderInput("inv.mlsto", "Approximate Fee:",
                                   min=500, max=250000, value=20000, step=500, round=2
                                   ),
                       
                       submitButton(text="Calculate")
                       
                       
                       )
                )),
                
        
        fluidRow(
                column(3,
                       wellPanel(
                       
                       # I() indicates it is raw JavaScript code that should be evaluated, instead
                       # of a normal character string
                       selectizeInput(
                               'code.client', 'Client:', choices = levels(all7$code.client),
                               options = list(
                                       placeholder = 'Please start typing',
                                       onInitialize = I('function() { this.setValue(""); }'),
                                       maxItems=1
                               )),
                       
                       selectInput("Biz.size", "Client size:",
                                   choices= levels(all7$Biz.size)),
                       
                       selectInput("Biz.type", "Client sector:",
                                   list("Government" = "gov", 
                                        "Not for Profit" = "NFP", 
                                        "Private" = "private",
                                        "Public" = "public"
                                        )),
                       
                       sliderInput("pc.pro", "% Hours by Professional:",
                                   min=0, max=100, value=50, step=5, round=0
                       ),
                       
                       submitButton(text="Calculate")                 
                       
                )
                ),
                
                column(9,
                       #main panel. show plot of each position and percentage within selected variable
                       tabsetPanel(type='tabs',
                                   tabPanel('Plot', plotOutput("fee.plot")),
                                   tabPanel('Similar Jobs', 
                                            titlePanel(""),
                                            
                                            wellPanel(
                                            fluidRow(
                                                    column(6,
                                                           numericInput('case', 'Demo job- range: 1-2321', 80, min=1, max=2321),
                                                           numericInput('ks','Number of similar jobs:', 4, min=1, max=8)
                                                                   
                                                           ),
                                                    
                                                    column(6,
                                                           selectInput("Column", "Narrow search by:",
                                                                       list('none'='NA',
                                                                            "Client" = "code.client", 
                                                                            "Contact" = "code.contact", 
                                                                            "Billing Type" = "Billing.Type",
                                                                            "Discipline" = "Discipline",
                                                                            'Business' = 'Business'
                                                                       )),
                                                           submitButton(text="Update")
                                                           )
                                                    
                                                    )),
                                            
                                            fluidRow(
                                                    tableOutput('knn.table')
                                                    )
                                   )
          
                       )
                )
                                  
)))



