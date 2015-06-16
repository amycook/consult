# setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
# library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
# library("dplyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
# library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
# library('magrittr',lib='C:/Progra~1/R/R-3.1.3/library')
# library('reshape2',lib='C:/Progra~1/R/R-3.1.3/library')
# library('shiny')

shinyUI(pageWithSidebar(
        #theme="amelia.css",
        
        #Application title
        headerPanel("Monthly Summary"),
        
        # Sidebar with controls to select the variable to plot against positions
        sidebarPanel(
                
                selectInput("disc", "Discipline:",
                            list("Civil" = "Civil", 
                                 "Structural" = "Structural", 
                                 "Environmental Planning" = "Environmental Planning",
                                 "Water" = "Water",
                                 "All"= "All")),
                
                selectInput("variable", "Fill Variable:",
                            list('Client Sector' = 'Biz.type',
                                 "Job Number Code"= 'code.jobnum',
                                 "Total Timespan" = "timespan.cut", 
                                 "Client Code" = "code.client",
                                 "Director Code" = "code.director",
                                 "Project Engineer Code"= "code.ProjEng",
                                 "Billing Type"= "Billing.Type",
                                 "Discipline"= "Discipline",
                                 "Primary Job Type"= 'JD.Primary',
                                 "Client Count"= 'clientcount.cut',
                                 "Business"= 'Business',
                                 "Client no. Employees" ='clientemps.cut',
                                 'Client Size' = 'Biz.size',
                                 'Secondary Job Type'='JD.Second',
                                 'Job Type'='Type')),
                
                dateRangeInput('date', 'Date', start = '2008-12-31', end = '2009-01-31', min = '2008-12-31', max = '2014-12-15', 
                               format = "dd-mm-yyyy", startview = "month"),
                
                selectInput("y", "Y Axis:",
                            list("Invoiced Amount" = "Invoiced.Amount", 
                                 "Balance (invoiced-costs)" = "bal.mon")),
                
                submitButton(text="Update")
                
        ),
        
        #main panel. show plot of each position and percentage within selected variable
        mainPanel(
                h3(textOutput("caption")),
                
                ggvisOutput("pos.plot")
                
        )
        
                          
))