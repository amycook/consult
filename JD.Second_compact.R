
# re-jog JD.Second and Business

str(all9a)

JD.invest<- table(all9a$b.rpdol, all9a$JD.Second) %>% t %>% as.data.frame.matrix()
colnames(JD.invest) <- c("profit", "loss")
JD.invest<- transform(JD.invest, ratio = round(loss/profit, 2))
JD.invest$JD <- rownames(JD.invest) 
JD.invest<- arrange(JD.invest, ratio) %>% select(JD, profit, loss, ratio)

#group:

# heritage/ expert opinion
# wharf/port, bridges
# membrane shade structure/art/awning/facade
# hospital/emergency services/health/carpark
# refurb/renovation
# sign, product development
# hotel/resort, offices, dining/retail
# report/investigation
# waste water/water management
# university/school/community/extension
# multi-dwelling/house
# industrial
# flood studies, water harvesting, recycled water
# parks and open spaces
# civil works to bldg, Subdivision, sewerage



# delete - aged care, erosion and sediment

#code to compact:

all9a$test<- as.character(all9a$JD.Second)
all9a$test <- ifelse(grepl("heritage|expert", all9a$JD.Second), 
                          "heritage_expert", all9a$test)
all9a$test <- ifelse(grepl("wharf|bridges", all9a$JD.Second), 
                          "wharf_bridge", all9a$test)
all9a$test <- ifelse(grepl("membrane|art|awning|facade", all9a$JD.Second), 
                          "art_facade_awn_memb", all9a$test)
all9a$test <- ifelse(grepl("hosp|emerg|health|carpark", all9a$JD.Second), 
                          "hosp_health_carpark", all9a$test)
all9a$test <- ifelse(grepl("sign|product", all9a$JD.Second), 
                          "sign_product", all9a$test)
all9a$test <- ifelse(grepl("hotel|office|dining", all9a$JD.Second), 
                          "hotel_office_dining", all9a$test)
all9a$test <- ifelse(grepl("report|investigation", all9a$JD.Second), 
                          "report", all9a$test)
all9a$test <- ifelse(grepl("waste water|management", all9a$JD.Second), 
                          "waste_wat_manage", all9a$test)
all9a$test <- ifelse(grepl("univ|commun|school|exten", all9a$JD.Second), 
                          "edu_exten_community", all9a$test)
all9a$test <- ifelse(grepl("multi|house", all9a$JD.Second), 
                          "residential", all9a$test)
all9a$test <- ifelse(grepl("flood|harvest|recycled", all9a$JD.Second), 
                          "flood_h2o_harvest", all9a$test)
all9a$test <- ifelse(grepl("bldg|subdiv|sewer", all9a$JD.Second), 
                          "civBldg_subdiv_sewer", all9a$test)
all9a$test <- ifelse(grepl("aged|erosion", all9a$JD.Second), 
                          NA, all9a$test)

summary(as.factor(all9a$test))






