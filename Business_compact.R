# re-jig Business

str(all9a)

B.invest<- table(all9a$b.rpdol, all9a$Business) %>% t %>% as.data.frame.matrix()
colnames(B.invest) <- c("profit", "loss")
B.invest<- transform(B.invest, ratio = round(loss/profit, 2))
B.invest$JD <- rownames(B.invest) 
B.invest<- arrange(B.invest, ratio) %>% select(JD, profit, loss, ratio)

#group:

# university, school
# lawyer/body corp
# signs/manufacturer
# environment
# roads/rail/gov
# artist/landscape arch
# hospital/institution/health
# utilities
# engineer/water
# business/building services/internal
# builder
# architect
# town planner/developer
# resources
# person

# unknown should be NA


#code to compact:

all9a$test<- as.character(all9a$Business)
all9a$test <- ifelse(grepl("heritage|expert", all9a$Business), 
                     "heritage_expert", all9a$test)
all9a$test <- ifelse(grepl("wharf|bridges", all9a$Business), 
                     "wharf_bridge", all9a$test)
all9a$test <- ifelse(grepl("membrane|art|awning|facade", all9a$Business), 
                     "art_facade_awn_memb", all9a$test)
all9a$test <- ifelse(grepl("hosp|emerg|health|carpark", all9a$Business), 
                     "hosp_health_carpark", all9a$test)
all9a$test <- ifelse(grepl("sign|product", all9a$Business), 
                     "sign_product", all9a$test)
all9a$test <- ifelse(grepl("hotel|office|dining", all9a$Business), 
                     "hotel_office_dining", all9a$test)
all9a$test <- ifelse(grepl("report|investigation", all9a$Business), 
                     "report", all9a$test)
all9a$test <- ifelse(grepl("waste water|management", all9a$Business), 
                     "waste_wat_manage", all9a$test)
all9a$test <- ifelse(grepl("univ|commun|school|exten", all9a$Business), 
                     "edu_exten_community", all9a$test)
all9a$test <- ifelse(grepl("multi|house", all9a$Business), 
                     "residential", all9a$test)
all9a$test <- ifelse(grepl("flood|harvest|recycled", all9a$Business), 
                     "flood_h2o_harvest", all9a$test)
all9a$test <- ifelse(grepl("bldg|subdiv|sewer", all9a$Business), 
                     "civBldg_subdiv_sewer", all9a$test)
all9a$test <- ifelse(grepl("aged|erosion", all9a$Business), 
                     NA, all9a$test)

summary(as.factor(all9a$test))