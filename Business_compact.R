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
# environment/water
# roads/rail/gov/util
# artist/landscape arch
# hospital/institution/health
# utilities
# engineer
# water
# business/building services/internal
# builder
# architect
# townplanner
# developer/realestate
# resources
# person



#code to compact:

all9a$test<- as.character(all9a$Business)
all9a$test <- ifelse(grepl("lawyer|body", all9a$Business), 
                     "lawyer_bodyC", all9a$test)
all9a$test <- ifelse(grepl("univ|school", all9a$Business), 
                     "uni_school", all9a$test)
all9a$test <- ifelse(grepl("sign|manuf", all9a$Business), 
                     "sign_manufac", all9a$test)
all9a$test <- ifelse(grepl("environ|water", all9a$Business), 
                     "enviro_water", all9a$test)
all9a$test <- ifelse(grepl("road|gov|util", all9a$Business), 
                     "road_rail_gov_util", all9a$test)
all9a$test <- ifelse(grepl("artist|landscape", all9a$Business), 
                     "artist_landarch", all9a$test)
all9a$test <- ifelse(grepl("hospit|institut|health", all9a$Business), 
                     "hosp_institut_health", all9a$test)
all9a$test <- ifelse(grepl("business|building|internal", all9a$Business), 
                     "biz_bldgserv_internal", all9a$test)

summary(as.factor(all9a$test))



