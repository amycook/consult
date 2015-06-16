 setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library('magrittr',lib='C:/Progra~1/R/R-3.1.3/library')
year13<-read.csv('2013projectdata.csv')
year14<-read.csv('2014projectdata.csv',
                 na.strings=c(""," ","Number of Blue Files","JobInvCount","Total Costs (AUD)",
                                                    "Timesheet Hours","Timesheet Total Charge (AUD)",
                                                    "Timesheet Total Cost (AUD)","Total Disbursements (AUD)",
                                                    "Total Fee (AUD)","Total QB Invoiced (AUD)",
                                                    "Total Sub-Contractors (AUD)","Project Value (AUD)",
                              "Job Number","NA"))
year02to12<-read.csv('project_data_all.csv',
                     na.strings=c(""," ","Number of Blue Files","JobInvCount","Total Costs (AUD)",
                                  "Timesheet Hours","Timesheet Total Charge (AUD)",
                                  "Timesheet Total Cost (AUD)","Total Disbursements (AUD)",
                                  "Total Fee (AUD)","Total QB Invoiced (AUD)",
                                  "Total Sub-Contractors (AUD)","Project Value (AUD)","Job Number","NA"))
#delete columns
year13<-year13[,-c(4,5)]
year13<-year13[,-c(15:17)]
year13<-year13[,-c(25:28)]
year13<-year13[,-c(37:38)]
head(year13)
dim(year14)
dim(year02to12)
#create separate file for hours of file year02to12
hours0212<-year02to12[,c(1,41:52)]
year02to12<-year02to12[,-c(41:52)]
write.csv(hours0212,file='hours0212.csv')
c.names<-data.frame("13"=colnames(year13)[1:40],"14"=colnames(year14)[1:40],
                    "all"=colnames(year02to12)[1:40])
c.names


#year 2013 has the job number printed for the first row of each job group only so must concatenate these 
#in a special way....
year13<-unique(year13)
head(year13,10)

#remove blank rows
write.csv(year13,'BT13.csv')
year13<-read.csv('BT13.csv',na.strings=c(""," ","Number of Blue Files","JobInvCount","Total Costs (AUD)",
                                         "Timesheet Hours","Timesheet Total Charge (AUD)",
                                         "Timesheet Total Cost (AUD)","Total Disbursements (AUD)",
                                         "Total Fee (AUD)","Total QB Invoiced (AUD)",
                                         "Total Sub-Contractors (AUD)","Project Value (AUD)","NA"))
year13<-year13[,-1]
year13<-year13[rowSums(is.na(year13))!=ncol(year13),]
year13$test<-rowSums(is.na(year13))>36
year13<-year13[year13$test==FALSE,]
#remove rows with NA in first column
yar13<-year13[!is.na(year13[,1]),]
head(yar13[,1:5],20)
year13<-yar13
year13<-year13[,-41]

#year13 is clean..
#unique rows for the other files
year14<-unique(year14)
year02to12<-unique(year02to12)
head(year02to12)
#delete rows of NA in year14 and year02to12
year14<-year14[rowSums(is.na(year14))!=ncol(year14),]
year02to12<-year02to12[rowSums(is.na(year02to12))!=ncol(year02to12),]

#have a look at barplots for all the variables
bar.plots<-function(df){
        nm=colnames(df)
        for(i in seq_along(nm)){
                plots<-ggplot(df,aes_string(x=nm[i]))+
                        geom_bar()+
                        theme(axis.text.x=element_text(angle=45,hjust=1))
                ggsave(plots,filename=paste("barplot",nm[i],".png",sep=""),
                       path="C:/Users/n9232371/Documents/Consultbusiness/barplots")
                
                cat(i," ")
        }
}

#rbind all the years
#correct name of Year13 'Job.Type..Primary'
colnames(year13)[22]<-'Job.Type.Primary'
#find class of all current variables
a<-data.frame(sapply(year13,class))
b<-data.frame(sapply(year14,class))
c<-data.frame(sapply(year02to12,class))
class.combo<-data.frame("year13"=a,"year14"=b,"year02to12"=c)

#convert all non-factors to the same type
year02to12$Post.Code<-as.factor(year02to12$Post.Code)
year14$Total.Costs..AUD.<-as.numeric(year14$Total.Costs..AUD.)
year13$Tmesheet.Total.Cost..AUD.<-as.numeric(year13$Tmesheet.Total.Cost..AUD.)
year14$Total.Fee..AUD.<-as.numeric(year14$Total.Fee..AUD.)
year02to12$Total.Fee..AUD.<-as.numeric(year02to12$Total.Fee..AUD.)
year14$Job.Size<-as.factor(year14$Job.Size)
year14$Project.Value..AUD.<-as.numeric(year14$Project.Value..AUD.)
year02to12$Project.Value..AUD.<-as.numeric(year02to12$Project.Value..AUD.)
#convert all factor levels to character for rbind. can't add new levels in rbind.. 
year13[,c(1:4,6:17,19:23,25:30,38,39)] <- lapply(year13[,c(1:4,6:17,19:23,25:30,38,39)],as.character) 
year14[,c(1:4,6:17,19:23,25:30,38,39)] <- lapply(year14[,c(1:4,6:17,19:23,25:30,38,39)],as.character)
year02to12[,c(1:4,6:17,19:23,25:30,38,39)] <- lapply(year02to12[,c(1:4,6:17,19:23,25:30,38,39)],as.character)
all<-rbind(year14,year13,year02to12)
str(all)

all[,c(1:4,6:17,19:23,25:30,38,39)] <- lapply(all[,c(1:4,6:17,19:23,25:30,38,39)],as.factor) 
str(all)

#change column names
colnames(all)[5]<-'No.Blue.Files'
colnames(all)[6]<-'Role'
colnames(all)[10]<-'Paperless'
colnames(all)[28]<-'Folders'
colnames(all)[30]<-'Engineer.2'
colnames(all)[32]<-'Tot.Timesheet.Charge'
colnames(all)[33]<-'Tot.Timesheet.Cost'
colnames(all)[34]<-'Total.Fee'
colnames(all)[35]<-'Disbursements'
colnames(all)[36]<-'Tot.Invoiced'
colnames(all)[37]<-'Tot.SubCon.Fee'
colnames(all)[40]<-'Project.Value'
rownames(all)<-c(1:dim(all)[1])

#### CLEANING WRT NA VALUES###

#have a look at rows with 'NA' in Job.Number
rowShift <- function(x, shiftLen = 1L) {
        r <- (1L + shiftLen):(length(x) + shiftLen)
        r[r<1] <- NA
        return(x[r])
}
all$jobno.shift<-rowShift(all$Job.Number,-1)
all$jobname.shift<-rowShift(all$Job.Name,-1)
head(all$Job.Number)
head(all$jobno.shift)
all[is.na(all$Job.Number),names(all) %in% c('Job.Number','Role','jobno.shift')]
extra.info<-all[is.na(all$Job.Number),names(all) %in% c('Job.Number','Role','jobno.shift','jobname.shift')]
write.csv(extra.info,'extra_info.csv')

#remove rows with NA in Job.Number and last two additional columns
all<-all[!is.na(all[,1]),]
all<-all[,!(names(all) %in% c('jobno.shift','jobname.shift'))]

#find repeated job numbers
dim(all)
length(unique(all$Job.Number))
#sort(table(all$Job.Number))
#2013.190.3 has two cases..
#all[all$Job.Number=='2013.190.300',]    #one dollar different under timesheet cost. delete row with $279 timesheet cost
del<-rownames(all[all$Tot.Timesheet.Cost==279 & all$Job.Number=='2013.190.300',])
all<-all[!(rownames(all)) %in% del,]
all<-all[!(all$Job.Number=='Job Number'),]
#job with negative invoicing
all[all$Tot.Invoiced<0,]
#check SAGE to correct total invoiced amount
all[all$Job.Number=='2012.039.300',]$Tot.Invoiced<-58800
all[all$Job.Number=='2012.160.300',]$Tot.Invoiced<-20000
#third case is completely written off job. This is an outlier, will remove after engineering variables.

#progressive save
write.csv(all,'temp_all.csv')
all<-read.csv('temp_all.csv')

#line of NAs?
#all[(all$Tot.Invoiced=='NA'),]
#all[is.na(all[,1]),]



###ENGINEERED VARIABLES### 

#year of job
all$Job.Number<-as.character(all$Job.Number)
strsplit(all$Job.Number[1],split='[.]')[[1]][1]
all$Year<-sapply(all$Job.Number,FUN=function(x){strsplit(x,split='[.]')[[1]][1]})
#timespan of job
#need to extract end date from 'hours' data.

#profit. simply invoiced - cost - disbursements - SubCon.Fee
tmp<-all[,names(all) %in% c('Tot.Invoiced','Tot.Timesheet.Cost','Disbursements','Tot.SubCon.Fee')]
tmp$profit<-apply(tmp,1, function(x) x[3]-x[2]-x[1]-x[4])
head(tmp)
all$Profit<-tmp$profit
summary(all$Profit)
#sometimes the 'cost' is way too low compared to hours spent. make a column for cost/timesheet hours.
colnames(all)[colnames(all) %in% 'Tot.Timesheet.Charge']<-'Tot.TS.Charge'
colnames(all)[colnames(all) %in% 'Tot.Timesheet.Cost']<-'Tot.TS.Cost'
colnames(all)[colnames(all) %in% 'Timesheet.Hours']<-'TS.Hours'
colnames(all)[colnames(all) %in% 'Tot.SubCon.Fee']<-'Subcon.fee'
colnames(all)[colnames(all) %in% 'Disbursements']<-'Disburse'
low.cost<- transform(all, Profit.check= ifelse(TS.Hours==0, 0, Tot.TS.Cost/TS.Hours))
nrow(low.cost)
#investigate cost/hr less than 50
low.cost<- low.cost[low.cost$Profit.check<50,]

#really hard to fix. Easier to count timesheethoursxcost in invoice cleaning!!! and do a check that way.


###look at rows with Timesheet.Hours<.5
no.win.0invoice.lowTS<-all[all$Timesheet.Hours<15 & all$Tot.Invoiced<0.5 & all$Total.Fee==0,]
no.win.0invoice.highTS<-all[all$Timesheet.Hours>=15 & all$Tot.Invoiced<0.5,]
no.win.0invoice.setfee<-all[all$Tot.Invoiced<0.5 & all$Total.Fee>0,]
#bar.plots(no.win.0invoice.lowTS)

#send third negative invoice to outlier.csv
write.csv(all[all$Job.Number=='2013.026.200',],'outlier.csv')


# all[all$Client.Contact %in% 'Shane Spargo',]


### CREATE list of clients and client contacts and client characteristics - unique
client<-data.frame('client'= all$Client.Company, 'client.contact'= all$Client.Contact)
#separate C/- 
client$client<-as.character(client$client)
#rownames of $client column has 'C/-' in the phrase.
RN.careof<-grep('C/-', client$client)
head(RN.careof)
#in new mini data frame split up terms using 'C/-' and create two sep columns
careof<-client$client[RN.careof]
careof<-as.data.frame(careof)
careof$careof<-as.character(careof$careof)
strsplit(careof$careof[1],split='C/-')[[1]]
careof$caredfor<-sapply(careof$careof,FUN=function(x){strsplit(x,split='C/-')[[1]][1]})
careof$caredby<-sapply(careof$careof,FUN=function(x){strsplit(x,split='C/-')[[1]][2]})
rownames(careof)=RN.careof
#merge 'care.of' into client df
colnames(careof)[names(careof) %in% 'careof']<-'client'
careof$caredfor<- sub("\\s+$", "", careof$caredfor)
careof$caredby <- sub("^\\s+", "", careof$caredby)
client$client2<- client$client
client$client2[RN.careof]<- careof$caredby
client$caredfor<- c(rep(NA, nrow(client)))
client$caredfor[RN.careof]<- careof$caredfor
#output data frame of unique clients and client contacts

#do some basic cleaning/re-labelling of client names first.
client$client2<- as.factor(client$client2)
levels(client$client2)[levels(client$client2)=="All Hallows' School C/ Fulton Trotter Architects"] <- "Fulton Trotter"
levels(client$client2)[levels(client$client2)=="Architectural Practice Acade"] <- "Architectural Practice Academy"
levels(client$client2)[levels(client$client2)=="Axis capital"] <- "Axis Capital"
levels(client$client2)[levels(client$client2)=="Arkhefeld"] <- "Arkhefield"
levels(client$client2)[levels(client$client2)=="Bark Design Architects"] <- "Bark Design"
levels(client$client2)[levels(client$client2)=="Bickerton Masters Architects"] <- "Bickerton Masters Architecture"
levels(client$client2)[levels(client$client2)=="Birchall & Partners Architects"] <- "Birchall & Partners Architects"
levels(client$client2)[levels(client$client2)=="BMT-WBM Melbourne Office"] <- "BMT WBM Pty Ltd"
levels(client$client2)[levels(client$client2)=="BMT WBM"] <- "BMT WBM Pty Ltd"
levels(client$client2)[levels(client$client2)=="Brand & Slater Arc"] <- "Brand and Slater"
levels(client$client2)[levels(client$client2)=="Brand & Slater Architects"] <- "Brand and Slater"
levels(client$client2)[levels(client$client2)=="Brand & Slater"] <- "Brand and Slater"
levels(client$client2)[levels(client$client2)=="Brand and Slater Architects"] <- "Brand and Slater"
levels(client$client2)[levels(client$client2)=="BMT-WBM Melbourne Office"] <- "BMT WBM Pty Ltd"
levels(client$client2)[levels(client$client2)=="Brown Consulting (ACT) Pty Ltd"] <- "Brown Consulting (QLD) Pty Ltd"
levels(client$client2)[levels(client$client2)=="Bud Brannigan Architec"] <- "Bud Brannigan Architects"
levels(client$client2)[levels(client$client2)=="CGRAM & W"] <- "CGRAMW"
levels(client$client2)[levels(client$client2) %in% c("Conrad Cargett Riddel",
                                                 'Conrad Gargett Riddell','DETE C/o Conrad Gargett Riddel')] <-
        "Conrad Gargett Riddel"
levels(client$client2)[levels(client$client2) %in% c("Conrad Gargett Riddel Ancher Mortlock Woolley",
                                                     'Conrad Gargett Riddel Mortloch Woolley')] <- "CGRAMW"
levels(client$client2)[levels(client$client2)=="Department of Transport and Main Roads Townsville"] <-
        "Department of Transport and Main Roads"
levels(client$client2)[levels(client$client2)=="Gabriel and Elizabeth Poole Design"] <- "Gabriel & Elizabeth Poole Design Company"
levels(client$client2)[levels(client$client2)=="Golder Associates Pty Ltd - Sydney"] <- "Golder Associates Pty Ltd"
levels(client$client2)[levels(client$client2)=="Guppy & Associates"] <- "Guppy and Associates"
levels(client$client2)[levels(client$client2)=="HH Tan Architects"] <- "HH Tan Architects Pty Ltd"
levels(client$client2)[levels(client$client2)=="Indigo Projects"] <- "Indigo Group"
levels(client$client2)[levels(client$client2)=="Ivan McDonald Architect"] <- "Ivan McDonald Architects"
levels(client$client2)[levels(client$client2)=="JFE Engineering Corporation"] <- "JFE Engineering Australia Pty Ltd"
levels(client$client2)[levels(client$client2)=="LandPartners Ltd"] <- "Landpartners Ltd"
levels(client$client2)[levels(client$client2)=="LVO Architecure"] <- "LVO Architecture"
levels(client$client2)[levels(client$client2)=="Michael Buzolic"] <- "Michael Buzolic Architect"
levels(client$client2)[levels(client$client2)=="Michael Ford and Loreta Portelli"] <- "Michael Ford"
levels(client$client2)[levels(client$client2)%in% 
                               c("Moreton Bay Regional Council - Caboolture Office",
                                 'Moreton Bay Regional Council - Redcliffe Office')] <- "Moreton Bay Regional Council"
levels(client$client2)[levels(client$client2)=="Mount Cotton Constructions"] <- "Mount Cotton Construction"
levels(client$client2)[levels(client$client2)=="Neylan Architecture"] <- "Neylan Architects"
levels(client$client2)[levels(client$client2)=="Peddle Thorp"] <- "Peddle Thorp Architects"
levels(client$client2)[levels(client$client2)%in% 
                               c("Philips Smith Conwell Architects",
                                 'Phillips Smith Conwell Architects Pty Ltd')] <- "Phillips Smith Conwell"
levels(client$client2)[levels(client$client2)=="Qbuild - Brisbane Metropolitan"] <- "Qbuild"
levels(client$client2)[levels(client$client2)=="Riddell Architecture"] <- "Riddel Architecture"
levels(client$client2)[levels(client$client2)=="Rohrig"] <- "Rohrig Constructions"
levels(client$client2)[levels(client$client2)%in% 
                               c("Sunshine Coast Regional Council - Maroochydore Office",
                                 'Sunshine Coast Regional Council - Caloundra Office')] <- "Sunshine Coast Regional Council"
levels(client$client2)[levels(client$client2)=="Thomson Adsett"] <- "Thomson Adsett Architects"
levels(client$client2)[levels(client$client2)=="Tract Consultants Pty Ltd"] <- "Tract Consultants"
levels(client$client2)[levels(client$client2)=="TVS Architects (Gold Coast)"] <- "TVS Architects (Brisbane)"
levels(client$client2)[levels(client$client2)=="UQ - AIBN Auditorium C/ Hassell"] <- "Hassell"
levels(client$client2)[levels(client$client2)=="Donald Cant Watts Corke Pty Ltd"] <- "Donald Cant Watts Corke"


#brisbane city council - 
BrisRN<- grep('Brisbane City Council', client$client2)
client$client2[BrisRN]<- 'Brisbane City Council'
#buckley vann town planers
BuckleyRN<-  grep('Buckley Vann', client$client2)
client$client2[BuckleyRN]<- 'Buckley Vann'
#bureau proberts
BureauRN<-  grep('Bureau Proberts', client$client2)
client$client2[BureauRN]<- 'Bureau Proberts'
#cobie group - 
CobieRN<- c(grep(c('Cobie'), client$client2),grep(c('COBie'), client$client2))
client$client2[CobieRN]<- 'Cobie Group'
#fulton trotter - 
FultonRN<- c(grep(c('Fulton Trotter'), client$client2),grep(c('FTA'), client$client2))
client$client2[FultonRN]<- 'Fulton Trotter'
#jeremy ferrier - 
JeremRN<- grep('Jeremy Ferrier', client$client2)
client$client2[JeremRN]<- 'Jeremy Ferrier Landscape Architect'
#m3 - 
m3RN<- c(grep('m3', client$client2), grep('M3', client$client2))
client$client2[m3RN]<- 'm3architecture'
#milne and stonehouse - 
milneRN<- grep('Milne', client$client2)
client$client2[milneRN]<- 'Milne & Stonehouse Artists'
#push - 
pushRN<- c(grep('Push', client$client2), grep('PUSH', client$client2))
client$client2[pushRN]<- 'Push'
#ranbury - 
ranbRN<- grep('Ranbury', client$client2)
client$client2[ranbRN]<- 'Ranbury Management Group Pty Ltd'
#tim bennetton- 
timbRN<- grep('Tim Benn', client$client2)
client$client2[timbRN]<- 'Tim Bennetton Architects'
#WIM- 
wimRN<- c(grep('WIM', client$client2), grep('Wim', client$client2), grep('wim', client$client2),
           grep('W I M', client$client2), grep('w.i.m', client$client2))
client$client2[wimRN]<- 'WIM Architects'





#count frequency of client company and client contact
client.freq<-(ddply(client,.(client2),nrow))
clientcontact.freq<- (ddply(client,.(client.contact),nrow))

#read csv file with all the client details - want to merge this with client df
details<- read.csv('C:/Users/n9232371/Documents/Consultbusiness/data/17marchclients.csv',
                   na.strings = c("", " ", "NA"))
head(details)
#delete client.contact, client.count, and contact.count columns
details<- details[,c(1,5:8)]
#rename columns
names(details)[2]<- 'Business'
names(details)[4]<- 'Biz.size'
names(details)[5]<- 'Biz.type'

#delete rows with 4 NA values
details<-details[rowSums(is.na(details))!=4,]
#combine details with client, except don't want to include alternate names for clients.
#try merging with client.freq first
client.freq<- merge(client.freq, details, by.x= 'client2', by.y= 'client', all.x=TRUE)
#investigate which rows 'details' didnt have data for
#client.freq[rowSums(is.na(client.freq))==4,]
#these are fine - couldnt find online

#merge client.freq and clientcontact.freq with client
client<- (merge(client, client.freq, by= 'client2', all=TRUE))
client<- merge(client, clientcontact.freq, by= 'client.contact', all=TRUE)
#rename columns
colnames(client)[names(client) %in% 'V1.x']<-'client.count'
colnames(client)[names(client) %in% 'V1.y']<-'contact.count'
#make all 'NA' client contact counts == NA
client[client$client.contact %in% c('- -', '- TBA'),]$contact.count <- 'NA'
client[is.na(client$client.contact),]$contact.count <- 'NA'
client$contact.count <- as.numeric(client$contact.count)

#now to merge client with all!
# first merge client 2 into all
merger<- client[,names(client) %in% c('client','client2', 'caredfor')]
merger<- unique(merger)
all2<- merge(all, merger, by.x= 'Client.Company', by.y= 'client')
#now merge client fully into all2 by merging with client AND client contact
#first make 'client' df unique rows of client2 and client.count
uniq.client<- unique(client[,-c(3,4)])
all3<- merge(all2, uniq.client, by.x= c('client2', 'Client.Contact'), by.y= c('client2', 'client.contact'), all=TRUE)
test2<- all3[,c(1,2,46:51)]

#periodic save
write.csv(all3, 'all3.csv')
all3<- read.csv('all3.csv')

#have deidentified data and added post code distance variable
all4<- read.csv('all4.csv')[,-c(1,3,7)]
all4<- all4[order(all4$Job.Number),]


############# CLEAN UP DISCIPLINE COLUMN ###############
#rename levels in Discipline
levels(all4$Discipline)[levels(all4$Discipline)=="Structural,"] <- "Structural"
levels(all4$Discipline)[levels(all4$Discipline)=="Civil,"] <- "Civil"
levels(all4$Discipline)[levels(all4$Discipline)=="Environmental Planning,"] <- "Environmental Planning"
levels(all4$Discipline)[levels(all4$Discipline)=="EnvironmentalPlanning"] <- "Environmental Planning"
levels(all4$Discipline)[levels(all4$Discipline)=="Water,"] <- "Water"
#correct single 'Civil, Structural,' entry. School by paul callum, state school
all4[all4$Job.Name %in% 'Woodridge State School',]$Discipline<- 'Structural'
all4[all4$Job.Name %in% 'QUT Creative Industries',]$Discipline<- 'Structural'
all4[all4$Job.Name %in% 'Community Partnering 2014',]$Discipline<- 'Civil'
all4[all4$Job.Number %in% '2014.500.200',]$Director<- 'Chris Tanner'
all4$Discipline <- droplevels(all4$Discipline)
all4$Director <- droplevels(all4$Director)

#Assign Disciplines as per Director
RNrod<- rownames(all4[all4$Director %in% 'Rod Bligh' & is.na(all4$Discipline), ])
all4[RNrod,]$Discipline<- 'Structural'
RNcam<- rownames(all4[all4$Director %in% 'Cameron Riach' & is.na(all4$Discipline), ])
all4[RNcam,]$Discipline<- 'Civil'
RNcal<- rownames(all4[all4$Director %in% 'Paul Callum' & is.na(all4$Discipline), ])
all4[RNcal,]$Discipline<- 'Structural'
RNeas<- rownames(all4[all4$Director %in% 'Paul Easingwood' & is.na(all4$Discipline), ])
all4[RNeas,]$Discipline<- 'Structural'
RNdav<- rownames(all4[all4$Director %in% 'David Hamlyn-Harris' & is.na(all4$Discipline), ])
all4[RNdav,]$Discipline<- 'Water'
#Chris Tanner is hard to categorise between Civil, Water, and Environ Planning
#lets look at job names with the word 'stormwater' in it
rownames(all4)<- 1:nrow(all4)
waterRN<- c(grep('stormwater', all4$Job.Name), grep('Stormwater', all4$Job.Name))
sub<- all4[waterRN,]
subRN<- rownames(sub[is.na(sub$Discipline) & sub$Director %in% 'Chris Tanner',
         names(sub) %in% c('Director','Job.Number','Job.Name', 'Discipline', 'Business')])
all4[subRN,]$Discipline<- 'Environmental Planning'
#lets look at other key terms - Civil!
civRN<- c(grep('civil', all4$Job.Name), grep('Civil', all4$Job.Name))
sub.civ<- all4[civRN,]
civRN<- rownames(sub.civ[is.na(sub.civ$Discipline) & sub.civ$Director %in% 'Chris Tanner',
                 names(sub.civ) %in% c('Director','Job.Number','Job.Name', 'Discipline', 'Business')])
all4[civRN,]$Discipline<- 'Civil'

View(all4[is.na(all4$Discipline), names(all4) %in% c('Director','Job.Number','Job.Name', 'Discipline', 'Business',
        'Job.Detail.Primary', 'Project.Engineer')])
# until 2010.018 - no job detail.primary. give all chris tanner jobs 'civil' tag before then

chrisRN<- rownames(all4[1:2046,][all4$Director %in% 'Chris Tanner' & is.na(all4$Discipline), ])
all4[chrisRN[1:278],]$Discipline<- 'Civil'

# post 2010.018 do it based on primary job detail
all4[c(3263,3663, 3672,3802,4011),]$Discipline<- 'Civil'
all4[c(2139,2385),]$Discipline<- 'Water'
all4[c(3445, 3684, 3742, 3744, 3778,4026,4161, 4023, 4025),]$Discipline<- 'Environmental Planning'

#GOOD. Still 8 NA's in Director and Discipline.
View(all4[is.na(all4$Discipline), names(all4) %in% c('Director','Job.Number','Job.Name', 'Discipline', 'Business',
        'Job.Detail.Primary', 'Project.Engineer')])
#read job names and fill in any obvious disciplines
all4[c(2,16,17,46),]$Discipline<- 'Structural'
all4[c(30,31),]$Discipline<- 'Civil'

write.csv(all4, 'all4a.csv')

############### CLEAN UP SUBURB COLUMN ####################

all4a<- read.csv('all4a.csv')[,-1]
#load list of suburbs : singPC from postcodes.R script
singPC<- read.csv('singPC.csv')[,-1]
# vector for list of available post suburbs
suburb<- singPC$Suburb
# want all4a to have another column for capital letter job names
all4a$SUBURB<-  toupper(all4a$Job.Name)
#create a df SUBS which has only rows with NA for suburb AND post code
RN<- rownames(all4a[is.na(all4a$Suburb) & is.na(all4a$Post.Code),
                names(all4a) %in% c('Post.Code','SUBURB', 'Job.Name')])
SUBS<- all4a[RN,]
# write a function that checks if a suburb is listed in the job name, and if so, lists the suburb in a vector

match<- NULL
for(i in 1:nrow(singPC)){
        RN= grep(paste("\\b",singPC[i,]$Suburb,"\\b", sep="") , SUBS$SUBURB)
        mi= data.frame('row'=RN, 'Suburb'= rep(singPC[i,]$Suburb, length(RN)))
        match= rbind(match, mi)
}
return(match)


#add in rownames that match all4a data frame
match$allRN<- rownames(SUBS[match$row,])
#substitute in match$Suburb column into relevant rows of all4a
all4a$Suburb<- as.character(all4a$Suburb)
all4a[match$allRN, names(test) %in% c('Suburb')]<- match$Suburb
#do same for UQ in Job.Name
RNuq= c(grep("\\bUQ\\b", SUBS$SUBURB),grep("\\buq\\b", SUBS$SUBURB))
rownamesuq<- rownames(SUBS[RNuq,])
all4a[rownamesuq, names(test) %in% 'Suburb'] <- 'UNIVERSITY OF QUEENSLAND'
#clean up anything that looks odd
all4a[match$allRN, names(test) %in% c('Suburb', 'Job.Name')]
all4a['2004',]$Suburb<- 'MT TAMBORINE'
all4a<- all4a[!(rowSums(is.na(all4a))==51),]
#delete SUBURBS column
all4a<- all4a[,!(names(all4a) %in% 'SUBURB')]


############### ADD INTO JOB.DETAIL.PRIMARY COLUMN ####################

#what are the options for Job.Detail.Primary?
summary(all4a$Job.Detail.Secondary)
#clean up the levels
levels(all4a$Job.Detail.Primary)[levels(all4a$Job.Detail.Primary)==
                                         "3B Flood drainage studies &  infrastructure plan"] <- 
        "3B Flood drainage studies & infrastructure plan"
View(all4a[(is.na(all4a$Job.Detail.Primary)),names(all4a) %in% c('Job.Name', 'Job.Detail.Primary','Job.Number', 'Discipline')])
View(all4a[all4a$Job.Detail.Secondary %in% '2E Land Subdivision',
           names(all4a) %in% c('Job.Detail.Primary','Job.Number', 'Job.Name','Job.Detail.Secondary','Discipline')])

#create another column of JD.Primary (Job.Detail.Primary) using YOUR rules only
#grep words
# vector of words that match a given category
#want to loop through each category. Then loop through each word within the vector for that category?? 
#periodic save before trying something weird
all4b<- all4a
write.csv(all4b,'all4b.csv')
all4b<- read.csv('all4b.csv')[,-1]

## words to correct
all4b$Job.Name<- as.character(all4b$Job.Name)
grep('All Hallows', all4b$Job.Name)
all4b['336', names(all4b) %in% 'Job.Name']<- 'All Hallows Facade'
grep('lakeview', all4b$Job.Name, ignore.case=TRUE)
all4b['559', names(all4b) %in% 'Job.Name']<- 'Lakeview Carwash Cafe'
grep('euro', all4b$Job.Name, ignore.case=TRUE)
all4b['603', names(all4b) %in% 'Job.Name']<- 'Euro Cafe'


#create new column for JD.Primary and make all lowercase
all4b$JD.Primary<- as.character(all4b$Job.Detail.Primary)

#create data frame of category key words (that I made manually by sscrolling through all the data) 
cat<- read.csv('Catgrep.csv', na.strings= "")
cat<- t(cat)
# delete columns with 29 NA values, rename columns using first row
colnames(cat)<- cat[1,]
cat<-cat[-c(1,2),]
cat<-cat[ ,colSums(is.na(cat))!=nrow(cat)]
#delete white space before and after
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
cat<- trim(cat)


#write function to create a vector for each column of complete cases only.
grep.vec<-function(df=cat){
        for (i in 1:ncol(df)){
                tempvec= c(df[,i])
                assign(paste("vec",i,sep=""), tempvec[complete.cases(tempvec)]
                       ,envir=.GlobalEnv)
                cat(i," ")
        }        
}

grep.vec(cat)

#grep through first column -- see what happens!
all4b$JD.Primary<- ifelse(grepl(paste(vec1,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & all4b$Discipline %in% 'Structural',
         colnames(cat)[1], all4b$JD.Primary)
all4b$JD.Primary<- ifelse(grepl(paste(vec2,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & all4b$Discipline %in% 'Structural',
         colnames(cat)[2], all4b$JD.Primary)
all4b$JD.Primary<- ifelse(grepl(paste(vec3,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & all4b$Discipline %in% 'Structural',
                          colnames(cat)[3], all4b$JD.Primary)
all4b$JD.Primary<- ifelse(grepl(paste(vec4,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & all4b$Discipline %in% 'Structural',
                          colnames(cat)[4], all4b$JD.Primary)

all4b$JD.Primary<- ifelse(grepl(paste(vec1,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & all4b$Discipline %in% 'Civil'
                          & is.na(all4b$Job.Detail.Primary), colnames(cat)[5], all4b$JD.Primary)
all4b$JD.Primary<- ifelse(grepl(paste(vec2,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & all4b$Discipline %in% 'Civil'
                          & is.na(all4b$Job.Detail.Primary), colnames(cat)[5], all4b$JD.Primary)
all4b$JD.Primary<- ifelse(grepl(paste(vec3,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & all4b$Discipline %in% 'Civil'
                          & is.na(all4b$Job.Detail.Primary), colnames(cat)[5], all4b$JD.Primary)
all4b$JD.Primary<- ifelse(grepl(paste(vec4,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & all4b$Discipline %in% 'Civil'
                          & is.na(all4b$Job.Detail.Primary), colnames(cat)[5], all4b$JD.Primary)
all4b$JD.Primary<- ifelse(grepl(paste(vec13,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & all4b$Discipline %in% 'Civil'
                          & is.na(all4b$Job.Detail.Primary), colnames(cat)[5], all4b$JD.Primary)

all4b$JD.Primary<- ifelse(grepl(paste(vec5,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & !(all4b$Discipline %in% 'Structural'),
                          colnames(cat)[5], all4b$JD.Primary)
all4b$JD.Primary<- ifelse(grepl(paste(vec6,collapse="|"), all4b$Job.Name, ignore.case=TRUE),
         colnames(cat)[6], all4b$JD.Primary)
all4b$JD.Primary<- ifelse(grepl(paste(vec7,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & !(all4b$Discipline %in% 'Structural'),
                          colnames(cat)[7], all4b$JD.Primary)
all4b$JD.Primary<- ifelse(grepl(paste(vec8,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & !(all4b$Discipline %in% 'Structural'),
                          colnames(cat)[8], all4b$JD.Primary)
all4b$JD.Primary<- ifelse(grepl(paste(vec9,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & !(all4b$Discipline %in% 'Structural'),
                          colnames(cat)[9], all4b$JD.Primary)
all4b$JD.Primary<- ifelse(grepl(paste(vec10,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & all4b$Discipline %in% 'Structural',
                          colnames(cat)[10], all4b$JD.Primary)
all4b$JD.Primary<- ifelse(grepl(paste(vec11,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & all4b$Discipline %in% 'Structural',
                          colnames(cat)[11], all4b$JD.Primary)
all4b$JD.Primary<- ifelse(grepl(paste(vec12,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & all4b$Discipline %in% 'Structural',
                          colnames(cat)[12], all4b$JD.Primary)
all4b$JD.Primary<- ifelse(grepl(paste(vec13,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & all4b$Discipline %in% 'Structural',
                          colnames(cat)[13], all4b$JD.Primary)
all4b$JD.Primary<- ifelse(grepl(paste(vec14,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & all4b$Discipline %in% 'Structural',
                          colnames(cat)[14], all4b$JD.Primary)

#inspect results: exclude NAs for Jobdetails, choose discipline, exclude Job.Detail.Primary = JD.Primary
nrow(all4b[!(is.na(all4b$Job.Detail.Primary) & (is.na(all4b$JD.Primary))),
           names(all4b) %in% c('Discipline','Job.Name','Job.Detail.Primary','JD.Primary')])

write.csv(all4b[!(is.na(all4b$Job.Detail.Primary) & (is.na(all4b$JD.Primary))),
           names(all4b) %in% c('Discipline','Job.Name','Job.Detail.Primary','JD.Primary','Job.Detail.Secondary')], 'temp.csv')


## ON TO GREPPING FOR SECONDARY DETAIL
summary(all4b$Job.Detail.Secondary)

#create new column for JD.Primary and make all lowercase
all4b$JD.Second<- c(rep(NA, nrow(all4b)))

#create data frame of category key words (that I made manually by sscrolling through all the data) 
sec<- read.csv('second_detail.csv', na.strings= "")
sec<- t(sec)
# delete columns with 29 NA values, rename columns using first row
colnames(sec)<- sec[1,]
sec<-sec[-c(1),]
sec<-sec[ ,colSums(is.na(sec))!=nrow(cat)]
#delete white space before and after
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
sec<- trim(sec)

#create trimmed vectors for each column to feed into grepping
grep.svec<-function(df=cat){
        for (i in 1:ncol(df)){
                tempvec= c(df[,i])
                assign(paste("svec",i,sep=""), tempvec[complete.cases(tempvec)]
                       ,envir=.GlobalEnv)
                cat(i," ")
        }        
}

 
 
 
grep.svec(sec)

#function for grepping columns - structural
#group my list of svec vectors into four 'lists'. Ones that I want to apply to structural, not structural, and civil, and any
# also create title vectors with the number column in 'sec' where you would find the title
struc.list<- list(svec1, svec2, svec3, svec11, svec13, svec14, svec15, svec16,svec17, svec18, svec19, svec20, svec21, svec22,
                  svec23, svec24, svec25, svec26, svec27, svec28, svec36, svec37, svec38, svec39, svec40, svec41, svec42, svec43, svec45)      
struc.title<- c(1,2,3,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,36,37,38,39,40,41,42,43,45)
civ.list<- list(svec44)
civ.title<- c(44)
nstruc.list<- list(svec5, svec6, svec8, svec29, svec30, svec31, svec32, svec33, svec34, svec35)
nstruc.title<- c(5,6,8,29,30,31,32,33,34,35)
any.list<- list(svec4, svec7, svec9, svec10, svec12)
anylist.title<- c(4,7,9,10,12)

# function for greppipng through each vector in the list of vectors. Also calls the vector referring to the title in sec. Also calls up 
#the discipline
grepster<- function(vlist = any.list, titles= anylist.title, Disc= c('Structural', 'Civil','Environmental Planning','Water','NA')){
        for( i in 1:length(vlist)){
                all4b$JD.Second = ifelse(grepl(paste(vlist[[i]],collapse="|"), all4b$Job.Name, ignore.case=TRUE)
                        & all4b$Discipline %in% Disc, colnames(sec)[titles[i]], all4b$JD.Second)               
        } 
        all4b$JD.Second
}
        
all4b$JD.Second<- grepster(any.list, anylist.title, c('Structural', 'Civil','Environmental Planning','Water','NA') )
all4b$JD.Second<- grepster(vlist= struc.list, titles= struc.title, Disc= c('Structural'))
all4b$JD.Second<- grepster(civ.list, civ.title, c('Civil') )
all4b$JD.Second<- grepster(nstruc.list, nstruc.title, c('Civil','Environmental Planning','Water','NA') )

#View(all4b[,names(all4b) %in% c('Discipline','Job.Name','JD.Primary', 'JD.Second')])
# saveeee!!!!!!
 
write.csv(all4b,'all4c.csv')

#after running through post-codes R script, have new all4d

all4d<- read.csv('all4d.csv')[,-1]
str(all4d)

#need to clean up Job.Type.Primary!! 
#if JD.Primary begins with a number, then print ___ into Job.Type.Primary
all4d$Job.Type.Primary<- ifelse(grepl("^1",all4d$JD.Primary, ignore.case=TRUE),
                           "1. BuildingStructures", all4d$Job.Type.Primary)
 all4d$Job.Type.Primary<- ifelse(grepl("^2",all4d$JD.Primary, ignore.case=TRUE),
                                "2. LandInfrastructure", all4d$Job.Type.Primary)
 all4d$Job.Type.Primary<- ifelse(grepl("^3",all4d$JD.Primary, ignore.case=TRUE),
                                "3. EnvironmentalPlanning", all4d$Job.Type.Primary)
 all4d$Job.Type.Primary<- ifelse(grepl("^4",all4d$JD.Primary, ignore.case=TRUE),
                                "4. IntegratedWaterManagement", all4d$Job.Type.Primary)
 all4d$Job.Type.Primary<- ifelse(grepl("^5",all4d$JD.Primary, ignore.case=TRUE),
                                "5. SpecialStructures", all4d$Job.Type.Primary)
 all4d$Job.Type.Primary<- ifelse(grepl("^6",all4d$JD.Primary, ignore.case=TRUE),
                                "6. Other", all4d$Job.Type.Primary)
 
 
 all4d %>% select(Job.Number, Job.Name, Job.Detail.Primary, Job.Type.Primary, JD.Primary) %>% arrange(Job.Number) %>% head(50)
 
 #save as 4e
 
 write.csv(all4d, 'all4e.csv')
 