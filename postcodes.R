#### create file with distances from Brisbane city using latitude/longitude coords

setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
library('fields',lib='C:/Progra~1/R/R-3.1.3/library')
library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
all3<- read.csv('all3.csv')
coords<- read.csv('postcodes.csv')

#merge all3 postcodes with coords
#create temp file with only post codes and suburbs and states from all3
str(all3)
temp<- all3[, names(all3) %in% c('Post.Code', 'Suburb', 'State')] %>% head()
temp$Post.Code<- as.integer(as.character(temp$Post.Code))
#very confusing because so many suburb names match the post code
#need to merge original databases using postcode AND suburb name
#therefore need suburb names to be in capital letters. use toupper()
temp$Suburb<- toupper(temp$Suburb)
temp$Suburb<- as.factor(temp$Suburb)
#temp<- temp[temp$Post.Code>0,]
#create temp file with only post codes and suburbs and states from coords, and coords
temp2<- coords[, names(coords) %in% c('postcode', 'suburb', 'state', 'lat', 'lon')]
head(temp2)
#merge the two
PC<- merge(temp, temp2, by.x= c('Post.Code', 'Suburb'), by.y= c('postcode','suburb'), all.x=TRUE)
PC[400:500,]
#cross check with all3 
all3[all3$Suburb %in% 'Lismore', names(all3) %in% c('Suburb', 'Post.Code')]
#looks fine
#delete rows with 3 NA's in a single row AND Post.Code = 0
PC<-PC[!(PC$Post.Code == 0 & rowSums(is.na(PC[,c(2,4:6)]))==4),]
#any rows of full NA left?
PC<- PC[!(rowSums(is.na(PC))==6),]
ggplot(PC, aes(x=Post.Code)) + geom_point(stat='bin')
# yes, deleted them
#want unique cases of each suburb
PC<- unique(PC)
# want to trim down to first entry for each post code number
#create a function

skim<- function(df= PC, col=PC$Post.Code, n=1){
        ref= unique(col)
        uniq= df[rep(FALSE,times=nrow(df)),]
        for(i in 1:length(ref)){
                rows= df[col %in% ref[i],]
                comp= rows[complete.cases(rows),][n,]
                uniq= rbind(uniq,comp)
        }
        return(uniq)
}

#create a new data frame with clean first entries
singPC<- skim(df=PC, col=PC$Post.Code, n=1)
#trim down to complete cases
singPC<- singPC[complete.cases(singPC),]
#now to address incomplete suburbs in original PC
left<- PC[(rowSums(is.na(PC[,c(4:6)]))==3),]
left<-unique(left)
#is the post codes in left an element of the post codes in singPC? 
# if so, can delete because it's taken care of.
left<- left[!(left$Post.Code %in% singPC$Post.Code),]
#what's left in 'left' data frame are all entries that WON'T work when I match singPC to the all3 dataframe
#because the post code was input incorrectly
#run a script over the original 'left' data frame that assigns the correct postcode to the names along with coords
left<- merge(left[,1:3], temp2, by.x= c('Suburb', 'State'), by.y= c('suburb','state'), all.x=TRUE)
#clean out rows that didn't work and save them as left 2
left2<- left[(rowSums(is.na(left[,c(4:6)]))==3),]
left<-left[complete.cases(left),]
#clean left delete original Post.Code column
left<- left[,-3]
#delete repeated suburbs
rownames(left)<- c(1:nrow(left))
left<-left[-c(17,19,42,45,49:199,205),]
#correct sydney
left['48',3]<-2000
left['48',4]<- -33.86714
left['48',5]<-151.2071
#this leaves a list of suburbs that need to have postcodes manually put into the 'all' data frame.
#make all suburb names capitalised, all post codes integers,
all3$Suburb<- toupper(all3$Suburb)
all3$Post.Code<- as.integer(as.character(all3$Post.Code))
#all postcodes less than 500 are NA
all3$Post.Code[all3$Post.Code< 500]<- NA
#then run this as a correction script over the original 'all'
step1<-all3[all3$Suburb %in% left$Suburb & is.na(all3$Post.Code),]
RN<- rownames(step1)
step3<- merge(step1, left, by= 'Suburb')
all3[RN,]$Post.Code<- step3$postcode 
#now all suburbs with blank postcodes have been filled in!
#next correction - create entries in singPC for the entries with a postcode but no suburb name: left2
left2<- data.frame(Post.Code= unique(left2$Post.Code))
#merge postcodes with temp2
left2<- merge(left2, temp2, by.x='Post.Code', by.y= 'postcode', all.x=TRUE)
#run function to skim the top entry off each unique Post.Code
left2<-skim(df= left2, col=left2$Post.Code, n=1)
left2<-left2[complete.cases(left2),]
#now combine left 2 with singPC because we have coordinates for all the postcodes with missing suburbs
#should just be an rbind!?
colnames(left2)[names(left2) %in% 'suburb']<-'Suburb'
singPC<- rbind(singPC[,-3],left2)
#now combine left with singPC - may have repeated postcodes?
#delete these rows which already have post codes in singPC
left$postcode<- as.integer(left$postcode)
left<- left[!(left$postcode %in% singPC$Post.Code),]
#only 9 left! rbind to singPC
colnames(left)[names(left) %in% 'State']<-'state'
colnames(left)[names(left) %in% 'postcode']<-'Post.Code'
singPC<-rbind(singPC,left[,c(3,1,2,4,5)])



#calculate distance to brisbane cbd.
#correct UNIVERSITY OF QUEENSLAND and SPRING HILL COORDS
singPC['301',4]<- -27.495150
singPC['301',5]<- 153.012041
singPC['53',4]<- -27.46087
singPC['53',5]<- 153.0245
singPC['132',4]<- -27.513190
singPC['132',5]<- 153.157777

#use rdist.earth() function with radius of 6371km
mtx1<- as.matrix(singPC[,4:5])
mtx2<- singPC[singPC$Suburb %in% 'BRISBANE',][,4:5]
#output the distance in km to a vector, and add to singPC dataframe
dist<-rdist.earth(mtx1,mtx2,miles=FALSE, R=6371)
singPC$dist<- round(dist,1)
#plot distances
ggplot(singPC, aes(x=dist))+ geom_point(stat='bin')


#progressive save

singPC<- singPC[!is.na(singPC$Suburb),]
write.csv(singPC, 'singPC.csv')
singPC<- read.csv('singPC.csv')[,-1]



#extra albion row and first col removed
singPC$Post.Code<- as.integer(singPC$Post.Code)

#populate all3 with distances!
#but first <- correct all the postcodes in all3.. now all4!
all4<- all3
# clean up suburbs a bit
#create 'INT' postcode for international jobs
WoodsRN<- grep('Woodsfield', all4$client2)
all4$Post.Code[WoodsRN]<- 'INT'
#Albion
AlbRN<- grep('ALBION', all3$Suburb)
all4$Post.Code[AlbRN]<- 4010
#Acacia Ridge
AcaRN<- grep('ACACIA RIDGE', all4$Suburb)
all4$Post.Code[AcaRN]<- 4110

#write a function for the above
grep.lev<- function(grep='ASCOT', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4007){
        RN= grep(grep, grepcol)
        replace.col[RN]<- replace
        replace.col
}

all4$Post.Code<- grep.lev(grep='ASCOT', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4007)
all4[all4$Suburb %in% 'ASCOT',]$Post.Code
all4$Post.Code<- grep.lev(grep='BALD KNOB', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4552)
all4[all4$Suburb %in% 'BALD KNOB',]$Post.Code
all4$Post.Code<- grep.lev(grep='BALLINA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 2478)
all4[all4$Suburb %in% 'BALLINA',]$Post.Code
all4$Post.Code<- grep.lev(grep='BALMORAL', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4171)
all4[all4$Suburb %in% 'BALMORAL',]$Post.Code
all4$Post.Code<- grep.lev(grep='BANGALOW', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 2479)
all4$Post.Code<- grep.lev(grep='BANORA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 2486)
all4$Post.Code<- grep.lev(grep='BANYO', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4014)
all4$Post.Code<- grep.lev(grep='BOWEN HILLS', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4006)
all4$Post.Code<- grep.lev(grep='BRIGHTON', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4017)
all4$Post.Code<- grep.lev(grep='BRISBANE AIRPORT', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4007)
all4$Post.Code<- grep.lev(grep='BROOKFIELD', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4069)
all4$Post.Code<- grep.lev(grep='BUDERIM', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4556)
all4$Post.Code<- grep.lev(grep='BULIMBA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4171)
all4$Post.Code<- grep.lev(grep='BUNDABERG', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4670)
all4$Post.Code<- grep.lev(grep='BUNYA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4055)
all4$Post.Code<- grep.lev(grep='BURPENGARY', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4505)
all4$Post.Code<- grep.lev(grep='BYRON BAY', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 2481)
all4$Post.Code<- grep.lev(grep='CABOOLTURE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4510)
all4$Post.Code<- grep.lev(grep='CAIRNS', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4870)
all4$Post.Code<- grep.lev(grep='CAMP HILL', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4152)
all4$Post.Code<- grep.lev(grep='CANBERRA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 2601)
all4$Post.Code<- grep.lev(grep='CAPALABA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4157)
all4$Post.Code<- grep.lev(grep='CAPE TRIBULATION', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4870)
all4$Post.Code<- grep.lev(grep='CARINA HEIGHTS', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4152)
all4$Post.Code<- grep.lev(grep='CARINDALE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4152)
all4$Post.Code<- grep.lev(grep='CHAPEL HILL', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4069)
all4$Post.Code<- grep.lev(grep='CHELMER', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4068)
all4$Post.Code<- grep.lev(grep='CHERMSIDE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4032)
all4$Post.Code<- grep.lev(grep='CHINCHILA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4413)
all4$Post.Code<- grep.lev(grep='CHRISTMAS ISLAND', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 6798)
all4$Post.Code<- grep.lev(grep='CLAYFIELD', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4011)
all4$Post.Code<- grep.lev(grep='CLEVELAND', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4163)
all4$Post.Code<- grep.lev(grep='CONDAMINE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4416)
all4$Post.Code<- grep.lev(grep='COOLUM', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4573)
all4$Post.Code<- grep.lev(grep='COORPAROO', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4151)
all4$Post.Code<- grep.lev(grep='CURRUMBIN VALLEY', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4223)
all4$Post.Code<- grep.lev(grep='DAISY HILL', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4127)
all4$Post.Code<- grep.lev(grep='DALBY', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4405)
all4$Post.Code<- grep.lev(grep='DECEPTION BAY', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4305)
all4$Post.Code<- grep.lev(grep='DOCKLAND', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 3008)
all4$Post.Code<- grep.lev(grep='DUBAI', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 'INT')
all4$Post.Code<- grep.lev(grep='DUTTON PARK', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4102)
all4$Post.Code<- grep.lev(grep='EAGLE FARM', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4009)
all4$Post.Code<- grep.lev(grep='EAGLE HEIGHTS', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4271)
all4$Post.Code<- grep.lev(grep='EAST BRISBANE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4169)
all4$Post.Code<- grep.lev(grep='EIGHT MILE PLAINS', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4113)
all4$Post.Code<- grep.lev(grep='EIMEO', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4740)
all4$Post.Code<- grep.lev(grep='ENOGGERA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4051)
all4$Post.Code<- grep.lev(grep='ESK', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4312)
all4$Post.Code<- grep.lev(grep='FAIRFIELD', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4103)
all4$Post.Code<- grep.lev(grep='FIG TREE POCKET', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4069)
all4$Post.Code<- grep.lev(grep='FITZGIBBON', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4018)
all4$Post.Code<- grep.lev(grep='FOREST LAKE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4078)
all4$Post.Code<- grep.lev(grep='FORTITUDE VALLEY', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4006)
all4$Post.Code<- grep.lev(grep='FRASER ISLAND', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4581)
all4$Post.Code<- grep.lev(grep='GAP', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4061)
all4$Post.Code<- grep.lev(grep='GARDENS POINT', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4000)
all4$Post.Code<- grep.lev(grep='GATTON', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4343)
all4$Post.Code<- grep.lev(grep='GLADSTONE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4680)
all4$Post.Code<- grep.lev(grep='GOLD COAST', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4227)
all4$Post.Code<- grep.lev(grep='GOVE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 880)
all4$Post.Code<- grep.lev(grep='GREENSLOPES', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4120)
all4$Post.Code<- grep.lev(grep='GYMPIE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4570)
all4$Post.Code<- grep.lev(grep='HAMILTON', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4007)
all4$Post.Code<- grep.lev(grep='HAWTHORNE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4171)
all4$Post.Code<- grep.lev(grep='HERSTON', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4006)
all4$Post.Code<- grep.lev(grep='HIGHGATE HILL', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4101)
all4$Post.Code<- grep.lev(grep='HILLCREST', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4118)
all4$Post.Code<- grep.lev(grep='HOLLAND PARK', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4121)
all4$Post.Code<- grep.lev(grep='INDOOROOPILLY', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4068)
all4$Post.Code<- grep.lev(grep='INVERELL', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 2360)
all4$Post.Code<- grep.lev(grep='IPSWICH', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4305)
all4$Post.Code<- grep.lev(grep='JIMBOOMBA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4280)
all4$Post.Code<- grep.lev(grep='KANGAROO POINT', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4169)
all4$Post.Code<- grep.lev(grep='KEDRON', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4031)
all4$Post.Code<- grep.lev(grep='KELVIN GROVE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4059)
all4$Post.Code<- grep.lev(grep='KENMORE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4069)
all4$Post.Code<- grep.lev(grep='KILCOY', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4515)
all4$Post.Code<- grep.lev(grep='KINGSGROVE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 2208)
all4$Post.Code<- grep.lev(grep='KNOCKROW', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4215)
all4$Post.Code<- grep.lev(grep='KURABY', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4112)
all4$Post.Code<- grep.lev(grep='KURWONGBAH', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4503)
all4$Post.Code<- grep.lev(grep='LANDSBOROUGH', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4550)
all4$Post.Code<- grep.lev(grep='LUTWYCHE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4030)
all4$Post.Code<- grep.lev(grep='MACKAY', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4740)
all4$Post.Code<- grep.lev(grep='MALENY', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4552)
all4$Post.Code<- grep.lev(grep='MANLY', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4179)
all4$Post.Code<- grep.lev(grep='MANSFIELD', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4122)
all4$Post.Code<- grep.lev(grep='MAROOCHY SHIRE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4558)
all4$Post.Code<- grep.lev(grep='MAROOCHYDORE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4558)
all4$Post.Code<- grep.lev(grep='MARYBOROUGH', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4650)
all4$Post.Code<- grep.lev(grep='MCDOWALL', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4053)
all4$Post.Code<- grep.lev(grep='MERINGANDAH', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4352)
all4$Post.Code<- grep.lev(grep='MIDDLEMOUNT', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4746)
all4$Post.Code<- grep.lev(grep='MONTVILLE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4560)
all4$Post.Code<- grep.lev(grep='MOOROOKA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4105)
all4$Post.Code<- grep.lev(grep='MORAYFIELD', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4506)
all4$Post.Code<- grep.lev(grep='MORETON ISLAND', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4025)
all4$Post.Code<- grep.lev(grep='MT GRAVATT', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4122)
all4$Post.Code<- grep.lev(grep='MT ISA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4825)
all4$Post.Code<- grep.lev(grep='MT TAMBORINE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4272)
all4$Post.Code<- grep.lev(grep='MT TAMBOURINE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4272)
all4$Post.Code<- grep.lev(grep='MURRARIE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4172)
all4$Post.Code<- grep.lev(grep='MURWILLUMBAH', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 2484)
all4$Post.Code<- grep.lev(grep='MUSWELLBROOK', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 2333)
all4$Post.Code<- grep.lev(grep='NAMBOUR', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4560)
all4$Post.Code<- grep.lev(grep='NEW FARM', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4005)
all4$Post.Code<- grep.lev(grep='NEWCASTLE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 2300)
all4$Post.Code<- grep.lev(grep='NEWMARKET', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4051)
all4$Post.Code<- grep.lev(grep='NEWSTEAD', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4006)
all4$Post.Code<- grep.lev(grep='NOOSA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4567)
all4$Post.Code<- grep.lev(grep='NORMAN PARK', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4170)
all4$Post.Code<- grep.lev(grep='NORTHGATE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4013)
all4$Post.Code<- grep.lev(grep='NUDGEE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4014)
all4$Post.Code<- grep.lev(grep='ORMISTON', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4160)
all4$Post.Code<- grep.lev(grep='PINJARA HILLS', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4069)
all4$Post.Code<- grep.lev(grep='PINKENBA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4008)
all4$Post.Code<- grep.lev(grep='POONA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4650)
all4$Post.Code<- grep.lev(grep='PORT MORESBY', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 'INT')
all4$Post.Code<- grep.lev(grep='PORT MORESBY', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 'INT')
all4$Post.Code<- grep.lev(grep='PORT OF BRISBANE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4178)
all4$Post.Code<- grep.lev(grep='PULLENVALE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4069)
all4$Post.Code<- grep.lev(grep='ROCKHAMPTON', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4701)
all4$Post.Code<- grep.lev(grep='ROCKLEA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4106)
all4$Post.Code<- grep.lev(grep='ROMA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4455)
all4$Post.Code<- grep.lev(grep='ROPES CROSSING', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 2760)
all4$Post.Code<- grep.lev(grep='ROTHWELL', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4022)
all4$Post.Code<- grep.lev(grep='RUSSELL ISLAND', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4184)
all4$Post.Code<- grep.lev(grep='SANDGATE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4017)
all4$Post.Code<- grep.lev(grep='SHERWOOD', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4075)
all4$Post.Code<- grep.lev(grep='SINGLETON', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 2330)
all4$Post.Code<- grep.lev(grep='SIPPY DOWNS', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4556)
all4$Post.Code<- grep.lev(grep='SOUTH BANK', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4101)
all4$Post.Code<- grep.lev(grep='SOUTH BRISBANE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4101)
all4$Post.Code<- grep.lev(grep='SOUTH STRADBROKE ISLAND', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4216)
all4$Post.Code<- grep.lev(grep='SOUTHPORT', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4215)
all4$Post.Code<- grep.lev(grep='SPRING HILL', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4000)
all4$Post.Code<- grep.lev(grep='SPRINGFIELD', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4300)
all4$Post.Code<- grep.lev(grep='SPRINGWOOD', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4127)
all4$Post.Code<- grep.lev(grep='ST LUCIA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4067)
all4$Post.Code<- grep.lev(grep='STAFFORD', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4053)
all4$Post.Code<- grep.lev(grep='STANHOPE GARDENS', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 2768)
all4$Post.Code<- grep.lev(grep='STRETTON', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4116)
all4$Post.Code<- grep.lev(grep='SUNSHINE COAST', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4567)
all4$Post.Code<- grep.lev(grep='SYDNEY', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 2000)
all4$Post.Code<- grep.lev(grep='TARA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4421)
all4$Post.Code<- grep.lev(grep='TARINGA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4068)
all4$Post.Code<- grep.lev(grep='THE GAP', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4061)
all4$Post.Code<- grep.lev(grep='THURSDAY ISLAND', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4875)
all4$Post.Code<- grep.lev(grep='TOOGOOLOOWAH', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4313)
all4$Post.Code<- grep.lev(grep='TOOMBUL', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4012)
all4$Post.Code<- grep.lev(grep='TOOWONG', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4066)
all4$Post.Code<- grep.lev(grep='TOOWOOMBA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4350)
all4$Post.Code<- grep.lev(grep='TOWNSVILLE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4810)
all4$Post.Code<- grep.lev(grep='TUGUN', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4224)
all4$Post.Code<- grep.lev(grep='TWEED HEADS', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4854)
all4$Post.Code<- grep.lev(grep='VANUATU', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 'INT')
all4$Post.Code<- grep.lev(grep='VARSITY LAKES', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4227)
all4$Post.Code<- grep.lev(grep='WACOL', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4076)
all4$Post.Code<- grep.lev(grep='WAGGA WAGGA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 2650)
all4$Post.Code<- grep.lev(grep='WEST END', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4101)
all4$Post.Code<- grep.lev(grep='WILSTON', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4051)
all4$Post.Code<- grep.lev(grep='WINDSOR', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4030)
all4$Post.Code<- grep.lev(grep='WOODRIDGE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4114)
all4$Post.Code<- grep.lev(grep='WYNNUM', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4178)
all4$Post.Code<- grep.lev(grep='YEPPOON', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4703)
all4$Post.Code<- grep.lev(grep='YERONGA', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4104)
all4$Post.Code<- grep.lev(grep='ZILLMERE', grepcol=all4$Suburb, replace.col=all4$Post.Code, replace= 4034)

#populate all4 with distances!
all4$Post.Code<- as.integer(all4$Post.Code)
write.csv(all4, 'all4.csv')
all4<-merge(all4, singPC[,c(1,6)], by= 'Post.Code', all.x=TRUE, all.y=FALSE)
summary(all4$dist)
ggplot(all4, aes(x=dist)) + geom_point(stat='bin')
#progressive save
write.csv(all4, 'all4.csv')

#all4c now has new 'suburb' extracted from job names that matched elements in singPC
#run the script again that converts suburb names to post codes from singPC.
all4c<- read.csv('all4c.csv')[,-1]
#subset out all rows with NA post.code but does have a suburb name
naPC<- all4c[is.na(all4c$Post.Code) & !(is.na(all4c$Suburb)),names(all4c) %in% c('Suburb', 'Post.Code')]
RNnaPC<- rownames(naPC)
#merge with singPC
naPC<- merge(naPC, singPC, by='Suburb', all.x=TRUE, all.y=FALSE)
# substitute back merged postcodes into full all4c dataframe
all4c[RNnaPC,]$Post.Code<- naPC$Post.Code.y

#input distances etc!
all4c<-merge(all4c, singPC[,c(1,6)], by= 'Post.Code', all.x=TRUE, all.y=FALSE)
summary(all4c$dist.y)
#rename dist.y to dist
colnames(all4c)[names(all4c) %in% 'dist.y']<-'dist'
#delete dist.x
all4c<- all4c[,!(names(all4c) %in% 'dist.x')]
ggplot(all4c, aes(x=dist)) + geom_point(stat='bin')
#progressive save
write.csv(all4c, 'all4d.csv')
