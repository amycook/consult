####### DEIDENTIFYING DATA
setwd("C:/Users/n9232371/Documents/Consultbusiness/data")
library('ggplot2', lib = 'C:/Progra~1/R/R-3.1.2/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.1.2/library')
library("magrittr",lib = 'C:/Progra~1/R/R-3.1.2/library')



#have added distances to all4
all4<- read.csv('all4.csv')
#de-identify data

#codify client2, Client.contact, job.number, Director, Project.Engineer, Engineer.2, Suburb
#code client2
code.client2<- data.frame(client2=unique(all4$client2))
code.client2$code<- paste('C', sample(2000:3000, nrow(code.client2), replace=F), sep="")
write.csv(code.client2,'code_client2.csv')
#code Client.Contact
code.contact<- data.frame(contact=unique(all4$Client.Contact))
code.contact$code<- paste('CN', sample(1000:2000, nrow(code.contact), replace=F), sep="")
write.csv(code.contact,'code_contact.csv')
#code Job.Number
# need to separate milestones
code.job<- data.frame(job=unique(all4$Job.Number))
#have a look at job numbers in order
code.job<- code.job[order(code.job$job),]
#split off the first way of numbering jobs
code.job1<- data.frame(job1=code.job[1:2426])
code.job2<- data.frame(job2=code.job[2427:length(code.job)])

#code.job1 - split up job number into 2010.409 and 1 - 2 separate columns
code.job1$job1<- as.character(code.job1$job1)
a<-strsplit(code.job1$job1[1],split='[.]')[[1]]
paste(a[1],a[2],sep='.')
code.job1$split<-sapply(code.job1$job1,FUN=function(x){strsplit(x,split='[.]')[[1]]})
code.job1$first<-sapply(code.job1$split,FUN=function(x){paste(x[1],x[2],sep=".")})
code.job1$second<-sapply(code.job1$split,FUN=function(x){x[3]})
# need to codify 'first' and 'second' columns separately, then combine
#first do 'first' column
First<- data.frame(first=unique(code.job1$first))
First$code<- paste('J', sample(1000:3000, nrow(First), replace=F), sep="")
#now code for 'second' column
Second<- data.frame(second=unique(code.job1$second))
Second$second<- as.integer(as.character(Second$second))
Second<- Second[order(Second$second),]
Second<- data.frame(second=Second)
Second$letter<- NULL
LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
Second$letter<- c("",LETTERS702[1:nrow(Second)-1])
#create another column saying 'Y or N' to milestone
Second$Milestone<- ifelse(Second$second>1, 'Y', 'N')
#compile First and Second into original code.job1
code.job1<- code.job1[,-2]
code.job1<-merge(code.job1, First, by='first', all.x=TRUE)
code.job1<-merge(code.job1, Second, by='second', all.x=TRUE)
code.job1<- transform(code.job1, code2 = paste(code, letter, sep=""))
code.job1<-code.job1[order(code.job1$job1),]
code.job1<- code.job1[,c(3,6,7)]

#second type of job number! hundred's at the end of code
#code.job2 - split up job number into 2010.409 and 1 - 2 separate columns
code.job2$job2<- as.character(code.job2$job2)
a<-strsplit(code.job2$job2[1],split='[.]')[[1]]
paste(a[1],a[2],sep='.')
code.job2$split1<-sapply(code.job2$job2,FUN=function(x){strsplit(x,split='[.]')[[1]][1]})
code.job2$split2<-sapply(code.job2$job2,FUN=function(x){strsplit(x,split='[.]')[[1]][2]})
code.job2<-transform(code.job2, first= paste(split1,split2,sep="."))
code.job2$second<-sapply(code.job2$job2,FUN=function(x){strsplit(x,split='[.]')[[1]][3]})
code.job2<- code.job2[,-c(2:3)]
# need to codify 'first' and 'second' columns separately, then combine
#first do 'first' column
First2<- data.frame(first=unique(code.job2$first))
First2$code<- paste('J', sample(3001:5000, nrow(First2), replace=F), sep="")

#now code for 'second' column
Second2<- data.frame(second=unique(code.job2$second))
Second2$second<- as.integer(as.character(Second2$second))

Second2<-Second2[order(Second2$second),]
Second2<- data.frame(second=Second2)
Second2$letter<- NULL
#LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
Second2$letter<- c("A","B",LETTERS702[53:76],"C",LETTERS702[79:99],"D",LETTERS702[105:108],'E','F','FA','FB')
#create another column saying 'Y or N' to milestone
Second2$Milestone<- ifelse(nchar(Second2$letter)== 1, 'N', 'Y')
#compile First and Second into original code.job2

code.job2<-merge(code.job2, First2, by='first', all.x=TRUE)
code.job2<-merge(code.job2, Second2, by='second', all.x=TRUE)
code.job2<- transform(code.job2, code2 = paste(code, letter, sep=""))
code.job2<-code.job2[order(code.job2$job2),]
code.job2<- code.job2[,names(code.job2) %in% c('job2','Milestone', 'code2')]

#compile first half of job numbers to second half of job numbers
#make colnames the same before rbind
colnames(code.job2)[names(code.job2) %in% 'job2']<-'job1'
#final coded job number file:
code.job<- rbind(code.job1,code.job2)
write.csv(code.job,'code_job.csv')

#code Directors
code.direc<- data.frame(User=unique(all4$Director))

#code Project Engineer
code.PEng<- data.frame(User=unique(all4$Project.Engineer))

#code Engineer.2
code.Eng<- data.frame(User= unique(all4$Engineer.2))

#users from hours data
#load hours data for users
hours<- read.csv('hours0212.csv')
code.users<- data.frame(User= unique(hours$User))

#merge all Directors, Project Engineers, Engineer.2, Users together
users<- merge(code.direc, code.PEng, by='User', all=TRUE)
users<-merge(users, code.Eng, by= 'User', all=TRUE)
users<-merge(users, code.users, by= 'User', all=TRUE)
#assign everyone a code, use S for staff
users$code<- paste('S', sample(100:200, nrow(users), replace=F), sep="")
#write out everyone's position
users$Position<- c('Director', 'Director', 'Director','Senior Professional','Director','Director','Director','Principal Professional',
                   'Senior Professional','Professional','Senior Professional','Principal Professional',
                   'Principal Professional', 'Professional','Senior Professional','Senior Professional',
                   'Grad Professional', 'Grad Professional','Grad Professional','Grad Professional',
                   'Senior Professional','Senior Professional','Senior Professional','Senior Professional',
                   'Professional','Senior Technical','Professional','Professional','Contract Technical',
                   'Professional','NA','Grad Professional', 'Grad Professional', 'Grad Professional','Professional',
                   'Senior Technical','Senior Professional','Senior Technical','Technical','Grad Professional',
                   'Senior Technical', 'NA','Professional','Grad Professional', 'Senior Technical','Professional',
                   'NA', 'Technical','Professional', 'NA', 'Technical','Admin','Technical','Senior Technical',
                   'Technical','Technical', 'Senior Technical','Technical','NA','Senior Technical','Senior Technical',
                   'Technical', 'Professional','Technical', 'Technical','Senior Technical','Technical','Technical',
                   'Senior Technical','NA')
users[is.na(users$User),]$code<- 'NA'
write.csv(users,'code_users.csv')

# delete Job.Name, Opportunity, Job.Address, Client.Company, Suburb

all5a<-read.csv('all5a.csv')[,-1]
all5a<- all5a[!(names(all5a) %in% c('Job.Name', 'Opportunity', 'Client.Company','Job.Address',
                                 'Suburb'))]
#substitute all the codes in
#client2
code.client2<- read.csv('Code_client2.csv')
all5a<- merge(all5a, code.client2[,2:3], by= 'client2', all.x=TRUE)
colnames(all5a)[names(all5a) %in% 'code']<-'code.client'
#sub in client contact
code.contact<- read.csv('code_contact.csv')
all5a<- merge(all5a, code.contact[,2:3], by.x= 'Client.Contact', by.y= 'contact', all.x=TRUE)
colnames(all5a)[names(all5a) %in% 'code']<-'code.contact'
#sub in job number
code.job<- read.csv('code_job.csv')[,-1]
all5a<- merge(all5a, code.job, by.x= 'Job.Number', by.y= 'job1', all.x=TRUE)
colnames(all5a)[names(all5a) %in% 'code2']<-'code.jobnum'
#code users into.. 'Director', 'Project.Engineer', 'Engineer.2'
code.staff<- read.csv('code_users2.csv')[,-c(1,5)]
all5a<- merge(all5a, code.staff, by.x= 'Director', by.y= 'User', all.x=TRUE)
colnames(all5a)[names(all5a) %in% 'code']<-'code.director'
all5a<- all5a[,!(names(all5a) %in% 'Position')]
all5a<- merge(all5a, code.staff, by.x= 'Project.Engineer', by.y= 'User', all.x=TRUE)
colnames(all5a)[names(all5a) %in% 'code']<-'code.ProjEng'
colnames(all5a)[names(all5a) %in% 'Position']<-'ProjEng.Pos'
all5a<- merge(all5a, code.staff, by.x= 'Engineer.2', by.y= 'User', all.x=TRUE)
colnames(all5a)[names(all5a) %in% 'code']<-'code.Eng2'
colnames(all5a)[names(all5a) %in% 'Position']<-'Eng2.Pos'
#delete columns - Director, Project.Engineer, Engineer.2
all5a<- all5a[,!(names(all5a) %in% c('Director', 'Project.Engineer', 'Engineer.2'))]

#investigate Parent.Job
invest<- all5a[!(is.na(all5a$Parent.Job)),(names(all5a) %in% c('Parent.Job', 'Job.Number'))]
#just delete Parent.Job - milestones tell you the same thing
all5a<- all5a[,!(names(all5a) %in% c('client2', 'Job.Number', 'Client.Contact', 'Parent.Job', 'caredfor'))]
all5a<- arrange(all5a, Start.Date)
write.csv(all5a, 'all6.csv')


##### NEW ENGINEERED INVOICING VARIABLES!! #######

#merge new coded invoicing engineered variables from invoices_eng.csv'
all6<- read.csv('all6.csv')[,-1]
inv.eng<- read.csv('invoices_eng.csv')[,-1]
all6a<- merge(all6, inv.eng %>% select(mlsto, num.inv, mean.inv, Inv.freq, num.neginv) %>% unique, 
              by='mlsto', all.x=T)
all6a<- merge(all6a, inv.eng %>% select(code.client, client.meaninv, client.invfreq, client.neginv, client.numinv, client.totinv) %>% unique,
              by='code.client', all.x=T)
test<- merge(all6 %>% select(mlsto), inv.eng %>% select(mlsto), by= 'mlsto', all.x=TRUE, all.y=TRUE)
write.csv(all6a,'all6a.csv')
all6a<- read.csv('all6a.csv')



### grep through Role and alter JD.Second!!!!!!!!!!!!!!!!!!!!!!!!!!!

all6a$Role %>% head
all6a$Role<- as.character(all6a$Role)
#look at all entries in Role with less than 10 characters
check<- sapply(all6a$Role %>% as.character, FUN=function(x) {nchar(x)}) %>% as.data.frame
check$Role<- all6a$Role
colnames(check)[names(check) %in% '.']<-'char'
check %>% unique %>% filter(char==1)
check %>% unique %>% filter(char==5)
check$Discipline<- all6a$Discipline
check$JD.Second<- all6a$JD.Second
check$mlsto<- all6a$mlsto
check$Job.Name<- all5a$Job.Name

#anything with nchar<=4 should be 'NA'
RN<- check[check$char<=4,] %>% rownames
all6a[RN,]$Role <- NA
all6a$Role <- droplevels(all6a$Role)

#now lets view remaining Role entries
all6a %>% filter(!is.na(Role)) %>% select(Role, JD.Second) %>% head
all6a$Role<- as.factor(all6a$Role)


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
struc.list<- list(svec1, svec3, svec11, svec13, svec14, svec15, svec16,svec17, svec18, svec19, svec20, svec21, svec22,
                  svec23, svec24, svec25, svec26, svec27, svec28, svec36, svec37, svec38, svec39, svec40, svec41, svec42, svec43, svec45, svec2)      
struc.title<- c(1,3,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,36,37,38,39,40,41,42,43,45,2)
civ.list<- list(svec44, svec48)
civ.title<- c(44,48)
nstruc.list<- list(svec5, svec6, svec8, svec29, svec30, svec31, svec32, svec33, svec34, svec35)
nstruc.title<- c(5,6,8,29,30,31,32,33,34,35)
any.list<- list(svec4, svec7, svec9, svec10, svec12, svec46, svec47)
anylist.title<- c(4,7,9,10,12, 46,47)

# function for greppipng through each vector in the list of vectors. Also calls the vector referring to the title in sec. Also calls up 
#the discipline
all6a$Role.JD<- rep(NA,nrow(all6a))
grepster<- function(vlist = any.list, titles= anylist.title, Disc= c('Structural', 'Civil','Environmental Planning','Water','NA')){
        for( i in 1:length(vlist)){
                all6a$Role.JD = ifelse(grepl(paste(vlist[[i]],collapse="|"), all6a$Role, ignore.case=TRUE)
                                         & all6a$Discipline %in% Disc, colnames(sec)[titles[i]], all6a$Role.JD)               
        } 
        all6a$Role.JD
}

all6a$Role.JD<- grepster(any.list, anylist.title, c('Structural', 'Civil','Environmental Planning','Water','NA') )
all6a$Role.JD<- grepster(vlist= struc.list, titles= struc.title, Disc= c('Structural'))
all6a$Role.JD<- grepster(civ.list, civ.title, c('Civil') )
all6a$Role.JD<- grepster(nstruc.list, nstruc.title, c('Civil','Environmental Planning','Water','NA') )

#worked!
#now compare when JD.Second differs from Role.JD
# all6a %>% filter(!is.na(Role.JD), !is.na(JD.Second),!(Role.JD==JD.Second)) %>% select(Role, Role.JD, JD.Second, Discipline,mlsto) %>% View()

#2014.371.3 - keep JD.Second
#2014.117.3 - keep JD.Second
#2014.414.3 - keep JD.Second
#2009.266 - keep JD.Second
#2014.538.2 - keep JD.Second
#2014.225.3 - keep JD.Second
#2005.106 - keep JD.Second
#2014.169.3 - keep JD.Second
#2014.279.6 - keep JD.Second
#2013.370.3 - keep JD.Second
#2014.134.3 - keep JD.Second
#2014.360.3 - keep JD.Second
#2012.297.3 - keep JD.Second
#2014.082.3 - keep JD.Second
#2007.236 - keep JD.Second
#2014.540.3 - keep JD.Second
#2014.562.3 - keep JD.Second
#2014.197.3 - keep JD.Second
#2009.266 - erosion & sediment

all6a$JD.Second<- as.character(all6a$JD.Second)
all6a$Role.JD<- as.character(all6a$Role.JD)
all6a$JD.Second<- ifelse(is.na(all6a$JD.Second), all6a$Role.JD, all6a$JD.Second)

no.replace= c('2014.371.3','2014.117.3', '2014.414.3','2009.266', '2014.538.2','2014.225.3','2005.106','2014.169.3','2014.279.6',
              '2013.370.3','2014.134.3','2014.360.3','2012.297.3','2014.082.3','2007.236','2014.540.3','2014.562.3','2014.197.3')
all6a$JD.Second<- ifelse(!is.na(all6a$JD.Second) & !is.na(all6a$Role.JD) & !(all6a$JD.Second==all6a$Role.JD) & !(all6a$mlsto %in% no.replace), 
                         all6a$Role.JD, all6a$JD.Second)
all6a<- all6a %>% select(-Role.JD)
all6a$JD.Second<- as.factor(all6a$JD.Second)

write.csv(all6a,'all6b.csv')

#go to 1st_stats.R
