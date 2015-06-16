###EXPLORATION - new file
#plot invoiced vs hours spent?
ggplot(all,aes(Invoiced,Timesheet.Hours))+geom_point(aes(colour=Discipline))
ggplot(all[all$Invoiced<25000 & all$Invoiced>0,],aes(Invoiced,Timesheet.Hours))+geom_point(aes(colour=Discipline))
#plot invoiced=0 jobs
ggplot(all,aes(Tot.Invoiced,Timesheet.Hours))+geom_point(aes(colour=Discipline))
#get a feel for how many jobs we didn't invoice for
nrow(all[all$Invoiced==0,])
nrow(all[all$Invoiced>0,])
nrow(all[all$Invoiced==0 & all$Timesheet.Hours==0,])
#plot year vs profit
ggplot(all,aes(x=Year,y=Profit))+geom_point()