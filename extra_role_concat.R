Df<-read.csv('extra_info.csv')

concat<-function(Data=Df){
        Ans=data.frame(matrix(0,nrow=length(na.omit(unique(Data$jobno.shift))),ncol=2))
        Is.NA = is.na(Data[,'jobno.shift'])
        Index.1 = data.frame(RN = 1:length(Is.NA), Is.NA)
        Index.F = Index.1[Index.1$Is.NA == FALSE,]
        Ans[,1]=Data[Index.F[,1],4]
        for (i in 1:(nrow(Index.F)-1)){
                SS1 = Data[Index.F$RN[i]:(Index.F$RN[i+1]-1),]
                Ans[i,2]=paste(SS1$Role,collapse='. ')
        SS2=Data[Index.F$RN[nrow(Index.F)]:nrow(Data),]
        Ans[nrow(Index.F),2]=paste(SS2$Role,collapse='. ')
        }
        return(Ans)
}

concat<-concat(Data=Df)
colnames(concat)[colnames(concat)=="X1"] <- "Job.Number"
head(concat)
#try merging with all
