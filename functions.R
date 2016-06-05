#function for running caret..

caret.all <- function(method = "boost", df = train, formula = formula.rf, sample.frac = 0.75, seed = 100){
        # set seeds
        set.seed(seed)
        seeds <- vector(mode = "list", length = 51)
        for(i in 1:50) seeds[[i]] <- sample.int(1000, 22)
        ## For the last model:
        seeds[[51]] <- sample.int(1000, 1)
        
        train.c = sample_frac(df, sample.frac, replace = FALSE)
        test.c = setdiff(df, train.c)
        
        cv.Control <- trainControl(method = "cv",
                                   number = 5,
                                   seeds= seeds
                                   , summaryFunction= twoClassSummary,
                                   classProbs=TRUE
                                   )
        
        if(method == "rf"){
                mtry.Grid <- expand.grid(mtry = c(3,4,5,6,7))
                
                set.seed(2)
                gbmFit <- caret::train(as.formula(formula), data= train.c,
                                       method = "rf", trControl = cv.Control, verbose = FALSE,
                                       tuneGrid = mtry.Grid, ntree = 1000,
                                       metric = 'ROC', allowParallel = T)
        }
        
        if(method == "boost"){
                train.c$b.rpdol<- as.factor(train.c$b.rpdol)
                levels(train.c$b.rpdol)[levels(train.c$b.rpdol)=="0"] <- "profit"
                levels(train.c$b.rpdol)[levels(train.c$b.rpdol)=="1"] <- "loss"
                Grid <- expand.grid(shrinkage = c(.0005),
                                         n.trees = c(6000),
                                    interaction.depth = c(3,4,5,6),
                                    n.minobsinnode = c(10,20,30,40))
                
                # , interaction.depth = c(1,2,3,4,5),
                # n.minobsinnode = c(10,20,30)
                
                set.seed(2)
                gbmFit <- caret::train(as.formula(formula), data= train.c,
                                method = "gbm", trControl = cv.Control, verbose = FALSE,
                                bag.fraction = 0.5, tuneGrid = Grid,
                                metric = 'ROC',
                                na.action = na.pass)
        }
        
        plot(gbmFit)
        return(gbmFit)
        
}

cfor.plotsave<- function(df= vars, name= 'cfor_blend', formular = formula.rf){
                
                #run cforest
                cfor.var1= cforest(as.formula(formular),
                                   data= df)
                #calculate variable importances
                var.imp= varimp(cfor.var1) %>% as.data.frame
                colnames(var.imp)[names(var.imp) %in% '.']= 'imp'
                var.imp$var = rownames(var.imp)
                #reorder levels for bar plot
                var.imp<- within(var.imp, 
                                 var <- factor(var,levels=var.imp[order(-abs(var.imp$imp)),]$var))
                
                q= ggplot(data=var.imp, aes(x=var, y=imp)) + theme(axis.text.x=element_text(angle=45,hjust=1))
                r=q + geom_bar(stat='identity') + 
                        labs(title = "Cforest variable importance: blend")
                #save plot
                # ggsave(plot=r,
                #        filename=paste(name,i,'.png',sep=''),
                #        path= 'C:/Users/n9232371/Documents/Consultbusiness/barplots/cforests/')
                
                print(r)
                
                #save datafraem for cforest plot
                saveRDS(var.imp, "/Users/yam/Documents/github/consult/finalwriteup/report_data/cfor_blend_varimp.rds")
                
        
}
