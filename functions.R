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


# check number of samples required to achieve power of 0.8

n.samples = function(data = results.core$diff, type.calc = 'one.sample', alt = 'greater'){
        if(type.calc == "one.sample"){
                d.calc = abs(mean(data))/sd(data)
                pow_pow = pwr.t.test(n = NULL, d = d.calc , sig.level = 0.05,
                                     power =.8, type = c(type.calc), 
                                     alternative = alt)
                return(ceiling(pow_pow$n))  
        }
        
        if(type.calc == 'two.sample'){
                d.calc = abs(mean(data[[1]])-mean(data[[2]]))/
                        (sqrt((sd(data[[1]])^2+sd(data[[2]])^2)/2))
                pow_pow = pwr.t.test(n = NULL, d = d.calc , sig.level = 0.05,
                                     power =.8, type = c(type.calc), 
                                     alternative = 'two.sided')
                return(ceiling(pow_pow$n)) 
        }
        
}



# perform 5-fold cross validation on train set to create complete train set of predictions for a method
# output is a vector of predictions for the method

full.predict <- function(df.pred = all10mice, method = "log", x.folds = 5, manual.seed = 100){
        
        #set up 10 fold cv using caret function
        set.seed(manual.seed)
        folds <- createFolds(df.pred$f.rpdol, k = x.folds, list = T, returnTrain = F)
        #now have folds[[1]] through folds[[10]] list of rows to exclude
        
        #create NULL dataframe with row names from folds
        method.pred <- data.frame('rownames' = unlist(folds))
        temp.pred = NULL
        
        
        #define formula
        if(method %in% c('log','rf')){
                formula <-"f.rpdol ~ Discipline + pc.pro + b.timespan.cbrt + no.users + b.inv.log + client.totinv.log + Business + majority.pos + pc.majpos.log + JD.Second + Billing.Type"
        } 
        
        if(method == 'boost') {
                #no JD.Second
                formula <-"b.rpdol ~ Discipline + pc.pro + b.timespan.cbrt + no.users + b.inv.log + client.totinv.log + Business + majority.pos + pc.majpos.log"
        }
        
        #loop over each fold, and compile predictions
        for (j in 1:length(folds)) {
                
                #turn predictors and response into 'ordered' variables for roc() to work
                test<- df.pred[folds[[j]],]
                levels(test$f.rpdol) <- c('profit', 'loss')
                test$f.rpdol <- ordered(test$f.rpdol)
                
                if(method %in% 'rf'){
                        fit <- randomForest(as.formula(formula), data = df.pred[-folds[[j]],], 
                                            mtry = 4, ntree = 1000)
                        pred <- predict(fit, test, type = "prob")
                        pred <- pred[,2]
                        
                }
                
                if(method %in% 'log'){
                        fit<- glm(as.formula(formula), 
                                  data = df.pred[-folds[[j]],], family = binomial())
                        pred <- predict(fit, test,
                                        type = "response")
                } 
                if(method %in% 'boost'){
                        fit <- gbm(as.formula(formula), data = df.pred[-folds[[j]],],
                                   distribution = "bernoulli", n.trees = 10000,
                                   shrinkage = 0.001, interaction.depth = 5,
                                   n.minobsinnode = 20)
                        
                        pred <- predict(fit, test, n.tree = 10000, type = "response")
                }
                
                
                temp.pred <- c(temp.pred, pred)
                
                #print j value
                cat(j, " ")
                
        }
        
        method.pred[, method] = temp.pred
        return(method.pred)
        
}

#assemble dataset with individual model results as columns
assemble <- function(methods = c("log", "rf", "boost"), df.assem = all10mice, x.folds.assem = 5,
                     manual.seed.assem = 100){
        
        joined = NULL
        
        #create empty vectors  - one for each method
        for(i in seq_along(methods)){
                temp = full.predict(df = df.assem, method = methods[i], x.folds = x.folds.assem,
                                    manual.seed = manual.seed.assem)
                if(is.null(joined)){
                        joined = temp
                } else{
                        joined = full_join(joined, temp)
                }
                
        }
        
        df.blend = full_join(df.assem %>% mutate(rown = 1:nrow(df.assem)), joined, 
                                by = c('rown'='rownames'))
        df.blend <- df.blend %>% select(-rown)
        
        return(df.blend)
        
}


iter.auc <- function(mult =2, x.folds.iter = 5, 
                     methods = c("orig.log","average","simp.boost",
                                 "simp.log", "orig.rf", "orig.boost"),
                     df.iter = all10.blend) {
        
        #create test and train set: 0.75/0.25
        pred.list <- vector(mode = "list", length = length(methods))
        names(pred.list) = methods
        
        #create model averaged column
        if("average" %in% methods){
        df.iter = mutate(df.iter, model.av = (log + rf + boost)/3) 
        }
        
        #set up 10 fold cv using caret function
        folds <- createFolds(df.iter$f.rpdol, k = x.folds.iter, list = T, returnTrain = F)
        #now have folds[[1]] through folds[[10]] list of rows to exclude
        if(mult > 1){
                for(k in 2:mult){                                
                        set.seed(sample(1:1000,1))
                        folds.temp = createFolds(df.iter$f.rpdol, k = x.folds.iter, list = T, returnTrain=F)
                        folds = append(folds, folds.temp)
                }
        }
        
        for(k in seq_along(folds)){
                
                train = df.iter[-folds[[k]],]
                test = df.iter[folds[[k]],]
                
                if ("simp.log" %in% methods) {
                        print("simp.log")
                        fit <- glm(f.rpdol ~ log + rf + boost,
                                   data = train,
                                   family = binomial())
                        
                        pred <- predict(fit, test, type = "response")
                        pred.list[["simp.log"]] = rbind(pred.list[["simp.log"]],
                                                        data.frame("index" = folds[[k]],
                                                                   "pred" = pred,
                                                                   "iteration" = paste("iter",ceiling(k/x.folds.iter),m, sep = "")))
                        
                }
                
                if ("simp.boost" %in% methods) {
                        print("simp.boost")
                        fit <- gbm(b.rpdol ~ norm.log + norm.rf + norm.boost, data = train,
                                   distribution = "bernoulli", n.trees = 6000,
                                   shrinkage = 0.0005, interaction.depth = 6,
                                   n.minobsinnode = 10)
                        
                        pred <- predict(fit, test, n.tree = 6000, type = "response")
                        pred.list[["simp.boost"]] = rbind(pred.list[["simp.boost"]],
                                                        data.frame("index" = folds[[k]],
                                                                   "pred" = pred,
                                                                   "iteration" = paste("iter",ceiling(k/x.folds.iter),m, sep = "")))
                        
                }

                
                if ("orig.log" %in% methods) { 
                        print("orig.log")
                        pred <- test$log
                        pred.list[["orig.log"]] = rbind(pred.list[["orig.log"]],
                                                        data.frame("index" = folds[[k]],
                                                                   "pred" = pred,
                                                                   "iteration" = paste("iter",ceiling(k/x.folds.iter),m, sep = "")))
                        
                }
                
                if ("orig.rf" %in% methods) { 
                        print("orig.rf")
                        pred <- test$rf
                        pred.list[["orig.rf"]] = rbind(pred.list[["orig.rf"]],
                                                        data.frame("index" = folds[[k]],
                                                                   "pred" = pred,
                                                                   "iteration" = paste("iter",ceiling(k/x.folds.iter),m, sep = "")))
                        
                }
                
                if ("orig.boost" %in% methods) { 
                        print("orig.boost")
                        pred <- test$boost
                        pred.list[["orig.boost"]] = rbind(pred.list[["orig.boost"]],
                                                        data.frame("index" = folds[[k]],
                                                                   "pred" = pred,
                                                                   "iteration" = paste("iter",ceiling(k/x.folds.iter),m, sep = "")))
                        
                }
                
                if ("average" %in% methods) {
                        print("average")
                        pred <- test$model.av
                        pred.list[["average"]] = rbind(pred.list[["average"]],
                                                        data.frame("index" = folds[[k]],
                                                                   "pred" = pred,
                                                                   "iteration" = paste("iter",ceiling(k/x.folds.iter),m, sep = "")))
                }
                
                
                #print AUC's
                cat(k, " ")
                
        }
        
        return(pred.list)
        
}

saveit <- iter.auc(mult =2, x.folds.iter = 5,methods = c("orig.log","average","simp.log"),
                     df.iter = all10.blend)

dontknow = map(saveit, function(x) spread(x, iteration, pred))



