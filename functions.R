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
                Grid <- expand.grid(shrinkage = c(.0005),
                                         n.trees = c(6000),
                                    interaction.depth = c(3,4,5),
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
        
}