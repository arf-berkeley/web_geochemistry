get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
        os <- sysinf['sysname']
        if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
        os <- .Platform$OS.type
        if (grepl("^darwin", R.version$os))
        os <- "osx"
        if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
}

list.of.packages <- c("pbapply", "reshape2", "TTR", "dplyr",  "random",  "randomForest", "caret", "doSNOW", "doParallel", "parallel", "foreach", "nnet", "neuralnet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/", dep = TRUE)

my.cores <- if(parallel::detectCores()>=3){
    paste0(parallel::detectCores()-2)
} else if(parallel::detectCores()<=2){
    "1"
}

library(caret)
library(randomForest)


dataClassifyTree <- function(data, Class, Variables, split=0.8, foresttrees=20, foresttry=5, forestmetric="Accuracy", foresttrain="repeatedcv", forestnumber=12){
    
    data <- data[,c(Class, Variables)]
    data <- data[complete.cases(data),]
    
    a <- createDataPartition(data[,Class], p = split, list=FALSE)
    training <- data[a,]
    test <- data[-a,]
    
    y_train <- as.factor(training[,Class])
    x_train <- training[,Variables]
    
    rf.grid <- expand.grid(.mtry=foresttry)
    
    cl <- if(get_os()=="windows"){
        parallel::makePSOCKcluster(as.numeric(my.cores))
    } else if(get_os()!="windows"){
        parallel::makeForkCluster(as.numeric(my.cores))
    }
    doParallel::registerDoParallel(cl)
    
    rf_model <- caret::train(x=x_train, y=y_train, method="rf", type="Classification",
    trControl=trainControl(method=foresttrain, number=forestnumber, verboseIter = TRUE), ntree=foresttrees,  tuneGrid=rf.grid,
    prox=TRUE,allowParallel=TRUE, metric=forestmetric, na.action=na.omit, importance=TRUE, trim=TRUE)
    
    rf_pred <- predict(rf_model, newdata = test)
    
    Validation <- table(rf_pred==test[,Class])
    
    parallel::stopCluster(cl)
    
    model.list <- list(ModelData=list(Model.Data=data, Class=Class, Variables=Variables), Model=rf_model, Validation=Validation)
    
    return(model.list)
    
}


andes.obsidian <- read.csv(file="data/obsidian-NAA-database.csv", na.strings=c(""," ","NA"))


andes.tree <- dataClassifyTree(data=andes.obsidian, Class="Source.Name", Variables=c("Rb", "Sr", "Yb", "Zr", "Th"))

andes.tree$Model
andes.tree$Validation
