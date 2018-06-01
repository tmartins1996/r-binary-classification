# -*- coding: utf-8 -*-

#' Created on Fri Apr 13 15:38:28 2018
#' R version 3.4.3 (2017-11-30)
#' 
#' @group   Group 2, DM2 2018 Semester 2
#' @author: Martins T.
#' @author: Mendes R.
#' @author: Santos R.
#'



# Libs --------------------------------------------------------------------
options(warn=-1)
source("src/packages.r")
include_packs(c("dygraphs","d3heatmap","rockchalk","forcats","rJava",
                "xlsxjars","xlsx","tidyverse","stringi","stringr","ggcorrplot",
                "sm","lubridate","magrittr","ggplot2","openxlsx","RColorBrewer",
                "psych","treemap","data.table","pROC","class",'gmodels','klaR',
                "C50","caret",'gmodels',"DMwR","recipes","epiR","pubh"))


# Load data ---------------------------------------------------------------
#Load normalized data xlsx
source("src/wrangling.r")
numericDataset <- xlsx::read.xlsx('datasets/numericDataset.xlsx',1, header= TRUE)
temp<-numericDataset
numericDataset<-temp

numericDataset$isChurn <- ifelse(numericDataset$isChurn==1,"Yes","No")

#Save test_labels
numericDataset$isChurn <- as.factor(numericDataset$isChurn)

set.seed(375)


# ## now using SMOTE to create a more "balanced problem"
newData <- DMwR::SMOTE(isChurn ~ ., numericDataset, perc.over = 300,perc.under=0)
# count(newData$isChurn)
# 
numericDataset<-rbind(numericDataset,newData)
# count(numericDataset$isChurn)

#glmStepAIC

#Setting up CV
ControlParamteres <- trainControl(method="repeatedcv", 
                                  number=10,
                                  repeats=10,
                                  #sampling = "up",
                                  #verboseIter = FALSE,
                                  # metric = "ROC",
                                  savePredictions = TRUE,
                                  classProbs = TRUE)

#regLogistic
log_CV <- train(isChurn~.,
                data=numericDataset,
                method = "glm",
                preProc=c("center", "scale"),
                trControl = ControlParamteres)


log_CV

pred <- predict(log_CV) 

#predict prob, no response w/ factors


numericDataset$prob <-predict(log_CV, type = "prob")[2]

g <- roc(isChurn ~ prob$Yes, data =numericDataset )
dev.off(); plot(g, col = 4, lty = 1, 
                main = "ROC", asp = NA,  
                xlab = "Specificity (%)", 
                ylab = "Sensitivity (%)",
                print.auc=TRUE, type  = 'l', ps=1000)
legend(.35,.75, 
       legend=c("48 Variables"), 
       col=c("4"), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)



#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(pred,numericDataset$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))

draw_confusion_matrix_cv(cm)



lmFuncs$fit<-function (x, y, first, last, ...){   
  tmp <- as.data.frame(x)   
  tmp$y <- y   
  glm(y ~ ., data = tmp,family=binomial)   
}
#note that this lmFuncs is logistic regr by my overwrite function
rfe_controller <- rfeControl(functions=lmFuncs, method="repeatedcv", 
                             rerank = FALSE,repeats = 10, verbose = FALSE)

subsets <- c(5:15)


predictorss<-normalizedDataset[,1:48]
predictorss<- predictorss[,sample(ncol(predictorss))]


log_Profiler <- rfe(predictorss,normalizedDataset[,49], 
                    size=subsets, rfeControl = rfe_controller)
log_Profiler
plot(log_Profiler, type = c("g", "o"), col="blue")







ga_ctrl <- gafsControl(functions = rfGA,
                       method = "cv")
# , repeats = 1)

## Use the same random number seed as the RFE process
## so that the same CV folds are used for the external
## resampling. 
set.seed(10)
rf_ga <- gafs(x = normalizedDataset[,1:48], y = normalizedDataset[,49],
              iters = 15,
              method = "glm",
              gafsControl = ga_ctrl)
rf_ga
plot(rf_ga, type = c("g", "o"), col="blue")




pubh::contingency(isChurn~Age, data = normalizedDataset, method = "cohort.count")
  

