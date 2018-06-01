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
                "C50","caret",'gmodels',"DMwR","recipes","epiR","pubh","leaps",
                "MASS","autoimage","randomForest","settings","factoextra","dummies"))


# Load data ---------------------------------------------------------------
#Load normalized data xlsx
source("src/wrangling.r")
normalizedDataset <- xlsx::read.xlsx('datasets/normalizedDataset.xlsx',1, header= TRUE)

#backup set
temp<-normalizedDataset


#Logistic


###' Debug
normalizedDataset <- temp


include <- c("DistanceHomeOffice","isAfterHours",
             "RoleSatisfaction","isSingle",
             "isMarried","Age",
             "FacilitiesSatisfaction","avgSatisfaction",
             "TenureWorking","NumCompaniesWorked",
             "LastPromotion","MonthlyIncome","isDepartIT",
             "isChurn")


normalizedDataset <- normalizedDataset[ , (names(normalizedDataset) %in% include)]

correlation <- round(cor(normalizedDataset, method = "kendall"), 1)
png(filename="presentations/correlation.png",width=760, height=565)
p<-corr_plot(correlation)
p
dev.off()
p



# Data Partition 
set.seed(375); trainingRowIndex <- sample(1:nrow(normalizedDataset),
                                          0.69*nrow(normalizedDataset))
#1000 records
trainData <- normalizedDataset[trainingRowIndex, ]
#remaining 450 records
testData <- normalizedDataset[-trainingRowIndex, ]


#checking propotion of target var
normalizedDataset$isChurn %>%
  table() %>% prop.table() %>% {. * 100} %>% round(0)
trainData$isChurn %>%
  table() %>% prop.table() %>% {. * 100} %>% round(0)
testData$isChurn %>% 
  table() %>% prop.table() %>% {. * 100} %>% round(0)


png(filename="presentations/stratification.png",width=760, height=565)
p<-stratification(normalizedDataset,trainData,testData)
p
dev.off()
p

# # 
trainData$isChurn <- ifelse(trainData$isChurn==1,"Yes","No")
testData$isChurn <- ifelse(testData$isChurn==1,"Yes","No")


# 
# #Save test_labels
test_labels <- as.factor(testData$isChurn)
#trainData$isChurn<-as.factor(trainData$isChurn)

#drop target from testData, not mandatory 
drops <- c("isChurn")
testData <- testData[ , !(names(testData) %in% drops)]




#Setting up CV
ControlParamteres <- trainControl(method="cv", 
                                  number=10,
                                  #repeats=10,
                                  #sampling = "up",
                                  #verboseIter = FALSE,
                                  savePredictions = TRUE,
                                  classProbs = TRUE)

#regLogistic
log_CV <- train(isChurn~.,
                data=trainData,
                method = "glm",
                #preProc=c("center", "scale"),
                #metric="Accuracy",
                family = binomial(),
                trControl = ControlParamteres)



summary(log_CV)

#The same propotion of target variable is maintained 
summary(log_CV$finalModel$data$.outcome)



testData$pred <- predict(log_CV,testData,type = "raw")
testData$prob <-predict(log_CV, testData,type = "prob")[2]
testData$isChurn <- test_labels

g <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

p<-recordPlot()
plot(g, col = 4, lty = 1, 
     main = "ROC", asp = NA,  
     xlab = "Specificity (%)", 
     ylab = "Sensitivity (%)",
     print.auc=TRUE, type  = 'l', ps=1000)
legend(.40,.75, 
       legend=c("13 Predictors CV"), 
       col=c("4"), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)

png(filename="presentations/roc50PredictorsCV.png",width=760, height=565)
p
dev.off()

#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = '1',
                             dnn=c("Pred","Actual"))

png(filename="presentations/cm13PredictorsCV.png",width=760, height=565)
draw_confusion_matrix_cv(cm)
dev.off()
draw_confusion_matrix_cv(cm)

