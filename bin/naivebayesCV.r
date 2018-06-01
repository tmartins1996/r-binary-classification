# -*- coding: utf-8 -*-

#' Created on Fri Apr 13 15:38:28 2018
#' R version 3.4.3 (2017-11-30)
#' 
#' @group   Group 2, DM2 2018 Semester 2
#' @author: Mendes R.
#' @author: Santos R.
#' @author: Martins T.
#'

# Libs --------------------------------------------------------------------
options(warn=-1)
source("src/packages.r")
include_packs(c("dygraphs","d3heatmap","rockchalk","forcats","rJava",
                "xlsxjars","xlsx","tidyverse","stringi","stringr","ggcorrplot",
                "sm","lubridate","magrittr","ggplot2","openxlsx","RColorBrewer",
                "psych","treemap","data.table","pROC","class",'gmodels','klaR',
                "C50","caret",'gmodels',"naivebayes"))

normalizedDataset <- xlsx::read.xlsx('datasets/normalizedDataset.xlsx',1, header= TRUE)


# Prep Training and Test data.
set.seed(333)
trainingRowIndex <- sample(1:nrow(normalizedDataset), 0.7*nrow(normalizedDataset))
trainData <- normalizedDataset[trainingRowIndex, ]
testData <- normalizedDataset[-trainingRowIndex, ]



trainData$isChurn <- ifelse(trainData$isChurn==1,"Yes","No")

testData$isChurn <- ifelse(testData$isChurn==1,"Yes","No")


test_labels <- as.factor(testData$isChurn)


drops <- c("isChurn")
testData <- testData[ , !(names(testData) %in% drops)]




ControlParamteres <- trainControl(method = "cv",
                                  number = 10,
                                  savePredictions = TRUE,
                                  classProbs = TRUE)



nb_CV <- train(isChurn~.,
                data=trainData,
                method = "naive_bayes",
                trControl = ControlParamteres)


nb_CV

predictions <- predict(nb_CV,testData)


caret::confusionMatrix(predictions, test_labels, positive = 'Yes',
                       dnn=c("Pred","Actual"))

