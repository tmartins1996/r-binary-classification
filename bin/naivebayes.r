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
                "C50","caret",'gmodels'))

normalizedDataset <- xlsx::read.xlsx('datasets/normalizedDataset.xlsx',1, header= TRUE)



# Prep Training and Test data.
set.seed(333)
trainingRowIndex <- sample(1:nrow(normalizedDataset), 0.7*nrow(normalizedDataset))
trainData <- normalizedDataset[trainingRowIndex, ]
testData <- normalizedDataset[-trainingRowIndex, ]

train_labels <- trainData$isChurn
test_labels <- as.factor(testData$isChurn)

nb_model <- NaiveBayes(factor(isChurn) ~.  , data = trainData, fL = 1)

# make predictions

predictions <- predict(nb_model, testData)

#summarize results
caret::confusionMatrix(predictions$class, test_labels, positive = '1',
                       dnn=c("Pred","Actual"))

