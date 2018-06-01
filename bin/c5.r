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
                "C50","caret",'gmodels'))

normalizedDataset <- xlsx::read.xlsx('datasets/normalizedDataset.xlsx',1, header= TRUE)


set.seed(333)
trainingRowIndex <- sample(1:nrow(normalizedDataset), 0.7*nrow(normalizedDataset))
trainData <- normalizedDataset[trainingRowIndex, ]
testData <- normalizedDataset[-trainingRowIndex, ]
prop.table(table(trainData$isChurn))
prop.table(table(testData$isChurn))

train_labels <- as.factor(trainData$isChurn)
test_labels <- as.factor(testData$isChurn)


drops <- c("isChurn")
trainData <- trainData[ , !(names(trainData) %in% drops)]
testData <- testData[ , !(names(testData) %in% drops)]



c5_model <- C5.0(trainData, train_labels)
c5_model
summary(c5_model)
c5_pred <- predict(c5_model, testData)
gmodels::CrossTable(test_labels, c5_pred,
                    prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                    dnn = c('actual default', 'predicted default'))

caret::confusionMatrix(c5_pred, test_labels, positive = "1")


