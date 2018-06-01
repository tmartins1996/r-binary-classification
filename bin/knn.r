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

fields <- c("JobLevel","NumCompaniesWorked","TenureWorking","TenureRole",
            "LastPromotion","JobDedication","FacilitiesSatisfaction","Age",
            "JobTypeLevels" ,"isAfterHours","isChurn")
normalizedDataset <- normalizedDataset[ , (names(normalizedDataset) %in% fields)]

# Prep Training and Test data.
set.seed(333)
trainingRowIndex <- sample(1:nrow(normalizedDataset), 0.7*nrow(normalizedDataset))
trainData <- normalizedDataset[trainingRowIndex, ]
testData <- normalizedDataset[-trainingRowIndex, ]

train_labels <- as.factor(trainData$isChurn)
test_labels <- as.factor(testData$isChurn)


drops <- c("isChurn")
trainData <- trainData[ , !(names(trainData) %in% drops)]
testData <- testData[ , !(names(testData) %in% drops)]

knn_pred <- knn(train = trainData, test = testData, 
                     cl = train_labels, k = 1)


caret::confusionMatrix(knn_pred, test_labels, positive = '1',
                       dnn=c("Pred","Actual"))

