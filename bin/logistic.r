# -*- coding: utf-8 -*-

#' Created on Fri Apr 13 15:38:28 2018
#' R version 3.4.3 (2017-11-30)
#' 
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

# Load data ---------------------------------------------------------------
#Load normalized data xlsx
source("src/wrangling.r")
normalizedDataset <- xlsx::read.xlsx('datasets/normalizedDataset.xlsx',1, header= TRUE)

#Size - 50 independents variables 
dim(normalizedDataset)


# Data Partition ----------------------------------------------------------
set.seed(375); trainingRowIndex <- sample(1:nrow(normalizedDataset), 
                                          0.69*nrow(normalizedDataset))
#1000 records
trainData <- normalizedDataset[trainingRowIndex, ]
#remaining 450 records
testData <- normalizedDataset[-trainingRowIndex, ]

#checking propotion of target var
trainData$isChurn %>%
  table() %>% prop.table() %>% {. * 100} %>% round(0)
testData$isChurn %>% 
  table() %>% prop.table() %>% {. * 100} %>% round(0)

#Save test_labels
test_labels <- as.factor(testData$isChurn)

#drop target from testData, not mandatory 
drops <- c("isChurn")
testData <- testData[ , !(names(testData) %in% drops)]



# Logistic Regression -----------------------------------------------------

# Build Logistic Model
logitmod <- glm(isChurn ~. , family = "binomial", data=trainData)

summary(logitmod)

#predict testData
testData$prob <- predict(logitmod, testData, type = "response")

#plot roc and calculate auc
testData$isChurn <- test_labels
g <- roc(isChurn ~ prob, data =testData )
plot(g, col = 3, lty = 3, main = "ROC", xlim=c(1, 0))
print("All Variables");auc(g1)


# Recode factors
y_pred_num <- ifelse(testData$prob > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))

#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(y_pred, test_labels, positive = '1',
                       dnn=c("Pred","Actual"))
draw_confusion_matrix(cm)




