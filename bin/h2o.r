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
                "MASS","gbm","autoimage","randomForest","settings","factoextra",
                "dummies","RRF","h2o","tidyquant","unbalanced"))


# Load data ---------------------------------------------------------------
#Load normalized data xlsx
source("src/wrangling.r")
normalizedDataset <- xlsx::read.xlsx('datasets/normalizedDataset.xlsx',1, header= TRUE)

#backup set
temp<-normalizedDataset



# Logistic CV 13 Predictors ----------------------------------------------


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



h2o.init()

# Data Partition 
set.seed(375); trainingRowIndex <- sample(1:nrow(normalizedDataset),
                                          0.69*nrow(normalizedDataset))
#1000 records
trainData <- normalizedDataset[trainingRowIndex, ]
#remaining 450 records
testData <- normalizedDataset[-trainingRowIndex, ]


# 
trainData$isChurn <- ifelse(trainData$isChurn==1,"Yes","No")
testData$isChurn <- ifelse(testData$isChurn==1,"Yes","No")


# 
# #Save test_labels
testData$isChurn <- as.factor(testData$isChurn)
trainData$isChurn<- as.factor(trainData$isChurn)





# #Save test_labels
test_labels <- as.factor(testData$isChurn)


#drop target from testData, not mandatory 
drops <- c("isChurn")
testData <- testData[ , !(names(testData) %in% drops)]




y <- "isChurn"
x <- setdiff(names(trainData), y)

train_h2o <- as.h2o(trainData)
test_h2o  <- as.h2o(testData)



dl <- h2o.deeplearning(x = x, 
                       y = y,
                      training_frame= train_h2o,
                      nfolds = 10, 
                      stopping_rounds = 10,
                      epochs=400,
                      overwrite_with_best_model = T,
                      activation = "Tanh",
                      input_dropout_ratio = .1,
                      hidden = c(10,10),
                      distribution = "AUTO",
                      stopping_metric = "logloss")


dl


predictions<- as.data.frame(predict(dl,test_h2o))

testData$pred<-as.factor(predictions$predict)
testData$isChurn<-test_labels
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))

draw_confusion_matrix_boot(cm)


perf_h2o <- h2o.performance(dl ,test_h2o ) 

# Plot ROC Curve
left_join(h2o.tpr(perf_h2o), h2o.fpr(perf_h2o)) %>%
  mutate(random_guess = fpr) %>%
  select(-threshold) %>%
  ggplot(aes(x = fpr)) +
  geom_area(aes(y = tpr, fill = "AUC"), alpha = 0.5) +
  geom_point(aes(y = tpr, color = "TPR"), alpha = 0.25) +
  geom_line(aes(y = random_guess, color = "Random Guess"), size = 1, linetype = 2) +
  theme_tq() +
  scale_color_manual(
    name = "Key", 
    values = c("TPR" = palette_dark()[[1]],
               "Random Guess" = palette_dark()[[2]])
  ) +
  scale_fill_manual(name = "Fill", values = c("AUC" = palette_dark()[[5]])) +
  labs(title = "ROC Curve", 
       subtitle = "Model is performing much better than random guessing") +
  annotate("text", x = 0.25, y = 0.65, label = "Better than guessing") +
  annotate("text", x = 0.75, y = 0.25, label = "Worse than guessing")

