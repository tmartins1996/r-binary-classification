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
                "MASS","gbm","autoimage","randomForest","settings","factoextra"
                ,"dummies","ROCR","neuralnet","sparseLDA","adabag","RWeka",
                "Metrics"))



# Load data ---------------------------------------------------------------
#Load normalized data xlsx
source("src/wrangling.r")
normalizedDataset <- xlsx::read.xlsx('datasets/normalizedDataset.xlsx',1, header= TRUE)

#backup set
temp<-normalizedDataset



# Logistic CV 7 Predictors ----------------------------------------------

###' Debug
normalizedDataset <- temp

include <- c("DistanceHomeOffice","isAfterHours",
             "isSingle","avgSatisfaction",
             "TenureWorking","NumCompaniesWorked",
             "isDepartIT","isChurn")


normalizedDataset <- normalizedDataset[ , (names(normalizedDataset) %in% include)]

correlation <- round(cor(normalizedDataset, method = "kendall"), 1)
png(filename="presentations/correlation7Log.png",width=1000, height=565)
corr_plot(correlation)
dev.off()

corr_plot(correlation)

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

png(filename="presentations/stratification7Log.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- Log 7 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- Log 7 predictors")


# target to yes/no
trainData$isChurn <- ifelse(trainData$isChurn==1,"Yes","No")
testData$isChurn <- ifelse(testData$isChurn==1,"Yes","No")



# #Save test_labels
test_labels <- as.factor(testData$isChurn)


#drop target from testData, not mandatory
drops <- c("isChurn")
testData <- testData[ , !(names(testData) %in% drops)]




#Setting up CV
ControlParamteres <- trainControl(method="cv", 
                                  number=10,
                                  #repeats=10,
                                  #sampling = "up",
                                  #verboseIter = FALSE,
                                  summaryFunction=twoClassSummary,
                                  savePredictions = TRUE,
                                  classProbs = TRUE)


#with no tuning parameters.  

#regLogistic
glm7 <- caret::train(isChurn~.,
                      data=trainData,
                      method = "glm",
                      #preProc=c("center", "scale"),
                      metric="ROC",
                      trControl = ControlParamteres)


glm7
summary(glm7)



testData$prob <- predict(glm7, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels

#Cutoff
reset.par()
plotCut(ROCR_test,"LOG 7")
abline(v = 0.618, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut7LogCV.png",width=760, height=565)
plotCut(ROCR_test,"LOG 7")
abline(v = 0.618, col="seagreen3", lwd=2, lty=8)
dev.off()

testData$pred <- ifelse(testData$prob$Yes > 0.618, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 7 Predictors", asp = NA,  
     xlab = "Specificity (%)", 
     ylab = "Sensitivity (%)",
     print.auc=TRUE, type  = 'l', ps=1000,las=1)
legend(.40,.8, 
       legend=c("Log CV"), 
       col=c("4"), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)
p<-recordPlot()

png(filename="presentations/roc7LogCV.png",width=760, height=565)
replayPlot(p)
dev.off()


#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))

png(filename="presentations/cm7LogCV.png",width=760, height=565)
draw_confusion_matrix_cv(cm,"- Log 7 predictors")
dev.off()

draw_confusion_matrix_cv(cm,"- Log 7 predictors")

save(glm7, file = "models/glm7.rda")


# SVM CV 7 Predictors ----------------------------------------------

###' Debug
normalizedDataset <- temp

include <- c("DistanceHomeOffice","isAfterHours",
             "isSingle","avgSatisfaction",
             "TenureWorking","NumCompaniesWorked",
             "isDepartIT","isChurn")

normalizedDataset <- normalizedDataset[ , (names(normalizedDataset) %in% include)]

correlation <- round(cor(normalizedDataset, method = "kendall"), 1)
png(filename="presentations/correlation7SVM.png",width=760, height=565)
corr_plot(correlation)
dev.off()

corr_plot(correlation)
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

png(filename="presentations/stratification7SVM.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- SVM 7 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- SVM 7 predictors")


# target to Yes/no
trainData$isChurn <- ifelse(trainData$isChurn==1,"Yes","No")
testData$isChurn <- ifelse(testData$isChurn==1,"Yes","No")



# #Save test_labels
test_labels <- as.factor(testData$isChurn)


#drop target from testData
drops <- c("isChurn")
testData <- testData[ , !(names(testData) %in% drops)]




#Setting up CV
ControlParamteres <- trainControl(method="cv", 
                                  number=10,
                                  #repeats=10,
                                  #sampling = "up",
                                  #verboseIter = FALSE,
                                  summaryFunction=twoClassSummary,
                                  savePredictions = TRUE,
                                  classProbs = TRUE)
set.seed(375)
svmGrid <- expand.grid(sigma= 2^c(-7:-2), C= 2^c(0:2))

#SVM
svm7 <- caret::train(isChurn~.,
                      data=trainData,
                      method = "svmRadial",
                      #preProc=c("center", "scale"),
                      metric="ROC",
                      #tuneLength = 5,
                      tuneGrid = svmGrid,
                      trControl = ControlParamteres)



svm7

png(filename="presentations/tune7SVMCV.png",width=760, height=565)
plot(svm7)
dev.off()

plot(svm7)


testData$prob <- predict(svm7, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels

#Cutoff
reset.par()
plotCut(ROCR_test,"SVM 7")
abline(v = 0.34, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut7SVMCV.png",width=760, height=565)
plotCut(ROCR_test,"SVM 7")
abline(v = 0.64, col="seagreen3", lwd=2, lty=8)
dev.off()

#Cutoff  
testData$pred <- ifelse(testData$prob$Yes >0.64, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g2 <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 7 Predictors", asp = NA,  
     xlab = "Specificity (%)", 
     ylab = "Sensitivity (%)",
     print.auc=TRUE, type  = 'l', ps=1000,las=1)
plot(g2, col = 3, lty = 1,
     print.auc=TRUE,
     print.auc.y = .45,
     add = TRUE, ps=1000)
legend(.40,.8, 
       legend=c("Log CV"), 
       col=c("4"), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)
legend(.40,.75, 
       legend=c("SVM CV"), 
       col=c(3), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)
p<-recordPlot()


png(filename="presentations/roc7SVMCV.png",width=760, height=565)
replayPlot(p)
dev.off()

#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))

png(filename="presentations/cm7SVMCV.png",width=760, height=565)
draw_confusion_matrix_cv(cm,"- SVM 7 predictors")
dev.off()

draw_confusion_matrix_cv(cm,"- SVM 7 predictors")


save(svm7, file = "models/svm7.rda")

# CART Bootstrap 7 Predictors ----------------------------------------------

###' Debug
normalizedDataset <- temp

include <- c("DistanceHomeOffice","isAfterHours",
             "isSingle","avgSatisfaction",
             "TenureWorking","NumCompaniesWorked",
             "isDepartIT","isChurn")


normalizedDataset <- normalizedDataset[ , (names(normalizedDataset) %in% include)]

correlation <- round(cor(normalizedDataset, method = "kendall"), 1)
png(filename="presentations/correlation7CART.png",width=760, height=565)
corr_plot(correlation)
dev.off()

corr_plot(correlation)
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

png(filename="presentations/stratification7CART.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- CART 7 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- CART 7 predictors")



# Yes/No target
trainData$isChurn <- ifelse(trainData$isChurn==1,"Yes","No")
testData$isChurn <- ifelse(testData$isChurn==1,"Yes","No")



# #Save test_labels
test_labels <- as.factor(testData$isChurn)


#drop target from testData, not mandatory
drops <- c("isChurn")
testData <- testData[ , !(names(testData) %in% drops)]




#Setting up CV
ControlParamteres <- trainControl(method="boot", 
                                  # number=10,
                                  #repeats=10,
                                  #sampling = "up",
                                  #verboseIter = FALSE,
                                  summaryFunction=twoClassSummary,
                                  savePredictions = TRUE,
                                  classProbs = TRUE)

set.seed(375)
tunegrid <- expand.grid(.maxdepth =c(2:25))
#CART
cart7 <- caret::train(isChurn~.,
                       data=trainData,
                       method = "rpart2",
                       #preProc=c("center", "scale"),
                       metric="ROC",
                       #tuneLength = 10,
                       tuneGrid=tunegrid,
                       trControl = ControlParamteres)


cart7

png(filename="presentations/tune7CART.png",width=760, height=565)
plot(cart7)
dev.off()

plot(cart7)


testData$prob <- predict(cart7, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels

#Cutoff
reset.par()
plotCut(ROCR_test,"CART 7")
abline(v = 0.29, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut7CART.png",width=760, height=565)
plotCut(ROCR_test,"CART 7")
abline(v = 0.29, col="seagreen3", lwd=2, lty=8)
dev.off()

testData$pred <- ifelse(testData$prob$Yes > 0.29, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g3 <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 7 Predictors", asp = NA,  
     xlab = "Specificity (%)", 
     ylab = "Sensitivity (%)",
     print.auc=TRUE, type  = 'l', ps=1000,las=1)
plot(g2, col = 3, lty = 1,
     print.auc=TRUE,
     print.auc.y = .45,
     add = TRUE, ps=1000)
plot(g3, col = 2, lty = 1,
     print.auc=TRUE,
     print.auc.y = .4,
     add = TRUE, ps=1000)
legend(.40,.8, 
       legend=c("Log CV"), 
       col=c("4"), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)
legend(.40,.75, 
       legend=c("SVM CV"), 
       col=c(3), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)
legend(.40,.7, 
       legend=c("CART Bootstrap"), 
       col=c(2), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)
p<-recordPlot()



png(filename="presentations/roc7CART.png",width=760, height=565)
replayPlot(p)
dev.off()

#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))

png(filename="presentations/cm7CART.png",width=760, height=565)
draw_confusion_matrix_boot(cm,"- CART 7 predictors")
dev.off()

draw_confusion_matrix_boot(cm,"- CART 7 predictors")

save(cart7, file = "models/cart7.rda")

# LDA CV 7 Predictors ----------------------------------------------


###' Debug
normalizedDataset <- temp

include <- c("DistanceHomeOffice","isAfterHours",
             "isSingle","avgSatisfaction",
             "TenureWorking","NumCompaniesWorked",
             "isDepartIT","isChurn")



normalizedDataset <- normalizedDataset[ , (names(normalizedDataset) %in% include)]

correlation <- round(cor(normalizedDataset, method = "kendall"), 1)
png(filename="presentations/correlation7LDA.png",width=1000, height=565)
corr_plot(correlation)
dev.off()

corr_plot(correlation)

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

png(filename="presentations/stratification7LDA.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- LDA 7 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- LDA 7 predictors")


#Yes /No target 
trainData$isChurn <- ifelse(trainData$isChurn==1,"Yes","No")
testData$isChurn <- ifelse(testData$isChurn==1,"Yes","No")



#Save test_labels
test_labels <- as.factor(testData$isChurn)


#drop target from testData, not mandatory
drops <- c("isChurn")
testData <- testData[ , !(names(testData) %in% drops)]




#Setting up CV
ControlParamteres <- trainControl(method="cv", 
                                  number=10,
                                  #repeats=10,
                                  #sampling = "up",
                                  #verboseIter = FALSE,
                                  # summaryFunction=mnLogLoss,
                                  summaryFunction=twoClassSummary,
                                  savePredictions = TRUE,
                                  classProbs = TRUE)

set.seed(375)
# tunegrid <-  expand.grid(.NumVars = c(11), 
#                             .lambda = c(100))

#LDA
lda7 <- caret::train(isChurn~.,
                      data=trainData,
                      method = "lda",
                      #preProc=c("center", "scale"),
                      metric="ROC",
                      #tuneGrid = tunegrid,
                      #tuneLength=10,
                      trControl = ControlParamteres)



lda7


testData$prob <- predict(lda7, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels

#Cutoff
reset.par()
plotCut(ROCR_test,"LDA 7")
abline(v = 0.63, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut7LDACV.png",width=760, height=565)
plotCut(ROCR_test,"LDA 7")
abline(v = 0.63, col="seagreen3", lwd=2, lty=8)
dev.off()

testData$pred <- ifelse(testData$prob$Yes > 0.63, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g4 <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 7 Predictors", asp = NA,  
     xlab = "Specificity (%)", 
     ylab = "Sensitivity (%)",
     print.auc=TRUE, type  = 'l', ps=1000,las=1)
plot(g2, col = 3, lty = 1,
     print.auc=TRUE,
     print.auc.y = .45,
     add = TRUE, ps=1000)
plot(g3, col = 2, lty = 1,
     print.auc=TRUE,
     print.auc.y = .4,
     add = TRUE, ps=1000)
plot(g4, col = 4, lty = 3,
     print.auc=TRUE,
     print.auc.y = .35,
     add = TRUE, ps=1000)
legend(.40,.8, 
       legend=c("Log CV"), 
       col=c("4"), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)
legend(.40,.75, 
       legend=c("SVM CV"), 
       col=c(3), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)
legend(.40,.7, 
       legend=c("CART Bootstrap"), 
       col=c(2), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)
legend(.40,.65, 
       legend=c("LDA CV"), 
       col=c(4), 
       lwd=2, ncol = 1,bty = "n", cex=1.3,lty=15)
p<-recordPlot()


png(filename="presentations/roc7LDACV.png",width=760, height=565)
replayPlot(p)
dev.off()

#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))

png(filename="presentations/cm7LDACV.png",width=760, height=565)
draw_confusion_matrix_cv(cm,"- LDA 7 predictors")
dev.off()

draw_confusion_matrix_cv(cm,"- LDA 7 predictors")

save(lda7, file = "models/lda7.rda")

# NN CV 7 Predictors ----------------------------------------------


###' Debug
normalizedDataset <- temp

include <- c("DistanceHomeOffice","isAfterHours",
             "isSingle","avgSatisfaction",
             "TenureWorking","NumCompaniesWorked",
             "isDepartIT","isChurn")


normalizedDataset <- normalizedDataset[ , (names(normalizedDataset) %in% include)]

correlation <- round(cor(normalizedDataset, method = "kendall"), 1)
png(filename="presentations/correlation7NN.png",width=760, height=565)
corr_plot(correlation)
dev.off()

corr_plot(correlation)

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

png(filename="presentations/stratification7NN.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- NN 7 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- NN 7 predictors")



# Yes no target
trainData$isChurn <- ifelse(trainData$isChurn==1,"Yes","No")
testData$isChurn <- ifelse(testData$isChurn==1,"Yes","No")



# #Save test_labels
test_labels <- as.factor(testData$isChurn)

#drop target from testData, not mandatory
drops <- c("isChurn")
testData <- testData[ , !(names(testData) %in% drops)]




#Setting up CV
ControlParamteres <- trainControl(method="cv", 
                                  number=10,
                                  #repeats=10,
                                  #sampling = "up",
                                  #verboseIter = FALSE,
                                  summaryFunction=twoClassSummary,
                                  savePredictions = TRUE,
                                  classProbs = TRUE)


set.seed(375) #weights
nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.1, to = 0.5, by = 0.1))

#NN
bin <- capture.output(nn7 <- caret::train(isChurn~.,
                                           data=trainData,
                                           method = "nnet",
                                           #preProc=c("center", "scale"),
                                           metric="ROC",
                                           #tuneLength = 5,
                                           tuneGrid = nnetGrid,
                                           trControl = ControlParamteres))


nn7

png(filename="presentations/tune7NNCV.png",width=760, height=565)
plot(nn7)
dev.off()

plot(nn7)


testData$prob <- predict(nn7, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels

#Cutoff
reset.par()
plotCut(ROCR_test,"NN 7")
abline(v = 0.681, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut7NNCV.png",width=760, height=565)
plotCut(ROCR_test,"NN 7")
abline(v = 0.681, col="seagreen3", lwd=2, lty=8)
dev.off()

testData$pred <- ifelse(testData$prob$Yes > 0.681, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g5 <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 7 Predictors", asp = NA,  
     xlab = "Specificity (%)", 
     ylab = "Sensitivity (%)",
     print.auc=TRUE, type  = 'l', ps=1000,las=1)
plot(g2, col = 3, lty = 1,
     print.auc=TRUE,
     print.auc.y = .45,
     add = TRUE, ps=1000)
plot(g3, col = 2, lty = 1,
     print.auc=TRUE,
     print.auc.y = .4,
     add = TRUE, ps=1000)
plot(g4, col = 4, lty = 3,
     print.auc=TRUE,
     print.auc.y = .35,
     add = TRUE, ps=1000)
plot(g5, col = 3, lty = 3,
     print.auc=TRUE,
     print.auc.y = .3,
     add = TRUE, ps=1000)
legend(.40,.8, 
       legend=c("Log CV"), 
       col=c("4"), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)
legend(.40,.75, 
       legend=c("SVM CV"), 
       col=c(3), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)
legend(.40,.7, 
       legend=c("CART Bootstrap"), 
       col=c(2), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)
legend(.40,.65, 
       legend=c("LDA CV"), 
       col=c(4), 
       lwd=2, ncol = 1,bty = "n", cex=1.3,lty=15)
legend(.40,.6, 
       legend=c("NN CV"), 
       col=c(3), 
       lwd=2, ncol = 1,bty = "n", cex=1.3,lty=15)
p<-recordPlot()



png(filename="presentations/roc7NN.png",width=760, height=565)
replayPlot(p)
dev.off()


#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))


png(filename="presentations/cm7NN.png",width=760, height=565)
draw_confusion_matrix_cv(cm,"- NN 7 predictors")
dev.off()

draw_confusion_matrix_cv(cm,"- NN 7 predictors")


save(nn7, file = "models/nn7.rda")

# RF Bootstrap 7 Predictors ----------------------------------------------

###' Debug
normalizedDataset <- temp

include <- c("DistanceHomeOffice","isAfterHours",
             "isSingle","avgSatisfaction",
             "TenureWorking","NumCompaniesWorked",
             "isDepartIT","isChurn")


normalizedDataset <- normalizedDataset[ , (names(normalizedDataset) %in% include)]

correlation <- round(cor(normalizedDataset, method = "kendall"), 1)
png(filename="presentations/correlation7RF.png",width=760, height=565)
corr_plot(correlation)
dev.off()

corr_plot(correlation)

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

png(filename="presentations/stratification7RF.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- RF 7 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- RF 7 predictors")



# Yes/No target
trainData$isChurn <- ifelse(trainData$isChurn==1,"Yes","No")
testData$isChurn <- ifelse(testData$isChurn==1,"Yes","No")



#Save test_labels
test_labels <- as.factor(testData$isChurn)


#drop target from testData, not mandatory
drops <- c("isChurn")
testData <- testData[ , !(names(testData) %in% drops)]




#Setting up CV
ControlParamteres <- trainControl(method="boot", 
                                  #number=10,
                                  #repeats=10,
                                  #sampling = "up",
                                  #verboseIter = FALSE,
                                  summaryFunction=twoClassSummary,
                                  savePredictions = TRUE,
                                  classProbs = TRUE)



set.seed(375)
tunegrid <- expand.grid(.mtry=c(1:7))

#Random forest
rf7 <- caret::train(isChurn~.,
                     data=trainData,
                     method = "rf",
                     ntree=500,
                     #preProc=c("center", "scale"),
                     #tuneLength=10,
                     metric="ROC",
                     tuneGrid=tunegrid,
                     trControl = ControlParamteres)


rf7

png(filename="presentations/tune7RF.png",width=760, height=565)
plot(rf7)
dev.off()

plot(rf7)


testData$prob <- predict(rf7, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels

#Cutoff
reset.par()
plotCut(ROCR_test,"RF 7")
abline(v = 0.3115, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut7RF.png",width=760, height=565)
plotCut(ROCR_test,"RF 7")
abline(v = 0.3115, col="seagreen3", lwd=2, lty=8)
dev.off()

testData$pred <- ifelse(testData$prob$Yes > 0.3115, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g6 <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 7 Predictors", asp = NA,  
     xlab = "Specificity (%)", 
     ylab = "Sensitivity (%)",
     print.auc=TRUE, type  = 'l', ps=1000,las=1)
plot(g2, col = 3, lty = 1,
     print.auc=TRUE,
     print.auc.y = .45,
     add = TRUE, ps=1000)
plot(g3, col = 2, lty = 1,
     print.auc=TRUE,
     print.auc.y = .4,
     add = TRUE, ps=1000)
plot(g4, col = 4, lty = 3,
     print.auc=TRUE,
     print.auc.y = .35,
     add = TRUE, ps=1000)
plot(g5, col = 3, lty = 3,
     print.auc=TRUE,
     print.auc.y = .3,
     add = TRUE, ps=1000)
plot(g6, col = 2, lty = 3,
     print.auc=TRUE,
     print.auc.y = .25,
     add = TRUE, ps=1000)
legend(.40,.8, 
       legend=c("Log CV"), 
       col=c("4"), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)
legend(.40,.75, 
       legend=c("SVM CV"), 
       col=c(3), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)
legend(.40,.7, 
       legend=c("CART Bootstrap"), 
       col=c(2), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)
legend(.40,.65, 
       legend=c("LDA CV"), 
       col=c(4), 
       lwd=2, ncol = 1,bty = "n", cex=1.3,lty=15)
legend(.40,.6, 
       legend=c("NN CV"), 
       col=c(3), 
       lwd=2, ncol = 1,bty = "n", cex=1.3,lty=15)
legend(.40,.55, 
       legend=c("RF Bootstrap"), 
       col=c(2), 
       lwd=2, ncol = 1,bty = "n", cex=1.3,lty=15)
p<-recordPlot()


png(filename="presentations/roc7RF.png",width=760, height=565)
replayPlot(p)
dev.off()

#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))

png(filename="presentations/cm7RF.png",width=760, height=565)
draw_confusion_matrix_boot(cm,"- RF 7 predictors")
dev.off()

draw_confusion_matrix_boot(cm,"- RF 7 predictors")


save(rf7, file = "models/rf7.rda")





