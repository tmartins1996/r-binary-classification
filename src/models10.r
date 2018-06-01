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



# Logistic CV 10 Predictors ----------------------------------------------

###' Debug
normalizedDataset <- temp


include <- c("DistanceHomeOffice","isAfterHours",
             "isSingle","Age","avgSatisfaction",
             "TenureWorking","NumCompaniesWorked",
             "LastPromotion","MonthlyIncome","isDepartIT",
             "isChurn")


normalizedDataset <- normalizedDataset[ , (names(normalizedDataset) %in% include)]

correlation <- round(cor(normalizedDataset, method = "kendall"), 1)
png(filename="presentations/correlation10Log.png",width=1000, height=565)
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

png(filename="presentations/stratification10Log.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- Log 10 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- Log 10 predictors")


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
glm10 <- caret::train(isChurn~.,
                    data=trainData,
                    method = "glm",
                    #preProc=c("center", "scale"),
                    metric="ROC",
                    trControl = ControlParamteres)


glm10
summary(glm10)



testData$prob <- predict(glm10, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels

#Cutoff plot
reset.par()
plotCut(ROCR_test,"LOG 10")
abline(v = 0.429, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut10LogCV.png",width=760, height=565)
plotCut(ROCR_test,"LOG 10")
abline(v = 0.429, col="seagreen3", lwd=2, lty=8)
dev.off()

testData$pred <- ifelse(testData$prob$Yes > 0.429, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 10 Predictors", asp = NA,  
     xlab = "Specificity (%)", 
     ylab = "Sensitivity (%)",
     print.auc=TRUE, type  = 'l', ps=1000,las=1)
legend(.40,.8, 
       legend=c("Log CV"), 
       col=c("4"), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)
p<-recordPlot()

png(filename="presentations/roc10LogCV.png",width=760, height=565)
replayPlot(p)
dev.off()


#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))

png(filename="presentations/cm10LogCV.png",width=760, height=565)
draw_confusion_matrix_cv(cm,"- Log 10 predictors")
dev.off()
source("src/wrangling.r")
draw_confusion_matrix_cv(cm,"- Log 10 predictors")

save(glm10, file = "models/glm10.rda")


# SVM CV 10 Predictors ----------------------------------------------

###' Debug
normalizedDataset <- temp

include <- c("DistanceHomeOffice","isAfterHours",
             "isSingle","Age","avgSatisfaction",
             "TenureWorking","NumCompaniesWorked",
             "LastPromotion","MonthlyIncome","isDepartIT",
             "isChurn")


normalizedDataset <- normalizedDataset[ , (names(normalizedDataset) %in% include)]

correlation <- round(cor(normalizedDataset, method = "kendall"), 1)
png(filename="presentations/correlation10SVM.png",width=760, height=565)
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

png(filename="presentations/stratification10SVM.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- SVM 10 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- SVM 10 predictors")


# target to Yes/no
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
set.seed(375)
svmGrid <- expand.grid(sigma= 2^c(-7:-2), C= 2^c(0:2))

#SVM
svm10 <- caret::train(isChurn~.,
                    data=trainData,
                    method = "svmRadial",
                    #preProc=c("center", "scale"),
                    metric="ROC",
                    #tuneLength = 5,
                    tuneGrid = svmGrid,
                    trControl = ControlParamteres)



svm10

png(filename="presentations/tune10SVMCV.png",width=760, height=565)
plot(svm10)
dev.off()

plot(svm10)


testData$prob <- predict(svm10, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels

#Cutoff plot
reset.par()
plotCut(ROCR_test,"SVM 10")
abline(v = 0.574, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut10SVMCV.png",width=760, height=565)
plotCut(ROCR_test,"SVM 10")
abline(v = 0.574, col="seagreen3", lwd=2, lty=8)
dev.off()

#Cutoff  
testData$pred <- ifelse(testData$prob$Yes >0.574, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g2 <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 10 Predictors", asp = NA,  
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


png(filename="presentations/roc10SVMCV.png",width=760, height=565)
replayPlot(p)
dev.off()

#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))

png(filename="presentations/cm10SVMCV.png",width=760, height=565)
draw_confusion_matrix_cv(cm,"- SVM 10 predictors")
dev.off()

draw_confusion_matrix_cv(cm,"- SVM 10 predictors")


save(svm10, file = "models/svm10.rda")

# CART Bootstrap 10 Predictors ----------------------------------------------

###' Debug
normalizedDataset <- temp

include <- c("DistanceHomeOffice","isAfterHours",
             "isSingle","Age","avgSatisfaction",
             "TenureWorking","NumCompaniesWorked",
             "LastPromotion","MonthlyIncome","isDepartIT",
             "isChurn")


normalizedDataset <- normalizedDataset[ , (names(normalizedDataset) %in% include)]

correlation <- round(cor(normalizedDataset, method = "kendall"), 1)
png(filename="presentations/correlation10CART.png",width=760, height=565)
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

png(filename="presentations/stratification10CART.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- CART 10 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- CART 10 predictors")



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
cart10 <- caret::train(isChurn~.,
                     data=trainData,
                     method = "rpart2",
                     #preProc=c("center", "scale"),
                     metric="ROC",
                     #tuneLength = 10,
                     tuneGrid=tunegrid,
                     trControl = ControlParamteres)


cart10

png(filename="presentations/tune10CART.png",width=760, height=565)
plot(cart10)
dev.off()

plot(cart10)


testData$prob <- predict(cart10, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels

#Cutoff
reset.par()
plotCut(ROCR_test,"CART 10")
abline(v = 0.8, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut10CART.png",width=760, height=565)
plotCut(ROCR_test,"CART 10")
abline(v = 0.8, col="seagreen3", lwd=2, lty=8)
dev.off()

testData$pred <- ifelse(testData$prob$Yes > 0.8, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g3 <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 10 Predictors", asp = NA,  
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



png(filename="presentations/roc10CART.png",width=760, height=565)
replayPlot(p)
dev.off()

#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))

png(filename="presentations/cm10CART.png",width=760, height=565)
draw_confusion_matrix_boot(cm,"- CART 10 predictors")
dev.off()

draw_confusion_matrix_boot(cm,"- CART 10 predictors")

save(cart10, file = "models/cart10.rda")

# LDA CV 10 Predictors ----------------------------------------------


###' Debug
normalizedDataset <- temp

include <- c("DistanceHomeOffice","isAfterHours",
             "isSingle","Age","avgSatisfaction",
             "TenureWorking","NumCompaniesWorked",
             "LastPromotion","MonthlyIncome","isDepartIT",
             "isChurn")



normalizedDataset <- normalizedDataset[ , (names(normalizedDataset) %in% include)]

correlation <- round(cor(normalizedDataset, method = "kendall"), 1)
png(filename="presentations/correlation10LDA.png",width=1000, height=565)
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

png(filename="presentations/stratification10LDA.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- LDA 10 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- LDA 10 predictors")


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
lda10 <- caret::train(isChurn~.,
                    data=trainData,
                    method = "lda",
                    #preProc=c("center", "scale"),
                    metric="ROC",
                    #tuneGrid = tunegrid,
                    #tuneLength=10,
                    trControl = ControlParamteres)



lda10


testData$prob <- predict(lda10, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels

#Cutoff
reset.par()
plotCut(ROCR_test,"LDA 10")
abline(v = 0.613, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut10LDACV.png",width=760, height=565)
plotCut(ROCR_test,"LDA 10")
abline(v = 0.613, col="seagreen3", lwd=2, lty=8)
dev.off()

testData$pred <- ifelse(testData$prob$Yes > 0.613, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g4 <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 10 Predictors", asp = NA,  
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



png(filename="presentations/roc10LDACV.png",width=760, height=565)
replayPlot(p)
dev.off()

#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))

png(filename="presentations/cm10LDACV.png",width=760, height=565)
draw_confusion_matrix_cv(cm,"- LDA 10 predictors")
dev.off()

draw_confusion_matrix_cv(cm,"- LDA 10 predictors")

save(lda10, file = "models/lda10.rda")

# NN CV 10 Predictors ----------------------------------------------


###' Debug
normalizedDataset <- temp

include <- c("DistanceHomeOffice","isAfterHours",
             "isSingle","Age","avgSatisfaction",
             "TenureWorking","NumCompaniesWorked",
             "LastPromotion","MonthlyIncome","isDepartIT",
             "isChurn")


normalizedDataset <- normalizedDataset[ , (names(normalizedDataset) %in% include)]

correlation <- round(cor(normalizedDataset, method = "kendall"), 1)
png(filename="presentations/correlation10NN.png",width=760, height=565)
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

png(filename="presentations/stratification10NN.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- NN 10 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- NN 10 predictors")



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
bin <- capture.output(nn10 <- caret::train(isChurn~.,
                                         data=trainData,
                                         method = "nnet",
                                         #preProc=c("center", "scale"),
                                         metric="ROC",
                                         #tuneLength = 5,
                                         tuneGrid = nnetGrid,
                                         trControl = ControlParamteres))

nn10

png(filename="presentations/tune10NNCV.png",width=760, height=565)
plot(nn10)
dev.off()

plot(nn10)


testData$prob <- predict(nn10, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels

#Cutoff
reset.par()
plotCut(ROCR_test,"NN 10")
abline(v = 0.53, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut10NNCV.png",width=760, height=565)
plotCut(ROCR_test,"NN 10")
abline(v = 0.53, col="seagreen3", lwd=2, lty=8)
dev.off()

testData$pred <- ifelse(testData$prob$Yes > 0.53, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g5 <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 10 Predictors", asp = NA,  
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




png(filename="presentations/roc10NN.png",width=760, height=565)
replayPlot(p)
dev.off()


#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))


png(filename="presentations/cm10NN.png",width=760, height=565)
draw_confusion_matrix_cv(cm,"- NN 10 predictors")
dev.off()

draw_confusion_matrix_cv(cm,"- NN 10 predictors")


save(nn10, file = "models/nn10.rda")

# RF Bootstrap 10 Predictors ----------------------------------------------

###' Debug
normalizedDataset <- temp

include <- c("DistanceHomeOffice","isAfterHours",
             "isSingle","Age","avgSatisfaction",
             "TenureWorking","NumCompaniesWorked",
             "LastPromotion","MonthlyIncome","isDepartIT",
             "isChurn")


normalizedDataset <- normalizedDataset[ , (names(normalizedDataset) %in% include)]

correlation <- round(cor(normalizedDataset, method = "kendall"), 1)
png(filename="presentations/correlation10RF.png",width=760, height=565)
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

png(filename="presentations/stratification10RF.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- RF 10 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- RF 10 predictors")



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
tunegrid <- expand.grid(.mtry=c(1:10))

#Random forest
rf10 <- caret::train(isChurn~.,
                   data=trainData,
                   method = "rf",
                   ntree=50,
                   #preProc=c("center", "scale"),
                   metric="ROC",
                   #tuneLength=10,
                   tuneGrid=tunegrid,
                   trControl = ControlParamteres)


rf10

png(filename="presentations/tune10RF.png",width=760, height=565)
plot(rf10)
dev.off()

plot(rf10)


testData$prob <- predict(rf10, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels

#Cutoff
reset.par()
plotCut(ROCR_test,"RF 10")
abline(v = 0.399, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut10RF.png",width=760, height=565)
plotCut(ROCR_test,"RF 10")
abline(v = 0.399, col="seagreen3", lwd=2, lty=8)
dev.off()

testData$pred <- ifelse(testData$prob$Yes > 0.399, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g6 <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 10 Predictors", asp = NA,  
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



png(filename="presentations/roc10RF.png",width=760, height=565)
replayPlot(p)
dev.off()

#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))

png(filename="presentations/cm10RF.png",width=760, height=565)
draw_confusion_matrix_boot(cm,"- RF 10 predictors")
dev.off()

draw_confusion_matrix_boot(cm,"- RF 10 predictors")


save(rf10, file = "models/rf10.rda")



