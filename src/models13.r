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
                ,"dummies","ROCR","neuralnet","sparseLDA","adabag","RWeka"))


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

correlation <- round(cor(normalizedDataset, method = "kendall"), 1)
png(filename="presentations/correlation13Log.png",width=1000, height=565)
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

png(filename="presentations/stratification13Log.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- Log 13 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- Log 13 predictors")


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
glm13 <- caret::train(isChurn~.,
            data=trainData,
            method = "glm",
            #preProc=c("center", "scale"),
            metric="ROC",
            trControl = ControlParamteres)


glm13
summary(glm13)



testData$prob <- predict(glm13, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels

#Cutoff
reset.par()
plotCut(ROCR_test,"LOG 13")
abline(v = 0.435, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut13LogCV.png",width=760, height=565)
plotCut(ROCR_test,"LOG 13")
abline(v = 0.435, col="seagreen3", lwd=2, lty=8)
dev.off()

testData$pred <- ifelse(testData$prob$Yes > 0.436, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 13 Predictors", asp = NA,  
     xlab = "Specificity (%)", 
     ylab = "Sensitivity (%)",
     print.auc=TRUE, type  = 'l', ps=1000,las=1)
legend(.40,.8, 
       legend=c("Log CV"), 
       col=c("4"), 
       lwd=2, ncol = 1,bty = "n", cex=1.3)
p<-recordPlot()

png(filename="presentations/roc13LogCV.png",width=760, height=565)
replayPlot(p)
dev.off()


#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))

png(filename="presentations/cm13LogCV.png",width=760, height=565)
draw_confusion_matrix_cv(cm,"- Log 13 predictors")
dev.off()

draw_confusion_matrix_cv(cm,"- Log 13 predictors")

save(glm13, file = "models/glm13.rda")

# SVM CV 13 Predictors ----------------------------------------------

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
png(filename="presentations/correlation13SVM.png",width=760, height=565)
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

png(filename="presentations/stratification13SVM.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- SVM 13 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- SVM 13 predictors")


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
svmGrid <- expand.grid(sigma= 2^c(-5), C= 2^c(-1:0))

#SVM
svm13 <- caret::train(isChurn~.,
             data=trainData,
             method = "svmRadial",
             #preProc=c("center", "scale"),
             metric="ROC",
             # tuneLength = 5,
             tuneGrid = svmGrid,
             trControl = ControlParamteres)



svm13

png(filename="presentations/tune13SVMCV.png",width=760, height=565)
plot(svm13)
dev.off()
plot(svm13)


testData$prob <- predict(svm13, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels

#Cutoff
reset.par()
plotCut(ROCR_test,"SVM 13")
abline(v = 0.528, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut13SVMCV.png",width=760, height=565)
plotCut(ROCR_test,"SVM 13")
abline(v = 0.528, col="seagreen3", lwd=2, lty=8)
dev.off()

#Cutoff  
testData$pred <- ifelse(testData$prob$Yes >0.528, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g2 <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 13 Predictors", asp = NA,  
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



png(filename="presentations/roc13SVMCV.png",width=760, height=565)
replayPlot(p)
dev.off()

#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))

png(filename="presentations/cm13SVMCV.png",width=760, height=565)
draw_confusion_matrix_cv(cm,"- SVM 13 predictors")
dev.off()

draw_confusion_matrix_cv(cm,"- SVM 13 predictors")

save(svm13, file = "models/svm13.rda")

# CART Bootstrap 13 Predictors ----------------------------------------------

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
png(filename="presentations/correlation13CART.png",width=760, height=565)
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

png(filename="presentations/stratification13CART.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- CART 13 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- CART 13 predictors")



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
tunegrid <- expand.grid(.maxdepth =c(2:20))
#CART
cart13 <- caret::train(isChurn~.,
                     data=trainData,
                     method = "rpart2",
                     #preProc=c("center", "scale"),
                     metric="ROC",
                     #tuneLength = 10,
                     tuneGrid=tunegrid,
                     trControl = ControlParamteres)


cart13

png(filename="presentations/tune13CART.png",width=760, height=565)
plot(cart13)
dev.off()

plot(cart13)


testData$prob <- predict(cart13, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels

#Cutoff
reset.par()
plotCut(ROCR_test,"CART 13")
abline(v = 0.799, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut13CART.png",width=760, height=565)
plotCut(ROCR_test,"CART 13")
abline(v = 0.799, col="seagreen3", lwd=2, lty=8)
dev.off()

testData$pred <- ifelse(testData$prob$Yes > 0.799, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g3 <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 13 Predictors", asp = NA,  
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





png(filename="presentations/roc13CART.png",width=760, height=565)
replayPlot(p)
dev.off()

#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))

png(filename="presentations/cm13CART.png",width=760, height=565)
draw_confusion_matrix_boot(cm,"- CART 13 predictors")
dev.off()

draw_confusion_matrix_boot(cm,"- CART 13 predictors")


save(cart13, file = "models/cart13.rda")

# LDA CV 13 Predictors ----------------------------------------------


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
png(filename="presentations/correlation13LDA.png",width=1000, height=565)
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

png(filename="presentations/stratification13LDA.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- LDA 13 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- LDA 13 predictors")


#Yes /No target 
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
# tunegrid <-  expand.grid(.NumVars = c(11), 
#                             .lambda = c(100))

#LDA
lda13 <- caret::train(isChurn~.,
                    data=trainData,
                    method = "lda",
                    #preProc=c("center", "scale"),
                    metric="ROC",
                    #tuneGrid = tunegrid,
                    #tuneLength=10,
                    trControl = ControlParamteres)



lda13


testData$prob <- predict(lda13, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels


#Cutoff
reset.par()
plotCut(ROCR_test,"LDA 13")
abline(v = 0.669, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut13LDACV.png",width=760, height=565)
plotCut(ROCR_test,"LDA 13")
abline(v = 0.669, col="seagreen3", lwd=2, lty=8)
dev.off()

testData$pred <- ifelse(testData$prob$Yes > 0.669, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g4 <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 13 Predictors", asp = NA,  
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

png(filename="presentations/roc13LDACV.png",width=760, height=565)
replayPlot(p)
dev.off()

#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))

png(filename="presentations/cm13LDACV.png",width=760, height=565)
draw_confusion_matrix_cv(cm,"- LDA 13 predictors")
dev.off()

draw_confusion_matrix_cv(cm,"- LDA 13 predictors")


save(lda13, file = "models/lda13.rda")

# NN CV 13 Predictors ----------------------------------------------


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
png(filename="presentations/correlation13NN.png",width=760, height=565)
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

png(filename="presentations/stratification13NN.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- NN 13 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- NN 13 predictors")



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
bin <- capture.output(nn13 <- caret::train(isChurn~.,
                           data=trainData,
                           method = "nnet",
                           #preProc=c("center", "scale"),
                           metric="ROC",
                           #tuneLength = 5,
                           tuneGrid = nnetGrid,
                           trControl = ControlParamteres))

nn13

png(filename="presentations/tune13NNCV.png",width=760, height=565)
plot(nn13)
dev.off()

plot(nn13)


testData$prob <- predict(nn13, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels

#Cutoff
reset.par()
plotCut(ROCR_test,"NN 13")
abline(v = 0.542, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut13NNCV.png",width=760, height=565)
plotCut(ROCR_test,"NN 13")
abline(v = 0.542, col="seagreen3", lwd=2, lty=8)
dev.off()

testData$pred <- ifelse(testData$prob$Yes > 0.542, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g5 <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 13 Predictors", asp = NA,  
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
legend(.40,0.8, 
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




png(filename="presentations/roc13NN.png",width=760, height=565)
replayPlot(p)
dev.off()


#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))


png(filename="presentations/cm13NN.png",width=760, height=565)
draw_confusion_matrix_cv(cm,"- NN 13 predictors")
dev.off()

draw_confusion_matrix_cv(cm,"- NN 13 predictors")

save(nn13, file = "models/nn13.rda")


# RF Bootstrap 13 Predictors ----------------------------------------------

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
png(filename="presentations/correlation13RF.png",width=760, height=565)
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

png(filename="presentations/stratification13RF.png",width=300, height=250)
stratification(normalizedDataset,trainData,testData,"- RF 13 predictors")
dev.off()

stratification(normalizedDataset,trainData,testData,"- RF 13 predictors")



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
rf13 <- caret::train(isChurn~.,
                   data=trainData,
                   method = "rf",
                   ntree=50,
                   #preProc=c("center", "scale"),
                   metric="ROC",
                   #tuneLength=10,
                   tuneGrid=tunegrid,
                   trControl = ControlParamteres)


rf13

png(filename="presentations/tune13RF.png",width=760, height=565)
plot(rf13)
dev.off()

plot(rf13)


testData$prob <- predict(rf13, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)
testData$isChurn <- test_labels


#Cutoff
reset.par()
plotCut(ROCR_test,"RF 13")
abline(v = 0.439, col="seagreen3", lwd=2, lty=8)

png(filename="presentations/cut13RF.png",width=760, height=565)
plotCut(ROCR_test,"RF 13")
abline(v = 0.439, col="seagreen3", lwd=2, lty=8)
dev.off()

testData$pred <- ifelse(testData$prob$Yes > 0.439, "Yes", "No")
testData$pred<-as.factor(testData$pred)


g6 <- roc(isChurn ~ prob$Yes, data =testData )
reset.par()

#ROC plot
plot(g, col = 4, lty = 1, 
     main = "ROC 13 Predictors", asp = NA,  
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


png(filename="presentations/roc13RF.png",width=760, height=565)
replayPlot(p)
dev.off()

#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))

png(filename="presentations/cm13RF.png",width=760, height=565)
draw_confusion_matrix_boot(cm,"- RF 13 predictors")
dev.off()

draw_confusion_matrix_boot(cm,"- RF 13 predictors")

save(rf13, file = "models/rf13.rda")




       
