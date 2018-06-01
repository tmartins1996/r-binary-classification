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
                "MASS","autoimage","randomForest","settings","factoextra",
                "dummies","corrplot"))



# Load data ---------------------------------------------------------------
#Load normalized data xlsx
source("src/wrangling.r")
normalizedDataset <- xlsx::read.xlsx('datasets/normalizedDataset.xlsx',1, header= TRUE)

#backup set
temp<-normalizedDataset


#Checking correlation, dropping variables not correlated
correlation <- round(cor(normalizedDataset, method = "kendall"), 1)
write_excel_report(correlation)


drops <- c("Dependents","JobPerformance",
           "DistanceHomeOffice","JobDedication","isTogether","JobTypeOffice"
           ,"isEducAssociate","isEducAreaOther","isEducAreaHR"
           ,"isRoleHR","isRoleMarketing","isMale","isEducMasters","isMarried","isSingle"
           ,"isEducCollege","isEducAreaIT","isRoleDev","isRoleIT"
           ,"SalaryRise","isEducAreaEng","NumberProjectsLastYear","RoleSatisfaction" 
           ,"HierarchySatisfaction","JobTypeRemote","isAfterHours","isRoleComercialRepr",
           "isEducBachelor","avgSatisfaction","FacilitiesSatisfaction",
           "isRoleDataSci","isEducAreaMarketing","isRoleManager","LastPromotion",
           "TcompanyperWorking" ,"WorkLifeLevels","NumcompaniesperWorking","isChurn")
normalizedDataset <- normalizedDataset[ , !(names(normalizedDataset) %in% drops)]

correlation <- round(cor(normalizedDataset, method = "kendall"), 1)


png(filename="presentations/correlation.png",width=1000, height=565)
p<-corr_plot(correlation)
p
dev.off()
p

#Corr w/ target
normalizedDataset<-temp

png(filename="presentations/correlationTarget.png",width=1000, height=565)
p<-corr_target(normalizedDataset)
p
dev.off()
p



# Feature selection -------------------------------------------------------
# Logistic Regr CV - 50 independents vars ------------------------------------

###' Debug
normalizedDataset <- temp

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


png(filename="presentations/stratification.png",width=300, height=250)
p<-stratification(normalizedDataset,trainData,testData,"without SMOTE")
p
dev.off()
p

# 
trainData$isChurn <- ifelse(trainData$isChurn==1,"Yes","No")
testData$isChurn <- ifelse(testData$isChurn==1,"Yes","No")


# 
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
                                  savePredictions = TRUE,
                                  classProbs = TRUE)

#regLogistic
log_CV <- train(isChurn~.,
                data=trainData,
                method = "glm",
                #preProc=c("center", "scale"),
                #metric="Accuracy",
                trControl = ControlParamteres)



summary(log_CV)

#The same propotion of target variable is maintained 
summary(log_CV$finalModel$data$.outcome)


#### Base model w/ all predictors just to have in mind what to expect 

# testData$pred <- predict(log_CV,testData,type = "raw")
# testData$prob <-predict(log_CV, testData,type = "prob")[2]
# testData$isChurn <- test_labels
# 
# g <- roc(isChurn ~ prob$Yes, data =testData )
# reset.par()
# 
# p<-recordPlot()
# plot(g, col = 4, lty = 1, 
#      main = "ROC", asp = NA,  
#      xlab = "Specificity (%)", 
#      ylab = "Sensitivity (%)",
#      print.auc=TRUE, type  = 'l', ps=1000)
# legend(.40,.75, 
#        legend=c("50 Predictors CV"), 
#        col=c("4"), 
#        lwd=2, ncol = 1,bty = "n", cex=1.3)
# 
# png(filename="presentations/roc50PredictorsCV.png",width=760, height=565)
# p
# dev.off()
# 
# #confusionMatrix(pred, obs, positive = NULL,...), not the inverse
# cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
#                              dnn=c("Pred","Actual"))
# 
# 
# png(filename="presentations/cm50PredictorsCV.png",width=760, height=565)
# draw_confusion_matrix_cv(cm,"- Log 50 predictors")
# dev.off()
# draw_confusion_matrix_cv(cm,"- Log 50 predictors")



# Logistic Regr SMOTECV (Oversampling) - 50 independents vars ----------------------------------

normalizedDataset<-temp


# Data Partition 
set.seed(375); trainingRowIndex <- sample(1:nrow(normalizedDataset),
                                          0.69*nrow(normalizedDataset))
#1000 records after smote
trainData <- normalizedDataset[trainingRowIndex, ]
#remaining 450 records
testData <- normalizedDataset[-trainingRowIndex, ]


# # ## now using SMOTE to create a more "balanced problem"
trainData$isChurn <- as.factor(trainData$isChurn)
minorSet <- DMwR::SMOTE(isChurn ~ ., trainData, 
                        k = 5, 
                        perc.over = 300,
                        perc.under=0)
#bind new data
trainData<-rbind(trainData,minorSet)


#checking propotion of target var
normalizedDataset$isChurn %>%
  table() %>% prop.table() %>% {. * 100} %>% round(0)
trainData$isChurn %>%
  table() %>% prop.table() %>% {. * 100} %>% round(0)
testData$isChurn %>% 
  table() %>% prop.table() %>% {. * 100} %>% round(0)

png(filename="presentations/stratificationSMOTE.png",width=300, height=250)
p<-stratification(normalizedDataset,trainData,testData,"with SMOTE")
p
dev.off()
p

# 
trainData$isChurn <- ifelse(trainData$isChurn==1,"Yes","No")
testData$isChurn <- ifelse(testData$isChurn==1,"Yes","No")


# 
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
                                  savePredictions = TRUE,
                                  classProbs = TRUE)

#regLogistic
log_CV <- train(isChurn~.,
                data=trainData,
                method = "glm",
                #preProc=c("center", "scale"),
                #metric="Accuracy",
                trControl = ControlParamteres)


log_CV
summary(log_CV)


#The same propotion of target variable is maintained 
summary(log_CV$finalModel$data$.outcome)



#### Base model w/ all predictors just to have in mind what to expect 

# 
# testData$pred <- predict(log_CV,testData,type = "raw")
# testData$prob <-predict(log_CV, testData,type = "prob")[2]
# testData$isChurn <- test_labels
# 
# 
# g2 <- roc(isChurn ~ prob$Yes, data =testData )
# reset.par()
# 
# 
# p<-recordPlot()
# plot(g, col = 4, lty = 1, 
#      main = "ROC", asp = NA,  
#      xlab = "Specificity (%)", 
#      ylab = "Sensitivity (%)",
#      print.auc=TRUE, type  = 'l', ps=1000)
# plot(g2, col = 3, lty = 2,
#      print.auc=TRUE,
#      print.auc.y = .45,
#      add = TRUE, ps=1000)
# legend(.40,.75, 
#        legend=c("50 Predictors CV"), 
#        col=c("4"), 
#        lwd=2, ncol = 1,bty = "n", cex=1.3)
# legend(.40,.7, 
#        legend=c("50 Predictors SMOTECV"), 
#        col=c("3"), 
#        lwd=2, ncol = 1,bty = "n", cex=1.3,lty=5)
# 
# png(filename="presentations/roc50PredictorsSMOTECV.png",width=760, height=565)
# p
# dev.off()
# 
# 
# 
# #confusionMatrix(pred, obs, positive = NULL,...), not the inverse
# cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
#                              dnn=c("Pred","Actual"))
# 
# png(filename="presentations/cm50PredictorsSMOTECV.png",width=760, height=565)
# draw_confusion_matrix_smote(cm,"- Log 50 predictors")
# dev.off()
# draw_confusion_matrix_smote(cm,"- Log 50 predictors")




# Logistic Regr PCA SMOTECV - 3 components  ---------------------------
numericDataset <- xlsx::read.xlsx('datasets/numericDataset.xlsx',1, header= TRUE)

# Data Partition 
set.seed(375); trainingRowIndex <- sample(1:nrow(numericDataset),
                                          0.69*nrow(numericDataset))
#1000 records
trainData <- numericDataset[trainingRowIndex, ]
#remaining 450 records
testData <- numericDataset[-trainingRowIndex, ]


# # ## now using SMOTE to create a more "balanced problem"
trainData$isChurn <- as.factor(trainData$isChurn)
minorSet <- DMwR::SMOTE(isChurn ~ ., trainData, 
                        k = 5, 
                        perc.over = 300,
                        perc.under=0)
#bind new data

trainData<-rbind(trainData,minorSet)
Churn <- trainData$isChurn

drops <- c("isChurn")
trainData <- trainData[ , !(names(trainData) %in% drops)]

trainData <- scale(trainData, center = T, scale = T)
trainData <- as.data.frame(trainData)

#PCA doesn't help the company to understand what variables are more important
pca<- prcomp( trainData, 
              center = F, 
              scale. = F)

# Plot of the variances (y-axis)
# associated with the PCs (x-axis).
png(filename="presentations/PCA.png",width=450, height=250)
fviz_eig(pca, main = "PCA", addlabels=TRUE, hjust = 0,
         barfill="#C51B7D", barcolor ="darkblue",
         linecolor =9) + ylim(0, 15) + 
  theme_minimal()+ labs(title = "PCA - Variance explained",
                        x = "Principal Components", y = "% of variance")
dev.off()

fviz_eig(pca, main = "PCA", addlabels=TRUE, hjust = 0,
         barfill="#C51B7D", barcolor ="darkblue",
         linecolor =9) + ylim(0, 15) + 
  theme_minimal()+ labs(title = "PCA - Variance explained",
                        x = "Principal Components", y = "% of variance")


png(filename="presentations/PCA1.png",width=300, height=300)
fviz_contrib(pca, choice = "var", axes = 1, top = 11, fill="#C51B7D")+
  labs(title = "Contribution to PCA 1")
dev.off()

fviz_contrib(pca, choice = "var", axes = 1, top = 11, fill="#C51B7D")+
    labs(title = "Contribution to PCA 1")
    

png(filename="presentations/PCA2.png",width=300, height=300)
fviz_contrib(pca, choice = "var", axes = 2, top = 8, fill="#C51B7D")+
  labs(title = "Contribution to PCA 2")
dev.off()

fviz_contrib(pca, choice = "var", axes = 2, top = 8, fill="#C51B7D")+
  labs(title = "Contribution to PCA 2")

png(filename="presentations/PCA3.png",width=300, height=300)
fviz_contrib(pca, choice = "var", axes = 3, top = 7, fill="#C51B7D")+
  labs(title = "Contribution to PCA 3")
dev.off()

fviz_contrib(pca, choice = "var", axes = 3, top = 7, fill="#C51B7D")+
  labs(title = "Contribution to PCA 3")


comp <- pca$x[,1:3]
comp <- as.data.frame(comp)

get_eigenvalue(pca)

pca$rotation[1:50,1:3]
pca_value<-pca$rotation[1:50,1:3]
pca_value<-as.data.frame(pca_value)
write_excel_report(pca_value)

trainData<-comp
trainData$isChurn<-Churn

 #checking propotion of target var
numericDataset$isChurn %>%
   table() %>% prop.table() %>% {. * 100} %>% round(0)
comp$isChurn %>%
   table() %>% prop.table() %>% {. * 100} %>% round(0)
testData$isChurn %>%
   table() %>% prop.table() %>% {. * 100} %>% round(0)

png(filename="presentations/stratificationPCA.png",width=300, height=250)
p<-stratification(numericDataset,trainData,testData,"with SMOTE")
p
dev.off()
p
 

trainData$isChurn <- ifelse(trainData$isChurn==1,"Yes","No")
testData$isChurn <- ifelse(testData$isChurn==1,"Yes","No")

# #Save test_labels
test_labels <- as.factor(testData$isChurn)


#drop target from testData, not mandatory 
drops <- c("isChurn")
testData <- testData[ , !(names(testData) %in% drops)]
 
 
 
# #Setting up CV
ControlParamteres <- trainControl(method="cv", 
                                   number=10,
                                   #repeats=10,
                                   #sampling = "up",
                                   #verboseIter = FALSE,
                                   savePredictions = TRUE,
                                   classProbs = TRUE)

# #regLogistic
log_CV <- train(isChurn~.,
                 data=trainData,
                 method = "glm",
                 #preProcess=c("pca"),
                 #metric="Accuracy",
                 trControl = ControlParamteres)
 
log_CV
summary(log_CV)


### PCA Loadings are required for predict on testset

# testData$pred <- predict(log_CV,testData,type = "raw")
# testData$prob <-predict(log_CV, testData,type = "prob")[2]
# testData$isChurn <- test_labels
#  
# g3 <- roc(isChurn ~ prob$Yes, data =testData )
# reset.par()
# 
# p<-recordPlot()
# plot(g, col = 4, lty = 1, 
#       main = "ROC", asp = NA,  
#       xlab = "Specificity (%)", 
#       ylab = "Sensitivity (%)",
#       print.auc=TRUE, type  = 'l', ps=1000)
# plot(g2, col = 3, lty = 2,
#       print.auc=TRUE,
#       print.auc.y = .45,
#       add = TRUE, ps=1000)
# plot(g3, col = "salmon", lty = 3,
#       print.auc=TRUE,
#       print.auc.y = .4,
#       add = TRUE, ps=1000)
# legend(.40,.75, 
#         legend=c("50 Predictors CV"), 
#         col=c("4"), 
#         lwd=2, ncol = 1,bty = "n", cex=1.3)
# legend(.40,.7, 
#         legend=c("50 Predictors SMOTECV"), 
#         col=c("3"), 
#         lwd=2, ncol = 1,bty = "n", cex=1.3,lty=5)
# legend(.40,.65, 
#           legend=c("3 Components SMOTECV"), 
#           col=c("salmon"), 
#           lwd=2, ncol = 1,bty = "n", cex=1.3,lty=15)
#  
# png(filename="presentations/roc3ComponentsSMOTECV.png",width=760, height=565)
# p
# dev.off()
# 
# # #confusionMatrix(pred, obs, positive = NULL,...), not the inverse
# cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
#                               dnn=c("Pred","Actual"))
#  
# png(filename="presentations/cm3ComponentsSMOTECV.png",width=760, height=565)
# draw_confusion_matrix_smote(cm,"- PCA 3 comp")
# dev.off()
# draw_confusion_matrix_smote(cm,"- PCA 3 comp")


# Rank randomForest  w/ SMOTE --------------------------------------------
normalizedDataset<-temp

# Data Partition 
set.seed(375); trainingRowIndex <- sample(1:nrow(normalizedDataset),
                                          0.69*nrow(normalizedDataset))
#1000 records
trainData <- normalizedDataset[trainingRowIndex, ]
#remaining 450 records
testData <- normalizedDataset[-trainingRowIndex, ]


# # ## now using SMOTE to create a more "balanced problem"
trainData$isChurn <- as.factor(trainData$isChurn)
minorSet <- DMwR::SMOTE(isChurn ~ ., trainData, 
                        k = 5, 
                        perc.over = 300,
                        perc.under=0)
#bind new data

trainData<-rbind(trainData,minorSet)



#Setting up CV
ControlParamteres <- trainControl(method="boot", 
                                  #number=10,
                                  #repeats=10,
                                  #sampling = "up",
                                  #verboseIter = FALSE,
                                  savePredictions = TRUE,
                                  classProbs = TRUE)

#gini index
randomForest <- randomForest(isChurn ~ .,
                        data=trainData, 
                        ntree=50,
                        keep.forest=FALSE, 
                        scale=FALSE,
                        #class="ROC",
                        importance=TRUE,
                        trControl = ControlParamteres)
randomForest

#The second measure is the total decrease in node impurities 
#from splitting on the variable, averaged over all trees
rfImportance<-importance(randomForest)
rfImportance
write_excel_report(rfImportance)


png(filename="presentations/rfAcc.png",width=700, height=900)
varImpPlot(randomForest,type=1)
dev.off()
varImpPlot(randomForest,type=1)

png(filename="presentations/rfGini.png",width=700, height=900)
varImpPlot(randomForest,type=2)
dev.off()
varImpPlot(randomForest,type=2)



# RFECV Knn w/ SMOTE ------------------------------------------------------
normalizedDataset<-temp

# Data Partition 
set.seed(375); trainingRowIndex <- sample(1:nrow(normalizedDataset),
                                          0.69*nrow(normalizedDataset))
#1000 records
trainData <- normalizedDataset[trainingRowIndex, ]
#remaining 450 records
testData <- normalizedDataset[-trainingRowIndex, ]


# # ## now using SMOTE to create a more "balanced problem"
trainData$isChurn <- as.factor(trainData$isChurn)
minorSet <- DMwR::SMOTE(isChurn ~ ., trainData, 
                        k = 5, 
                        perc.over = 300,
                        perc.under=0)
#bind new data

trainData<-rbind(trainData,minorSet)
trainData$isChurn <- ifelse(trainData$isChurn==1,"Yes","No")
trainData$isChurn <- as.factor(trainData$isChurn)


rfe_controller <- rfeControl(functions=caretFuncs, 
                             method="cv",
                             rerank = FALSE,
                             number=10, 
                             verbose = FALSE)


ControlParamteres <- trainControl(#method="cv"
                                  #number=10,
                                  #repeats=10,
                                  #sampling = "up",
                                  #verboseIter = FALSE,
                                  #savePredictions = TRUE,
                                  classProbs = TRUE)
knn <- rfe(isChurn~.,
           data=trainData,
           method = "knn",
           sizes = c(5:50),
           metric="Kappa",
           rfeControl = rfe_controller,
           trControl = ControlParamteres)

knn

rankknn<-knn$variables
rankknn
write_excel_report(rankknn)

png(filename="presentations/rfeKnn.png",width=760, height=565)
p <- ggplot(knn) +
  labs(title="RFE Knn SMOTE CV",
       x ="Number of Predictors", y = "Kappa")+
  geom_line(color="black")+
  geom_point(color="blue")+ 
  xlim(2.3, 50)
p
dev.off()
p


# RFECV NB w/ SMOTE ------------------------------------------------------
normalizedDataset<-temp

# Data Partition 
set.seed(375); trainingRowIndex <- sample(1:nrow(normalizedDataset),
                                          0.69*nrow(normalizedDataset))
#1000 records
trainData <- normalizedDataset[trainingRowIndex, ]
#remaining 450 records
testData <- normalizedDataset[-trainingRowIndex, ]


# # ## now using SMOTE to create a more "balanced problem"
trainData$isChurn <- as.factor(trainData$isChurn)
minorSet <- DMwR::SMOTE(isChurn ~ ., trainData, 
                        k = 5, 
                        perc.over = 300,
                        perc.under=0)
#bind new data

trainData<-rbind(trainData,minorSet)
trainData$isChurn <- ifelse(trainData$isChurn==1,"Yes","No")
trainData$isChurn <- as.factor(trainData$isChurn)


control <- rfeControl(functions=nbFuncs,
                      method="cv", 
                      number=10)


rfe_predictors<-trainData[,1:(ncol(trainData)-1)]
rfe_target<- trainData[,(ncol(trainData))]

# run the RFE algorithm
results <- rfe(isChurn~.,
               data=trainData,
               sizes=c(5:50), 
               metric="Accuracy", 
               rfeControl=control)

results

rankNB<-results$variables
rankNB
write_excel_report(rankNB)

png(filename="presentations/rfeNB.png",width=760, height=565)
p <- ggplot(results) +
  labs(title="RFE NB SMOTE",
       x ="Number of Predictors", y = "Kappa")+
  geom_line(color="black")+
  geom_point(color="blue")+ 
  xlim(2.3, 50)
p
dev.off()
p




##### Plot variables selected 
normalizedDataset <- temp

include <- c("DistanceHomeOffice","isAfterHours",
             "RoleSatisfaction","isSingle",
             "isMarried","Age",
             "FacilitiesSatisfaction","avgSatisfaction",
             "TenureWorking","NumCompaniesWorked",
             "LastPromotion","MonthlyIncome","isDepartIT",
             "isChurn")


normalizedDataset <- normalizedDataset[ , (names(normalizedDataset) %in% include)]

# churn~. dont work
# attach(normalizedDataset)

pairs(normalizedDataset$isChurn~normalizedDataset$DistanceHomeOffice+normalizedDataset$isAfterHours+
        normalizedDataset$RoleSatisfaction+normalizedDataset$isSingle+
        normalizedDataset$isMarried+normalizedDataset$Age+
        normalizedDataset$FacilitiesSatisfaction+normalizedDataset$avgSatisfaction,
      panel = panel.smooth,  # Optional smoother
      main = "Variables Selected",
      diag.panel = panel.hist,
      pch = 16,
      col = brewer.pal(2, "Pastel2")[unclass(normalizedDataset$Churn)])



