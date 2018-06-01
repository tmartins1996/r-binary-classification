
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




# Data Partition 
set.seed(375); trainingRowIndex <- sample(1:nrow(normalizedDataset),
                                          0.69*nrow(normalizedDataset))
#1000 records
trainData <- normalizedDataset[trainingRowIndex, ]
#remaining 450 records
testData <- normalizedDataset[-trainingRowIndex, ]


# Yes no target
trainData$isChurn <- ifelse(trainData$isChurn==1,"Yes","No")
testData$isChurn <- ifelse(testData$isChurn==1,"Yes","No")



# #Save test_labels
test_labels <- as.factor(testData$isChurn)

#drop target from testData
drops <- c("isChurn")
testData <- testData[ , !(names(testData) %in% drops)]



yes<-trainData[trainData[,51]=="Yes",]
no<-trainData[trainData[,51]=="No",]
trainData<-rbind(yes,no[1:157,])

#Setting up CV
ControlParamteres <- trainControl(method="loocv", 
                                  # number=10,
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
                                           tuneGrid = nnetGrid,
                                           trControl = ControlParamteres))



nn10

plot(nn10)

testData$isChurn <- test_labels
testData$prob <- predict(nn10, testData ,type = "prob")[2]
ROCR_test <- ROCR::prediction(testData$prob, test_labels)


#Cutoff
reset.par()
plotCut(ROCR_test,"NN 10")
abline(v = 0.53, col="red", lwd=2, lty=3)



testData$pred <- ifelse(testData$prob$Yes > 0.53, "Yes", "No")
testData$pred<-as.factor(testData$pred)







#confusionMatrix(pred, obs, positive = NULL,...), not the inverse
cm <- caret::confusionMatrix(testData$pred, testData$isChurn, positive = 'Yes',
                             dnn=c("Pred","Actual"))




draw_confusion_matrix_cv(cm)





