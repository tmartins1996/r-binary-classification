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
                "Metrics","lattice"))



# Performance on ROC Comparison --------------------------------------------------------------

#Linear Models Comparison
files <- paste("models/",list.files(path="models/", pattern ="glm|lda"),sep="")
for (i in 1:length(files)){load(files[i])};rm(i)
#Log Models Comparison -------------------------------------------------------


Lresults <- resamples(list("Log 13"=glm13, 
                           "Log 10"=glm10, 
                           "Log 7"=glm7))
                           # , 
                           # "Lda 13"=lda13, 
                           # "Lda 10"=lda10, 
                           # "Lda 7"=lda7))

# Boxplot 
png(filename="presentations/LOGmodels.png",width=1000, height=750)
bwplot(Lresults, scales=list(cex=1.5,x="free",tck=c(0,0), y=list(cex=3)), main="LOG Models Comparison")
dev.off()

bwplot(Lresults , scales=list(cex=1.5,x="free",tck=c(0,0), y=list(cex=3)),
       main="Log Models Comparison")

#Lda Models Comparison -------------------------------------------------------


Lresults <- resamples(list(
                          "Lda 13"=lda13,
                          "Lda 10"=lda10,
                          "Lda 7"=lda7))

# Boxplot 
png(filename="presentations/LDAmodels.png",width=1000, height=750)
bwplot(Lresults, scales=list(cex=1.5,x="free",tck=c(0,0), y=list(cex=3)), main="LDA Models Comparison")
dev.off()

bwplot(Lresults , scales=list(cex=1.5,x="free",tck=c(0,0), y=list(cex=3)),
       main="LDA Models Comparison")



#Non-Linear Models Comparison
files <- paste("models/",list.files(path="models/", pattern ="svm|nn"),sep="")
for (i in 1:length(files)){load(files[i])};rm(i)
#SVM Models Comparison -------------------------------------------------------


NLresults <- resamples(list("Svm 13"=svm13, 
                           "Svm 10"=svm10, 
                           "Svm 7"=svm7))
                           # , 
                           # "NN 13"=nn13, 
                           # "NN 10"=nn10, 
                           # "NN 7"=nn7))
# Boxplot 
png(filename="presentations/SVMmodels.png",width=1000, height=750)
bwplot(NLresults,  main="SVM Models Comparison",
       scales=list(cex=1.5,x="free",tck=c(0,0), y=list(cex=3)))
dev.off()

bwplot(NLresults,  main="SVM Models Comparison",
       scales=list(cex=1.5,x="free",tck=c(0,0), y=list(cex=3)))

#NN Models Comparison -------------------------------------------------------


NLresults <- resamples(list(
                            "NN 13"=nn13, 
                            "NN 10"=nn10, 
                            "NN 7"=nn7))
# Boxplot 
png(filename="presentations/NNmodels.png",width=1000, height=750)
bwplot(NLresults,  main="NN Models Comparison",
       scales=list(cex=1,x="free",tck=c(0,0), y=list(cex=3)))
dev.off()

bwplot(NLresults,  main="NN Models Comparison",
       scales=list(cex=1,x="free",tck=c(0,0), y=list(cex=3)))



#Ensemble Models Comparison 
files <- paste("models/",list.files(path="models/", pattern ="cart|rf"),sep="")
for (i in 1:length(files)){load(files[i])};rm(i)
#CART Models Comparison -------------------------------------------------------


Eresults <- resamples(list("CART 13"=cart13, 
                           "CART 10"=cart10, 
                           "CART 7"=cart7))
                           # , 
                           # "RF 13"=rf13, 
                           # "RF 10"=rf10, 
                           # "RF 7"=rf7))
# Boxplot 
png(filename="presentations/CARTModels.png",width=1000, height=750)
bwplot(Eresults, scales=list(cex=1,x="free",tck=c(0,0), y=list(cex=3)), main="CART Models Comparison")
dev.off()

bwplot(Eresults, scales=list(cex=1,x="free",tck=c(0,0), y=list(cex=3)), main="CART Models Comparison")

#RF Models Comparison -------------------------------------------------------

Eresults <- resamples(list( 
                           "RF 13"=rf13, 
                           "RF 10"=rf10, 
                           "RF 7"=rf7))
# Boxplot 
png(filename="presentations/RFModels.png",width=1000, height=750)
bwplot(Eresults, scales=list(cex=1,x="free",tck=c(0,0), y=list(cex=3)), main="RF Models Comparison")
dev.off()

bwplot(Eresults, scales=list(cex=1,x="free",tck=c(0,0), y=list(cex=3)), main="RF Models Comparison")




#Final Models Comparison -------------------------------------------------------

Eresults <- resamples(list( 
  "SVM 13"=svm13, 
  "NN 10"=nn10, 
  "NN 7"=nn7))
# Boxplot 
png(filename="presentations/FinalModels.png",width=1000, height=750)
bwplot(Eresults, scales=list(cex=1,x="free",tck=c(0,0), y=list(cex=3)), cex.axis = 3,main="Final Models Comparison")
dev.off()

bwplot(Eresults, scales=list(cex=1,x="free",tck=c(0,0), y=list(cex=3)), main="Final Models Comparison")
