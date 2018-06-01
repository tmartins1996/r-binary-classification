# Libs --------------------------------------------------------------------
  

include_packs <- function(list.of.packages){ 
  #' 
  #' Check to see if packages are installed. 
  #' Install them if they are not, then load them into the R session.
  
  new.packages <- list.of.packages[!(list.of.packages  %in% 
                                       installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
  lapply(list.of.packages, require, character.only = TRUE)
  rm(list = setdiff(ls(), lsf.str()))
  
}


draw_confusion_matrix_smote <- function(cm,string) {
  
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title(paste('         Confusion matrix', string), cex.main=1.8)
  
  # create the matrix "#DE77AE","#C51B7D"
  rect(130, 430, 220, 370, col='#C51B7D')
  text(175, 435, 'Yes', cex=1.2)
  rect(230, 430, 320, 370, col='#DE77AE')
  text(275, 435, 'No', cex=1.2)
  text(115, 370, 'Actual', cex=1.8, srt=90, font=2)
  text(225, 445, 'Predicted', cex=1.8, font=2)
  rect(130, 305, 220, 365, col='#DE77AE')
  rect(230, 305, 320, 365, col='#C51B7D')
  text(127, 400, 'Yes', cex=1.2, srt=90)
  text(127, 335, 'No', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(175, 400, res[4], cex=1.6, font=2, col='white')
  text(175, 335, res[2], cex=1.6, font=2, col='white')
  text(275, 400, res[3], cex=1.6, font=2, col='white')
  text(275, 335, res[1], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(98, 0), c(100, 0), type = "n", xlab="", ylab="", main = "        DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.3, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.3, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$overall[2]), cex=1.3, font=2)
  text(50, 70, round(as.numeric(cm$overall[2]), 3), cex=1.2) 
  text(70, 85, names(cm$byClass[6]), cex=1.3, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.3, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  text(20, 37, "Trainset", cex=1.3, font=2)
  text(20, 27, "(Bootstrap)", cex=1.3)
  text(20, 17, "1628", cex=1.2)
  text(80, 35, "Testset", cex=1.3, font=2)
  text(80, 20, "450", cex=1.2)
  
  
  
  # add in the accuracy information 
  text(38, 35, names(cm$overall[1]), cex=1.7, font=2)
  text(38, 20, round(as.numeric(cm$overall[1]), 3), cex=1.65)
  text(62, 35, names(cm$byClass[5]), cex=1.7, font=2)
  text(62, 20, round(as.numeric(cm$byClass[5]), 3), cex=1.65)
  
}  

#creating a new function because a param don't solve
draw_confusion_matrix_cv <- function(cm, string) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title(paste('         Confusion matrix', string), cex.main=1.8)
  
  # create the matrix "#DE77AE","#C51B7D"
  rect(130, 430, 220, 370, col='#C51B7D')
  text(175, 435, 'Yes', cex=1.2)
  rect(230, 430, 320, 370, col='#DE77AE')
  text(275, 435, 'No', cex=1.2)
  text(115, 370, 'Actual', cex=1.8, srt=90, font=2)
  text(225, 445, 'Predicted', cex=1.8, font=2)
  rect(130, 305, 220, 365, col='#DE77AE')
  rect(230, 305, 320, 365, col='#C51B7D')
  text(127, 400, 'Yes', cex=1.2, srt=90)
  text(127, 335, 'No', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(175, 400, res[4], cex=1.6, font=2, col='white')
  text(175, 335, res[2], cex=1.6, font=2, col='white')
  text(275, 400, res[3], cex=1.6, font=2, col='white')
  text(275, 335, res[1], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(98, 0), c(100, 0), type = "n", xlab="", ylab="", main = "        DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.3, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.3, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$overall[2]), cex=1.3, font=2)
  text(50, 70, round(as.numeric(cm$overall[2]), 3), cex=1.2) 
  text(70, 85, names(cm$byClass[6]), cex=1.3, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.3, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  text(20, 35, "Trainset CV", cex=1.3, font=2)
  text(20, 27, "(10 Folds)", cex=1.3)
  text(20, 20, "1000", cex=1.2)
  text(80, 35, "Testset", cex=1.3, font=2)
  text(80, 20, "450", cex=1.2)
  
  
  # add in the accuracy information 
  text(38, 35, names(cm$overall[1]), cex=1.7, font=2)
  text(38, 20, round(as.numeric(cm$overall[1]), 3), cex=1.65)
  text(62, 35, names(cm$byClass[5]), cex=1.7, font=2)
  text(62, 20, round(as.numeric(cm$byClass[5]), 3), cex=1.65)
  
}  

draw_confusion_matrix_boot <- function(cm,string) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title(paste('         Confusion matrix', string), cex.main=1.8)
  
  # create the matrix "#DE77AE","#C51B7D"
  rect(130, 430, 220, 370, col='#C51B7D')
  text(175, 435, 'Yes', cex=1.2)
  rect(230, 430, 320, 370, col='#DE77AE')
  text(275, 435, 'No', cex=1.2)
  text(115, 370, 'Actual', cex=1.8, srt=90, font=2)
  text(225, 445, 'Predicted', cex=1.8, font=2)
  rect(130, 305, 220, 365, col='#DE77AE')
  rect(230, 305, 320, 365, col='#C51B7D')
  text(127, 400, 'Yes', cex=1.2, srt=90)
  text(127, 335, 'No', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(175, 400, res[4], cex=1.6, font=2, col='white')
  text(175, 335, res[2], cex=1.6, font=2, col='white')
  text(275, 400, res[3], cex=1.6, font=2, col='white')
  text(275, 335, res[1], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(98, 0), c(100, 0), type = "n", xlab="", ylab="", main = "        DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.3, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.3, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$overall[2]), cex=1.3, font=2)
  text(50, 70, round(as.numeric(cm$overall[2]), 3), cex=1.2) 
  text(70, 85, names(cm$byClass[6]), cex=1.3, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.3, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  text(20, 35, "Trainset", cex=1.3, font=2)
  text(20, 27, "(25 Bootstrap)", cex=1.3)
  text(20, 20, "1000", cex=1.2)
  text(80, 35, "Testset", cex=1.3, font=2)
  text(80, 20, "450", cex=1.2)
  
  
  # add in the accuracy information 
  text(38, 35, names(cm$overall[1]), cex=1.7, font=2)
  text(38, 20, round(as.numeric(cm$overall[1]), 3), cex=1.65)
  text(62, 35, names(cm$byClass[5]), cex=1.7, font=2)
  text(62, 20, round(as.numeric(cm$byClass[5]), 3), cex=1.65)
  
}  

# Put histograms on the diagonal (from "pairs" help)
panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y,  ...)
  # Removed "col = "cyan" from code block; original below
  # rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...) 
}


corr_plot <- function(corr){
  ggcorrplot(corr, hc.order = TRUE, 
             type = "lower", 
             lab = TRUE, 
             lab_size = 3, 
             method="circle", 
             colors = c("tomato2", "white", "springgreen3"), 
             title="Correlogram plot", 
             ggtheme=theme_bw)
}



stratification <- function(normalizedDataset,trainData,testData,string){
  
  df1 <- data.frame("Original", normalizedDataset$isChurn)
  names(df1)[1:2]<-c("head","Churn")
  df2 <- data.frame("CV / Bootstrap", trainData$isChurn)
  names(df2)[1:2]<-c("head","Churn")
  df3 <- data.frame("Testset", testData$isChurn)
  names(df3)[1:2]<-c("head","Churn")
  df<-rbind(df1,df2)
  df<-rbind(df,df3)
  
  
  d2 <- df %>% 
    group_by(head,Churn) %>% 
    summarise(count=n()) %>% 
    mutate(perc=count/sum(count))
  
  # inv_perc<-d2[c(2,1,4,3,6,5),4]
  inv_perc<-d2[c(1,2,3,4,5,6),4]
  inv_perc <- round(inv_perc,2)
  
  ggplot(d2, aes(x = factor(head), y = inv_perc, fill = factor(Churn,
                                                               levels = c("1", "0")))) +
    geom_bar(stat="identity", width = 0.7,position = position_fill(reverse = FALSE)) +
    labs(x = "Datasets", y = "Percent", fill = "Churn")+ggtitle(paste("Stratification",string))+
    geom_text(aes(label = inv_perc), size = 3, position = position_stack(vjust = 0.5))+
    theme_minimal(base_size = 14)+ 
    scale_fill_manual(values = c("#DE77AE","#C51B7D"))+
    theme_update(plot.title = element_text(hjust = 0.5))
  
}


plotCut <- function(ROCR,string){
  perf1<- ROCR::performance(ROCR_test, "prec")
  perf2<- ROCR::performance(ROCR_test, "rec")
  
  par(mar=c(5, 4, 4, 6) + 0.1)
  plot(perf1,  axes=F, ylim=c(0,1),xlab="", ylab="",  
       lwd=3, col="purple3", main=paste(string,"Precision-Recal Tradeoff"))
  box(); par(new=TRUE)
  plot(perf2, xlab="", ylab="", ylim=c(0,1), 
       axes=FALSE, col="deeppink",lwd=3)
  mtext("Precision",side=2,col="purple3",line=2.5,lwd=3,cex=1.4)
  mtext("Recal",side=4,col="deeppink",line=4,lwd=3,cex=1.4) 
  mtext("Threshold",side=1,col="black",line=2.5,lwd=3) 
  axis(4, ylim=c(0,1), col="deeppink",col.axis="deeppink")
  axis(2, ylim=c(0,1),col="purple3",col.axis="purple3")  
  axis(1, ylim=c(0,1), col="black",col.axis="black")
}

corr_target <- function(set){
  #Checking correlation in relation to churn
  corr <- round(cor(set, method = "kendall"), 1)
  theme_set(theme_bw())  
  df_corr<-as.data.frame(rownames(corr)[1:(ncol(set)-1)])
  df_corr$corr<- corr[1:(ncol(set)-1),(ncol(set))]
  df_corr$type <- ifelse(df_corr$corr < 0, "below", "above") 
  df_corr <- df_corr[order(df_corr$corr), ]
  df_corr$rownames <- factor(df_corr$rownames, levels = df_corr$rownames)
  df_corr <- df_corr[!df_corr$corr==0,]
  
  # Diverging Barcharts
  ggplot(df_corr, aes(x=rownames, y=corr, label=corr)) + 
    geom_bar(stat='identity', aes(fill=type), width=.5)  +
    scale_fill_manual(name="Corr", 
                      labels = c("Positive Correlation", "Negative Correlation"), 
                      values = c("above"="#00ba38", "below"="#f8766d")) + 
    labs(subtitle="In relation to Churn var", 
         title= "Correlation Bar Plot") + 
    coord_flip()
}



stacked_plot <- function(df){
  df <- melt(df, id.vars = 'Churn' )
  df[, Percent := prop.table(value)*100, by = 'Churn']
  df
  
  ggplot(data = df, aes( x = Churn, y = Percent, fill = variable, label = Percent )) + 
    geom_bar(stat = 'identity') + 
    geom_text(size = 4, position = position_stack(vjust = 0.5)) 
}
