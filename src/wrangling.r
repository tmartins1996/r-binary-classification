# Data wrangling ---------------------------------------------------------------
#setting up the directory

# setwd('datasets'); list.files()


mug_data <- function(){
  #load historical data from survey  (xlsx)
  histData <- xlsx::read.xlsx('datasets/HistoricalData.xlsx',sheetName = 'Sheet1', 
                              stringsAsFactors = TRUE, header= TRUE)
  str(histData)
  head(histData, n=3)
  
  
  #load human resources file (csv)
  hrData <- read.csv('datasets/HumanResourcesEvaluation.csv', 
                     header=TRUE, sep=";", 
                     stringsAsFactors = TRUE)
  str(hrData)
  head(hrData, n=3)
  names(hrData)[1]<-"EmployeeID"
  
  
  #load data regarding churn indicator (csv)
  testData <- read.csv('datasets/ChurnIndicator.csv', 
                       header=TRUE, sep=",",
                       stringsAsFactors = TRUE)
  str(testData)
  head(testData, n=3)
  names(testData)[1]<-"EmployeeID"
  
  
  #load data from satisfaction survey (txt)
  satisfData<- read.table('datasets/SatisfactionSurvey.txt', 
                          header=TRUE, sep='\t',
                          stringsAsFactors = TRUE)
  str(satisfData)
  head(satisfData, n=3)
  
  subset(idCount<-setNames(aggregate(satisfData$EmployeeID, 
                                     by=list(EmployeeID=satisfData$EmployeeID), FUN=length),
                           c("EmployeeID","freq")),idCount[,2]>1)
  
  #check integrity 
  satisfData[satisfData$EmployeeID==14,]
  satisfData[satisfData$EmployeeID==282,]
  satisfData[satisfData$EmployeeID==620,]
  
  #taking out duplicate records and reset index 
  satisfData<-unique(satisfData)
  rownames(satisfData) <- 1:nrow(satisfData)
  
  
  #merge dataframes in one set, and export to excel
  churnDataset<-merge(histData,hrData, by="EmployeeID")
  churnDataset<-merge(churnDataset,testData, by="EmployeeID")
  churnDataset<-merge(churnDataset,satisfData, by="EmployeeID")
  
  str(churnDataset)
  namesoldCol<-c("Marital.Status","SalaryRise...","BalanceWork.Life")
  namesnewCol<-c("MaritalStatus","SalaryRise","BalanceWorkLife")
  colnames(churnDataset)[which(colnames(churnDataset) %in% namesoldCol )] <-namesnewCol
  
  return <- churnDataset;
}

write_excel <- function(dataset){
  setwd('datasets')
  xlsx::write.xlsx(x = dataset, file = sub(" ", "", paste(deparse(substitute(dataset)),".xlsx")),
                 sheetName = deparse(substitute(dataset)), row.names = F)
  setwd('..')
}

write_excel_report <- function(dataset){
  setwd('reports')
  xlsx::write.xlsx(x = dataset, file = sub(" ", "", paste(deparse(substitute(dataset)),".xlsx")),
                   sheetName = deparse(substitute(dataset)), row.names = F)
  setwd('..')
}


attach_set <- function(){
  # attachig set w/ non-overlapping names
  if(any(sum(search() == "churnDataset"))){
    for ( i in 1:sum(search() == "churnDataset")) {
      detach(churnDataset)
    }
  }
  attach(churnDataset)
}

attach_dataset <- function(){
  # attachig set w/ non-overlapping names
  if(any(sum(search() == "origin_Dataset"))){
    for ( i in 1:sum(search() == "origin_Dataset")) {
      detach(origin_Dataset)
    }
  }
  attach(origin_Dataset)
}


# Create normalize function Minmax 
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}


Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}





