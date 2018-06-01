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
                "C50","caret",'gmodels'))

source("src/wrangling.r")


# Load data ---------------------------------------------------------------
#Load compiled data xlsx
churnDataset <- xlsx::read.xlsx('datasets/churnDataset.xlsx',1,
                        header= TRUE)


# attach_set w/ non-overlapping names and clean Global Env from main.r w/ ls() 
attach_set(); rm(list = setdiff(ls(.GlobalEnv), c("churnDataset" ,lsf.str())))




# Feature engineering -----------------------------------------------------


#'convert BirthDate to char
#'convert BirthDate from serial number to date where no "-"
#'invert the order of original format to be the same as the new one
#'concat 0 to months that have only 1 char

churnDataset$BirthDate <- as.character(churnDataset$BirthDate)
churnDataset$BirthDate <- gsub(" ", "",ifelse(grepl("-", churnDataset$BirthDate),
                                              paste(substr(churnDataset$BirthDate,start = stri_length(churnDataset$BirthDate)-3, stop =stri_length(churnDataset$BirthDate)),
                                                    ifelse(stri_length(str_match(churnDataset$BirthDate, "-(.*?)-")[,2])<2,
                                                           paste("-0",str_match(churnDataset$BirthDate, "-(.*?)-")[,2],"-"),paste("-",str_match(churnDataset$BirthDate, "-(.*?)-")[,2],"-")),
                                                    substr(churnDataset$BirthDate, start = 1, stop = 2))
                                              ,as.character(openxlsx::convertToDate(churnDataset$BirthDate))), fixed = TRUE)



#'index 768 have the following birthdate "1963-02-29"
#'since 63' Fev has only 28 days, and the rest of the data in the row seems valid
#'we opted by put 28 on the day
churnDataset$BirthDate[768] <- "1963-02-28"

#calculate the age to the day 2018-04-24
churnDataset$Age <- as.integer(round((as.Date("2018-04-24")- as.Date(churnDataset$BirthDate))/365,0))


#Gender to binary
churnDataset$isMale <- ifelse((churnDataset$Gender)=="Male",1,0)



# Fill missing values with Mode in numerical values
churnDataset$MaritalStatus[is.na(churnDataset$MaritalStatus)] <- 
  Mode(churnDataset$MaritalStatus)


#MaritalStatus to binary
#churnDataset$isDivorced <- ifelse((churnDataset$MaritalStatus)=="Divorced",1,0)
churnDataset$isTogether <- ifelse((churnDataset$MaritalStatus)=="Together",1,0)
churnDataset$isMarried <- ifelse((churnDataset$MaritalStatus)=="Married",1,0)
churnDataset$isSingle <- ifelse((churnDataset$MaritalStatus)=="Single",1,0)

#JobType to binary
# churnDataset$JobTypeLevels <- ifelse((churnDataset$JobType)=="Office",0,
#                                      ifelse((churnDataset$JobType)=="Office/Remote",1,2))
churnDataset$JobTypeOffice <- ifelse((churnDataset$JobType)=="Office",1,0)
churnDataset$JobTypeRemote <- ifelse((churnDataset$JobType)=="Remote",1,0)

#Collapse duplicated departments
churnDataset$Department <- (churnDataset$Department %>% fct_collapse(
  IT = c("IT","Information Technologies"), HR=c("HR","Human Resources")))

#Department to binary
churnDataset$isDepartComercial <- ifelse(churnDataset$Department=="Commercial",1,0)
churnDataset$isDepartIT <- ifelse(churnDataset$Department=="IT",1,0)
#churnDataset$isDepartHR <- ifelse(churnDataset$Department=="HR",1,0)

#JobRole to binary
churnDataset$isRoleComercial <- ifelse(churnDataset$JobRole=="Commercial",1,0)
churnDataset$isRoleComercialRepr <- ifelse(churnDataset$JobRole=="Commercial Representative",1,0)
churnDataset$isRoleDataSci <- ifelse(churnDataset$JobRole=="Data Scientist",1,0)
churnDataset$isRoleDev <- ifelse(churnDataset$JobRole=="Developer",1,0)
churnDataset$isRoleHR <- ifelse(churnDataset$JobRole=="Human Resources",1,0)
churnDataset$isRoleIT <- ifelse(churnDataset$JobRole=="IT Technician",1,0)
churnDataset$isRoleManager <- ifelse(churnDataset$JobRole=="Manager",1,0)
churnDataset$isRoleMarketing <- ifelse(churnDataset$JobRole=="Marketing Representative",1,0)
#churnDataset$isRoleResearcher <- ifelse(churnDataset$JobRole=="Researcher",1,0)


#Education to binary
churnDataset$isEducAssociate <- ifelse((churnDataset$Education)=="Associate Degree",1,0)
churnDataset$isEducBachelor <- ifelse((churnDataset$Education)=="Bachelor Degree",1,0)
churnDataset$isEducCollege <- ifelse((churnDataset$Education)=="College",1,0)
churnDataset$isEducMasters <- ifelse((churnDataset$Education)=="Masters Degree",1,0)
#churnDataset$isEducPhD <- ifelse((churnDataset$Education)=="PhD",1,0)

#EducationArea to binary
churnDataset$isEducAreaEng <- ifelse((churnDataset$EducationArea)=="Engineering",1,0)
churnDataset$isEducAreaHR <- ifelse((churnDataset$EducationArea)=="Human Resources",1,0)
churnDataset$isEducAreaIT <- ifelse((churnDataset$EducationArea)=="Information Technologies",1,0)
churnDataset$isEducAreaMarketing <- ifelse((churnDataset$EducationArea)=="Marketing",1,0)
churnDataset$isEducAreaOther <- ifelse((churnDataset$EducationArea)=="Other",1,0)
#churnDataset$isEducAreaTechnical <- ifelse((churnDataset$EducationArea)=="Technical Degree",1,0)

#AfterHours to binary
churnDataset$isAfterHours <- ifelse((churnDataset$AfterHours)=="Yes",1,0)


#BalanceWorkLife to levels 1 to 4, same as others var on SatisfactionSurvey
#drop later,just for do the AVG Satisfaction
churnDataset$WorkLifeLevels <- ifelse((churnDataset$BalanceWorkLife)=="Bad",1,
                                       ifelse((churnDataset$BalanceWorkLife)=="Medium",2,
                                              ifelse((churnDataset$BalanceWorkLife)=="Good",3,4)))

# churnDataset$WorkLifeBad <- ifelse((churnDataset$BalanceWorkLife)=="Bad",1,0)
# churnDataset$WorkLifeGood <- ifelse((churnDataset$BalanceWorkLife)=="Good",1,0)   


#AVG from SatisfactionSurvey
churnDataset$avgSatisfaction <- (churnDataset$WorkLifeLevels+
                                    churnDataset$HierarchySatisfaction+
                                        churnDataset$RoleSatisfaction+
                                            churnDataset$FacilitiesSatisfaction)/4

#Ratio NumCompaniesWorked / Age
churnDataset$companiesperAge <- round(churnDataset$NumCompaniesWorked/churnDataset$Age,3)

#Ratio Tenure Company / Working
churnDataset$TcompanyperWorking <- round(churnDataset$TenureCompany/(churnDataset$TenureWorking+1),3)

#Ratio TenureRole / Manager
churnDataset$TroleperManager <- round(churnDataset$TenureRole/(churnDataset$TenureManager+1),3)

#Ratio NumCompaniesWorked / Working
churnDataset$NumcompaniesperWorking <- round(churnDataset$NumCompaniesWorked/(churnDataset$TenureWorking+1),3)


#target var to binary
churnDataset$isChurn <- ifelse((churnDataset$Churn)=="Yes",1,0)


# Write excel Datasets --------------------------------------------
#write processed dataset avoiding overwriting the original
processedDataset <- churnDataset
write_excel(processedDataset)

#write numericDataset, weekhours is unimodal
numericDataset <- processedDataset[sapply(processedDataset,is.numeric)]
drops <-c("WeekHours", "EmployeeID")
numericDataset <-numericDataset[ , !(names(numericDataset) %in% drops)]
write_excel(numericDataset)


# apply normalization function to our dataset
normalizedDataset <- as.data.frame(lapply(numericDataset, normalize))
#write normalizedDataset
write_excel(normalizedDataset)

