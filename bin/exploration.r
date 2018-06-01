#!/usr/bin/env Rscript

#' Created on Fri Apr 13 15:38:28 2018
#' R version 3.4.3 (2017-11-30)
#' 
#' @group   Group 2, DM2 2018 Semester 2
#' @author: Mendes R.
#' @author: Santos R.
#' @author: Martins T.
#'

# EDA Before - PRE-PROCESSING

library(xlsx)
library(RColorBrewer)
library(ggplot2)
library(tidyverse)
library(psych)
library(png)


origin_Dataset <- xlsx::read.xlsx('datasets/churnDataset.xlsx',sheetName = 'churnDataset', 
                            stringsAsFactors = TRUE, header= TRUE)

# dataset description
str(origin_Dataset)

# get column data types
origin_Dataset %>% sapply(class)

# summary statistics
summarystats <- origin_Dataset %>% describe
summarystats
xlsx::write.xlsx(summarystats, 'datasets/summarystats.xlsx', sheetName="summarystats")

summarystats2 <- origin_Dataset %>% summary
summarystats2
xlsx::write.xlsx(summarystats2, 'datasets/summarystats2.xlsx', sheetName="summarystats2")


# missing values
is.na(origin_Dataset) %>% colSums

# check churn population
plyr::count(origin_Dataset$Churn)


# CONSISTENCY CHECKS

# check consistency between total work years vs company years
which(origin_Dataset$TenureCompany > origin_Dataset$TenureWorking)

which(origin_Dataset$TenureRole > origin_Dataset$TenureCompany)

which(origin_Dataset$TenureManager > origin_Dataset$TenureCompany)



#display.brewer.all() # shows the different palettes you can choose from



# churn plot
png(filename="presentations/churn.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_bar(aes(x=origin_Dataset$Churn), fill="deeppink2", color="white")
p <- p + scale_x_discrete(name="Churn") + scale_y_discrete(name="Frequency")
p <- p + ggtitle(label="Churn")
p
dev.off()
p



# gender plot
png(filename="presentations/gender.png",width=760, height=565)
df <- data.frame(origin_Dataset$Gender, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$Gender,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("gender") + ggtitle("Gender vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p


# marital status plot
png(filename="presentations/maritalstatus.png",width=760, height=565)
df <- data.frame(origin_Dataset$MaritalStatus, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$MaritalStatus,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("marital status") + ggtitle("Marital Status vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# dependents plot
png(filename="presentations/dependents.png",width=760, height=565)
df <- data.frame(origin_Dataset$Dependents, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$Dependents,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("dependents") + ggtitle("Dependents vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# dependent box plot
png(filename="presentations/dependentsboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="dependents", y=Dependents, fill=Churn))
p <- p + ggtitle(label=" Dependents")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# job type plot
png(filename="presentations/jobtype.png",width=760, height=565)
df <- data.frame(origin_Dataset$JobType, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$JobType,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("job type") + ggtitle("Job Type vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# department plot
png(filename="presentations/department.png",width=760, height=565)
df <- data.frame(origin_Dataset$Department, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$Department,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("department") + ggtitle("Department vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# job level plot
png(filename="presentations/joblevel.png",width=760, height=565)
df <- data.frame(origin_Dataset$JobLevel, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$JobLevel,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("job level") + ggtitle("Job Level vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# job level box plot
png(filename="presentations/joblevelboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="Job Level", y=JobLevel, fill=Churn))
p <- p + ggtitle(label="Job Levels")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# role plot
png(filename="presentations/role.png",width=760, height=565)
df <- data.frame(origin_Dataset$JobRole, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$JobRole,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("role") + ggtitle("Job Role vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
dev.off()
p

# type contract
png(filename="presentations/typecontract.png",width=760, height=565)
df <- data.frame(origin_Dataset$TypeContract, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$TypeContract,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("type contract") + ggtitle("Type Contract vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# week hours
png(filename="presentations/weakhours.png",width=760, height=565)
df <- data.frame(origin_Dataset$WeekHours, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$WeekHours,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("type contract") + ggtitle("Type Contract vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p



# education
png(filename="presentations/education.png",width=760, height=565)
df <- data.frame(origin_Dataset$Education, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$Education,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("education") + ggtitle("Education vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# education area
png(filename="presentations/educationarea.png",width=760, height=565)
df <- data.frame(origin_Dataset$EducationArea, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$EducationArea,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("education area") + ggtitle("Education Area vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# number of projects
png(filename="presentations/numberprojects.png",width=760, height=565)
df <- data.frame(origin_Dataset$NumberProjectsLastYear, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$NumberProjectsLastYear,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("number projects") + ggtitle("Number Projects vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# number of Projects box plot
png(filename="presentations/numberprojectsboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="number projects", y=NumberProjectsLastYear, fill=Churn))
p <- p + ggtitle(label="Number Projects")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p


# number of companies
png(filename="presentations/companies.png",width=760, height=565)
df <- data.frame(origin_Dataset$NumCompaniesWorked, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$NumCompaniesWorked,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("number companies") + ggtitle("Number Companies vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# number of companies box plot
png(filename="presentations/companiesboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="number companies", y=NumCompaniesWorked, fill=Churn))
p <- p + ggtitle(label="Number Companies")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# percent of salary rise
png(filename="presentations/salaryrise.png",width=760, height=565)
df <- data.frame(origin_Dataset$SalaryRise, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$SalaryRise,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("salary rise") + ggtitle("salary rise vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# percent of salary box plot
png(filename="presentations/salaryriseboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="salary rise", y=SalaryRise, fill=Churn))
p <- p + ggtitle(label="Salary Rise")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# last promotion
png(filename="presentations/lastpromotion.png",width=760, height=565)
df <- data.frame(origin_Dataset$LastPromotion, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$LastPromotion,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("last promotion") + ggtitle("Last Promotion vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p


# last promotion box plot
png(filename="presentations/lastpromotionboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="last promotion", y=LastPromotion, fill=Churn))
p <- p + ggtitle(label="Last Promotion")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p



# job dedication
png(filename="presentations/jobdedication.png",width=760, height=565)
df <- data.frame(origin_Dataset$JobDedication, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$JobDedication,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("job dedication") + ggtitle("Job Dedication vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# job dedication box plot
png(filename="presentations/jobdedicationboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="job dedication", y=JobDedication, fill=Churn))
p <- p + ggtitle(label="Job Dedication")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# afterhours
png(filename="presentations/afterhours.png",width=760, height=565)
df <- data.frame(origin_Dataset$AfterHours, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$AfterHours,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("after hours") + ggtitle("After Hours vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# job performance
png(filename="presentations/jobperformance.png",width=760, height=565)
df <- data.frame(origin_Dataset$JobPerformance, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$JobPerformance,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("job performance") + ggtitle("Job Performance vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# job performation box plot
png(filename="presentations/jobperformanceboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="job performance", y=JobPerformance, fill=Churn))
p <- p + ggtitle(label="Job Performance")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p


# facilities satisfaction
png(filename="presentations/facilitiessatisfaction.png",width=760, height=565)
df <- data.frame(origin_Dataset$FacilitiesSatisfaction, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$FacilitiesSatisfaction,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("facierilities satisfaction") + ggtitle("Facilities Satisfaction vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# facilities satisfaction box plot
png(filename="presentations/facilitiessatisfactionboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="facilities satisfaction", y=FacilitiesSatisfaction, fill=Churn))
p <- p + ggtitle(label="Facilities Satisfaction")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# role satisfaction
png(filename="presentations/rolesatisfaction.png",width=760, height=565)
df <- data.frame(origin_Dataset$RoleSatisfaction, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$RoleSatisfaction,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("role satisfaction") + ggtitle("Role Satisfaction vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# role satisfaction box plot
png(filename="presentations/rolesatisfactionboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="role satisfaction", y=RoleSatisfaction, fill=Churn))
p <- p + ggtitle(label="Role Satisfaction")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p



# hierarchy satisfaction
png(filename="presentations/hierarchysatisfaction.png",width=760, height=565)
df <- data.frame(origin_Dataset$HierarchySatisfaction, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$HierarchySatisfaction,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("hierarchy satisfaction") + ggtitle("Hierarchy Satisfaction vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# hierarchy satisfaction box plot
png(filename="presentations/hierarchysatisfactionboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="hierarchy satisfaction", y=HierarchySatisfaction, fill=Churn))
p <- p + ggtitle(label="Hierarchy Satisfaction")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p


# tenure working
png(filename="presentations/tenureworking.png",width=760, height=565)
df <- data.frame(origin_Dataset$TenureWorking, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$TenureWorking,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("tenure working") + ggtitle("Tenure Working vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# tenure working box plot
png(filename="presentations/tenureworkingboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="tenure working", y=TenureWorking, fill=Churn))
p <- p + ggtitle(label="Tenure Working")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p



# tenure company
png(filename="presentations/tenurecompany.png",width=760, height=565)
df <- data.frame(origin_Dataset$TenureCompany, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$TenureCompany,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("tenure company") + ggtitle("Tenure company vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# tenure company box plot
png(filename="presentations/tenurecompanyboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="tenure company", y=TenureCompany, fill=Churn))
p <- p + ggtitle(label="Tenure Company")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# tenure role
png(filename="presentations/tenurerole.png",width=760, height=565)
df <- data.frame(origin_Dataset$TenureRole, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$TenureRole,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("tenure role") + ggtitle("Tenure Role vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# tenure role box plot
png(filename="presentations/tenureroleboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="tenure role", y=TenureRole, fill=Churn))
p <- p + ggtitle(label="Tenure Role")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p


# tenure manager
png(filename="presentations/tenuremanager.png",width=760, height=565)
df <- data.frame(origin_Dataset$TenureManager, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$TenureManager,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("tenure manager") + ggtitle("Tenure Manager vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# tenure manager box plot
png(filename="presentations/tenuremanagerboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="tenure manager", y=TenureManager, fill=Churn))
p <- p + ggtitle(label="Tenure Manager")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p


# balance worklife
png(filename="presentations/balanceworklife.png",width=760, height=565)
df <- data.frame(origin_Dataset$BalanceWorkLife, origin_Dataset$Churn)
p <- ggplot(df, aes(origin_Dataset$BalanceWorkLife,..count..)) + geom_bar(aes(fill = origin_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("balance worklife") + ggtitle("Balance Worklife vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p
# Numerical continous VARIABLES

# income
png(filename="presentations/income.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_histogram(aes(x=origin_Dataset$MonthlyIncome), fill="maroon3", color="white")
p <- p + scale_x_continuous(name="Income") + scale_y_continuous(name="Frequency")
p <- p + ggtitle(label="Income")
p
dev.off()
p


# distance home office
png(filename="presentations/distancehomeoffice.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_histogram(aes(x=origin_Dataset$DistanceHomeOffice), fill="maroon3", color="white")
p <- p + scale_x_continuous(name="Distance") + scale_y_continuous(name="Frequency")
p <- p + ggtitle(label="Distance Home / Office")
p
dev.off()
p


# distance box plot
png(filename="presentations/distancehomeofficeboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="Distance", y=DistanceHomeOffice, fill=Churn))
p <- p + ggtitle(label="Distance Home / Office")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p


# box plot #

# income

png(filename="presentations/incomeboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x = "income", y=MonthlyIncome, fill = "income"))
p <- p + ggtitle(label="Monthly Income")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

png(filename="presentations/lastpromotionboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x = "last promotion", y=LastPromotion, fill = ""))
p <- p + ggtitle(label="Last Promotion")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

png(filename="presentations/tenureworkingboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x = "tenure working", y=TenureWorking, fill = ""))
p <- p + ggtitle(label="Tenure Working")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

png(filename="presentations/tenurecompanyboxplot.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x = "tenure company", y=TenureCompany, fill = ""))
p <- p + ggtitle(label="Tenure Company")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# box plot Income
png(filename="presentations/incomeboxplotchurn.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="income", y=MonthlyIncome, fill=Churn))
p <- p + ggtitle(label=" Monthly Income")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# distance home / office
png(filename="presentations/distancehomeofficeboxplotchurn.png",width=760, height=565)
p <- ggplot(data=origin_Dataset) + geom_boxplot(aes(x="Distance", y=DistanceHomeOffice, fill=Churn))
p <- p + ggtitle(label="Distance Home / Office")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p


# --------------------------------------------------------------------------------------------------
# POST PROCESSING ANALYSIS
# --------------------------------------------------------------------------------------------------

processed_Dataset <- xlsx::read.xlsx('datasets/processedDataset.xlsx',sheetName = 'processedDataset', 
                                  stringsAsFactors = TRUE, header= TRUE)

# summary statistics
summarystats <- processed_Dataset %>% describe
summarystats
write.xlsx(summarystats, 'datasets/processedsummarystats.xlsx', sheetName="summarystats")

summarystats2 <- processed_Dataset %>% summary
summarystats2
write.xlsx(summarystats2, 'datasets/processedsummarystats2.xlsx', sheetName="summarystats2")

# Age plot
png(filename="presentations/age.png",width=760, height=565)
df <- data.frame(processed_Dataset$Age, processed_Dataset$Churn)
p <- ggplot(df, aes(processed_Dataset$Age,..count..)) + geom_bar(aes(fill = processed_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("Age") + ggtitle("Age vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p


# age box plot
png(filename="presentations/ageboxplot.png",width=760, height=565)
p <- ggplot(data=processed_Dataset) + geom_boxplot(aes(x="Age", y=DistanceHomeOffice, fill=Churn))
p <- p + ggtitle(label="Age")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p


# Avg Satisfaction plot
png(filename="presentations/averagesatisfaction.png",width=760, height=565)
df <- data.frame(processed_Dataset$avgSatisfaction, origin_Dataset$Churn)
p <- ggplot(df, aes(processed_Dataset$avgSatisfaction,..count..)) + geom_bar(aes(fill = processed_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("Age") + ggtitle("Average Satisfaction vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p


# Avg Satisfaction box plot
png(filename="presentations/averagesatisfactionboxplot.png",width=760, height=565)
p <- ggplot(data=processed_Dataset) + geom_boxplot(aes(x="Average Satisfaction", y=avgSatisfaction, fill=Churn))
p <- p + ggtitle(label="Average Satisfaction")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p


# balance worklife plot
png(filename="presentations/worklifelevels.png",width=760, height=565)
df <- data.frame(processed_Dataset$WorkLifeLevels, processed_Dataset$Churn)
p <- ggplot(df, aes(processed_Dataset$WorkLifeLevels,..count..)) + geom_bar(aes(fill = processed_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("Balance Work/Life") + ggtitle("Balance Work/Life vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p


# balance worklife box plot
png(filename="presentations/worklifelevelsboxplot.png",width=760, height=565)
p <- ggplot(data=processed_Dataset) + geom_boxplot(aes(x="Balance Work/Life", y=WorkLifeLevels, fill=Churn))
p <- p + ggtitle(label="Balance Work/Life")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p


# companies/age histogram
png(filename="presentations/companiesperagehist.png",width=760, height=565)
p <- ggplot(data=processed_Dataset) + geom_histogram(aes(x=processed_Dataset$companiesperAge), fill="maroon3", color="white")
p <- p + scale_x_continuous(name="Companies/Age") + scale_y_continuous(name="")
p <- p + ggtitle(label="Companies per Age")
p
dev.off()
p

# companies/age plot
png(filename="presentations/companiesperage.png",width=760, height=565)
df <- data.frame(processed_Dataset$companiesperAge, processed_Dataset$Churn)
p <- ggplot(df, aes(processed_Dataset$companiesperAge,..count..)) + geom_bar(aes(fill = processed_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("Companies/Age") + ggtitle("Companies per Age vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# companies/age box plot
png(filename="presentations/companiesperageboxplot.png",width=760, height=565)
p <- ggplot(data=processed_Dataset) + geom_boxplot(aes(x="Companies/Age", y=companiesperAge, fill=Churn))
p <- p + ggtitle(label="Companies per Age")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# Tenure company / working histogram
png(filename="presentations/tenurecompanyworkinghist.png",width=760, height=565)
p <- ggplot(data=processed_Dataset) + geom_histogram(aes(x=processed_Dataset$TcompanyperWorking), fill="maroon3", color="white")
p <- p + scale_x_continuous(name="Tenure Company/Working") + scale_y_continuous(name="")
p <- p + ggtitle(label="Tenure Company/Working")
p
dev.off()
p

# tenure company / working plot
png(filename="presentations/tenurecompanyworking.png",width=760, height=565)
df <- data.frame(processed_Dataset$TcompanyperWorking, processed_Dataset$Churn)
p <- ggplot(df, aes(processed_Dataset$TcompanyperWorking,..count..)) + geom_bar(aes(fill = processed_Dataset$Churn),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("Tenure Company/Working") + ggtitle("Tenure Company / Working vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p

# tenure company / working box plot
png(filename="presentations/tenurecompanyworkingboxplot.png",width=760, height=565)
p <- ggplot(data=processed_Dataset) + geom_boxplot(aes(x="Tenure Company/Working", y=TcompanyperWorking, fill=Churn))
p <- p + ggtitle(label="Tenure Company / Working")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p



# tenure role/manager histogram
png(filename="presentations/tenurerolemanagerhist.png",width=760, height=565)
p <- ggplot(data=processed_Dataset) + geom_histogram(aes(x=processed_Dataset$TroleperManager), fill="maroon3", color="white")
p <- p + scale_x_continuous(name="Tenure Role/Manager") + scale_y_continuous(name="")
p <- p + ggtitle(label="Tenure Role/Manager")
p
dev.off()
p


# tenure role/manager box plot
png(filename="presentations/tenurerolemanagerboxplot.png",width=760, height=565)
p <- ggplot(data=processed_Dataset) + geom_boxplot(aes(x="Tenure Role/Manager", y=TroleperManager, fill=Churn))
p <- p + ggtitle(label="Tenure Role/Manager")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p


# companies/tenure working histogram
png(filename="presentations/companiesperworkinghist.png",width=760, height=565)
p <- ggplot(data=processed_Dataset) + geom_histogram(aes(x=processed_Dataset$NumcompaniesperWorking), fill="maroon3", color="white")
p <- p + scale_x_continuous(name="Companies / Tenure Working") + scale_y_continuous(name="")
p <- p + ggtitle(label="Companies / Tenure Working")
p
dev.off()
p


# companies/tenure working box plot
png(filename="presentations/companiesperworkingboxplot.png",width=760, height=565)
p <- ggplot(data=processed_Dataset) + geom_boxplot(aes(x="Companies / Tenure Working", y=NumcompaniesperWorking, fill=Churn))
p <- p + ggtitle(label="Companies / Tenure Working")
p <- p + facet_wrap(~Churn)
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p


# marital status plot
png(filename="presentations/processed_maritalstatus.png",width=760, height=565)
df <- data.frame(processed_Dataset$MaritalStatus, origin_Dataset$Churn)
p <- ggplot(df, aes(processed_Dataset$MaritalStatus,..count..)) + geom_bar(aes(fill = processed_Dataset$MaritalStatus),position = "stack")
p <- p + guides(fill=guide_legend(title="Churn")) + xlab("Marital Status") + ggtitle("Marital Status vs Churn")
p <- p + scale_fill_manual(values = brewer.pal(8,"PiYG"))
p
dev.off()
p






