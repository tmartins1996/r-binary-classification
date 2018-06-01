#summary statistics

churnDataset %>% describe
churnDataset %>% summary

is.na(churnDataset) %>% colSums
MaritalStatus %>% summary
is.na(MaritalStatus) %>% which

plyr::count(Churn)

churnDataset %>% sapply(class)

plyr::count(Education)


with(churnDataset[Dependents==0,] , plot(MaritalStatus, 
                                         ylim=c(0,155),
                                         col = "dark blue"))

with(churnDataset[Churn=="No",] , plot(MonthlyIncome, 
                                       col = "dark blue"))
with(churnDataset[Churn=="Yes",] , plot(MonthlyIncome, 
                                        col = "dark blue"))





#0 anos a trabalhar? 
TenureWorking %>% min
# %<>%
which(TenureWorking==0)
churnDataset[which((churnDataset$TenureWorking)==0),'NumberProjectsLastYear']


# EDA | BirthDate & Age & VerifyAge --------------------------------------------

#BirthDate factor w/ 1378 levels out of 1450 obs.
str(BirthDate)
sum(is.na(BirthDate))
head(BirthDate, n=3);tail(BirthDate, n=3)




#Age - TenureWorking
min(churnDataset$Age-churnDataset$TenureWorking)
max(churnDataset$Age-churnDataset$TenureWorking)
which((churnDataset$Age-churnDataset$TenureWorking)==10)
churnDataset[which((churnDataset$Age-churnDataset$TenureWorking)==10),'Age']
churnDataset[which((churnDataset$Age-churnDataset$TenureWorking)==10),'TenureWorking']

# attach_set w/ non-overlapping names and clean Global Env from main.r w/ ls() 
attach_set(); rm(list = setdiff(ls(.GlobalEnv), c("churnDataset" ,lsf.str())))



# EDA | Gender & isMale ---------------------------------------------------------
plyr::count(Gender)

# attach_set w/ non-overlapping names and clean Global Env from main.r w/ ls() 
attach_set(); rm(list = setdiff(ls(.GlobalEnv), c("churnDataset" ,lsf.str())))



# EDA | MaritalStatus & isMaritalStatus --------------------------------------------------

levels(MaritalStatus)
plyr::count(MaritalStatus)
round(plyr::count(churnDataset$MaritalStatus)$freq/nrow(churnDataset),3)*100
with(churnDataset[Churn == "Yes",], 
     round(plyr::count(MaritalStatus)$freq/plyr::count(churnDataset$Churn)$freq[2],3))*100
with(churnDataset[Churn == "No" ,], 
     round(plyr::count(MaritalStatus)$freq/plyr::count(churnDataset$Churn)$freq[1],3))*100




MaritalStatus %>% summary
is.na(MaritalStatus) %>% which

# Fill missing values with Mode in numerical values
churnDataset$MaritalStatus[is.na(churnDataset$MaritalStatus)] <- 
  Mode(churnDataset$MaritalStatus, na.rm = TRUE)

Mode(churnDataset$MaritalStatus, na.rm = TRUE)





# attach_set w/ non-overlapping names and clean Global Env from main.r w/ ls() 
attach_set(); rm(list = setdiff(ls(.GlobalEnv), c("churnDataset" ,lsf.str())))

# EDA | JobType & JobTypeLevels  --------------------------------------------------------

#is office? enviroment is bad?
JobType %>% levels
round(plyr::count(churnDataset$JobType)$freq/nrow(churnDataset),3)*100
with(churnDataset[Churn == "Yes",], 
     round(plyr::count(JobType)$freq/plyr::count(churnDataset$Churn)$freq[2],2))*100
with(churnDataset[Churn == "No" ,], 
     round(plyr::count(JobType)$freq/plyr::count(churnDataset$Churn)$freq[1],3))*100

df <- fread('Churn Office Office/Remote Remote
             " Global" 18.6 71.4 10.0
              " Yes" 28 67  5
              "No" 16.8 72.2 11.0')

stacked_plot(df)

# attach_set w/ non-overlapping names and clean Global Env from main.r w/ ls() 
attach_set(); rm(list = setdiff(ls(.GlobalEnv), c("churnDataset" ,lsf.str())))


# EDA | Department & Departments & isDepart -------------------------------------

#Collapse levels of IT and Comercial in a new var
Department %>% levels


churnDataset$Departments %>% levels

#create var isDepartmentComercial?
round(plyr::count(churnDataset$Departments)$freq/nrow(churnDataset),3)*100
with(churnDataset[Churn == "Yes",], 
     round(plyr::count(Departments)$freq/plyr::count(churnDataset$Churn)$freq[2],3))*100
with(churnDataset[Churn == "No" ,], 
     round(plyr::count(Departments)$freq/plyr::count(churnDataset$Churn)$freq[1],3))*100

df <- fread('Churn Commercial HR IT
             " Global" 30.3  4.3 65.4
             " Yes" 38.7  5.2 56.1
             "No" 28.7  4.2 67.1')

stacked_plot(df)




# attach_set w/ non-overlapping names and clean Global Env from main.r w/ ls() 
attach_set(); rm(list = setdiff(ls(.GlobalEnv), c("churnDataset" ,lsf.str())))


# EDA | JobRole & isRole --------------------------------------------------------

#create var isCommercial Representative/IT Technician?
JobRole %>% levels
round(plyr::count(churnDataset$JobRole)$freq/nrow(churnDataset),3)*100
with(churnDataset[Churn == "Yes",], 
     round(plyr::count(JobRole)$freq/plyr::count(churnDataset$Churn)$freq[2],3))*100
with(churnDataset[Churn == "No" ,], 
     round(plyr::count(JobRole)$freq/plyr::count(churnDataset$Churn)$freq[1],3))*100





# attach_set w/ non-overlapping names and clean Global Env from main.r w/ ls() 
attach_set(); rm(list = setdiff(ls(.GlobalEnv), c("churnDataset" ,lsf.str())))




#Only Fixed term on TypeContract
TypeContract %>% levels

# EDA | Education & isEduc ------------------------------------------------------

Education %>% levels
plyr::count(Education)
round(plyr::count(churnDataset$Education)$freq/nrow(churnDataset),3)*100
with(churnDataset[Churn == "Yes",], 
     round(plyr::count(Education)$freq/plyr::count(churnDataset$Churn)$freq[2],3))*100
with(churnDataset[Churn == "No" ,], 
     round(plyr::count(Education)$freq/plyr::count(churnDataset$Churn)$freq[1],3))*100



# attach_set w/ non-overlapping names and clean Global Env from main.r w/ ls() 
attach_set(); rm(list = setdiff(ls(.GlobalEnv), c("churnDataset" ,lsf.str())))



# EDA | EducationArea & isEducArea ----------------------------------------

EducationArea %>% levels
plyr::count(EducationArea)
round(plyr::count(churnDataset$EducationArea)$freq/nrow(churnDataset),3)*100
with(churnDataset[Churn == "Yes",], 
     round(plyr::count(EducationArea)$freq/plyr::count(churnDataset$Churn)$freq[2],3))*100
with(churnDataset[Churn == "No" ,], 
     round(plyr::count(EducationArea)$freq/plyr::count(churnDataset$Churn)$freq[1],3))*100


# attach_set w/ non-overlapping names and clean Global Env from main.r w/ ls() 
attach_set(); rm(list = setdiff(ls(.GlobalEnv), c("churnDataset" ,lsf.str())))


# EDA | AfterHours & isAfterHours -----------------------------------------

AfterHours %>% levels
plyr::count(AfterHours)
round(plyr::count(churnDataset$AfterHours)$freq/nrow(churnDataset),3)*100
with(churnDataset[Churn == "Yes",], 
     round(plyr::count(AfterHours)$freq/plyr::count(churnDataset$Churn)$freq[2],3))*100
with(churnDataset[Churn == "No" ,], 
     round(plyr::count(AfterHours)$freq/plyr::count(churnDataset$Churn)$freq[1],3))*100



# attach_set w/ non-overlapping names and clean Global Env from main.r w/ ls() 
attach_set(); rm(list = setdiff(ls(.GlobalEnv), c("churnDataset" ,lsf.str())))



# EDA | Churn & isChurn ---------------------------------------------------

Churn %>% levels
plyr::count(Churn)


# attach_set w/ non-overlapping names and clean Global Env from main.r w/ ls() 
attach_set(); rm(list = setdiff(ls(.GlobalEnv), c("churnDataset" ,lsf.str())))




# EDA | BalanceWorkLife & WorkLifeLevels ------------------------------------------

BalanceWorkLife %>% levels
plyr::count(BalanceWorkLife)
round(plyr::count(churnDataset$BalanceWorkLife)$freq/nrow(churnDataset),3)*100
with(churnDataset[Churn == "Yes",], 
     round(plyr::count(BalanceWorkLife)$freq/plyr::count(churnDataset$Churn)$freq[2],3))*100
with(churnDataset[Churn == "No" ,], 
     round(plyr::count(BalanceWorkLife)$freq/plyr::count(churnDataset$Churn)$freq[1],3))*100


# attach_set w/ non-overlapping names and clean Global Env from main.r w/ ls() 
attach_set(); rm(list = setdiff(ls(.GlobalEnv), c("churnDataset" ,lsf.str())))





# Charts temp. ------------------------------------------------------------


#  PIE CHARTS - three data sets
pie.a <- round(plyr::count(churnDataset$Departments)$freq/1450,2)
pie.b <- with(churnDataset[Churn=="Yes",] , round(plyr::count(Departments)$freq/230,2))
pie.c <- with(churnDataset[Churn=="No",] , round(plyr::count(Departments)$freq/1220,2))

# Changing graphical parameters for a minute
oldpar <- par()   # Stores old graphical parameters
par(mfrow    = c(1, 3),  # Num. rows/cols
    cex.main = 3)   #  Main title 3x bigger
colors <- c("chocolate1", "blue", "chartreuse4")


# Three pie charts side by side
# Is the green slice or blue slice bigger?
pie(pie.a, main = "Global", col = colors)
pie(pie.b, main = "Yes", col = colors)
pie(pie.c, main = "No", col = colors)

par(oldpar)


require(data.table)
require(ggplot2)

levels(Departments)




library(treemap)

plyr::count(JobRole)
# Create data
group=plyr::count(JobRole)$x
value=plyr::count(JobRole)$freq
data=data.frame(group,value)
data

# treemap

png(filename="presentations/tree.png",width=800, height=800)


treemap(data,
        index="group",
        vSize="value",
        type="index",
        fontsize.labels=c(10,1),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("white"),    # Color of labels
        fontface.labels=c(1,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels=c("transparent"),              # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ),    
        palette = "Set2",# Where to place labels in the rectangle?
        overlap.labels=.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=TRUE,    
        title="My Treemap",                      # Customize your title
        fontsize.title=12
)

dev.off()

library(ggplot2)
ggplot(churnDataset, aes(x=Departments, fill=Churn)) +
  geom_histogram()






library(plyr)
library(psych)
multi.hist(churnDataset[,sapply(churnDataset, is.numeric)])


display.brewer.pal(3, "Pastel2")

# Put histograms on the diagonal (from "pairs" help)
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y,  ...)
  # Removed "col = "cyan" from code block; original below
  # rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...) 
}

pairs(churnDataset$JobLevel~MonthlyIncome, 
      panel = panel.smooth,  # Optional smoother
      main = "Scatterplot Matrix for ChurnDataset Using pairs Function",
      diag.panel = panel.hist, 
      pch = 16, 
      col = brewer.pal(2, "Pastel1")[unclass(churnDataset$Churn)])
      # col = "coral3")


names(churnDataset)

# Correlation matrix
numeric_df <- churnDataset[sapply(churnDataset,is.numeric)]
drops <-c("WeekHours", "NumberProjectsLastYear","RoleSatisfaction",
          "Dependents","isSingle" ,"isTogether","isDivorced" ,
          "DistanceHomeOffice", "JobDedication",
          "FacilitiesSatisfaction","HierarchySatisfaction" )
numeric_df <-numeric_df[ , !(names(numeric_df) %in% drops)]
corr <- round(cor(numeric_df), 1)


# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)






library(ggplot2)
theme_set(theme_bw())  

# Data Prep
data("mtcars")  # load data
mtcars$`car name` <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.


churnDataset$mean <-round((churnDataset$MonthlyIncome - 
                             mean(churnDataset$MonthlyIncome))/sd(churnDataset$MonthlyIncome), 2)
churnDataset$type <- ifelse(churnDataset$mean < 0, "below", "above")
mtcars <- churnDataset[order(churnDataset$mean), ]
# Diverging Barcharts
ggplot(mtcars, aes(x=JobRole, y=mean, label=mean)) + 
  geom_bar(stat='identity', aes(fill=type), width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised salary per xxxx'", 
       title= "Diverging Bars") + 
  coord_flip()



summary(JobLevel)
ggplot(data=churnDataset)+geom_histogram(aes(x=JobLevel),
                                         color="blue")+
  scale_x_continuous(name="Level", breaks=seq(0,5,1))+ 
  scale_y_continuous(name="Number of Employees", breaks=seq(0,600,100))+
  ggtitle(label="Job Levels")+ theme_bw()

ggplot(data=churnDataset)+geom_point(aes(x=JobLevel, 
                                         y=TenureCompany, 
                                         color=as.factor(Gender),
                                         shape=as.factor(Churn)),
                                     size=2,
                                     alpha=0.8)+
  scale_x_continuous(name="Level", breaks=seq(0,5,1))+
  scale_y_continuous(name="Tenure Company", breaks=seq(0,60,10))+
  ggtitle(label="Job Levels")+ theme_bw()



ggplot(data=churnDataset)+geom_bar(aes(x=as.character(AfterHours),fill=as.factor(Gender)))+
  ggtitle(label="Job Levels")+ theme_bw()


ggplot(data=churnDataset)+geom_bar(aes(x=as.character(AfterHours),fill=as.factor(AfterHours)))+
  ggtitle(label="Job Levels")+ theme_bw() + facet_wrap(~Gender)

ggplot(data=churnDataset)+geom_boxplot(aes(x="JobLevel",y=JobLevel))+
  ggtitle(label="Job Levels")+ theme_bw()

ggplot(data=churnDataset)+geom_boxplot(aes(x=JobType,y=MonthlyIncome))+
  ggtitle(label=" MonthlyIncome")+ theme_bw()  + facet_wrap(~Gender)



plot(Education, NumberProjectsLastYear, main="Projects Last Year per Education",
     xlab="Education", ylab="Projects Last Year", pch=1) 

plot(Dependents, JobLevel, main="Dependents  per JobLevel",
     xlab="Dependents", ylab="JobLevel", pch=19) 


# Add a Normal Curve (Thanks to Peter Dalgaard)

h<-hist(Dependents, breaks=10, col="red", xlab="Job Level",
        main="JobLevel")
xfit<-seq(min(Dependents),max(Dependents),length=30)
yfit<-dnorm(xfit,mean=mean(Dependents),sd=sd(Dependents))
yfit <- yfit*diff(h$mids[1:2])*length(Dependents)
lines(xfit, yfit, col="blue", lwd=2) 


ggplot(data = churnDataset) +
  geom_boxplot(mapping = aes(x = Gender, y = Dependents))





# Data cleaning -----------------------------------------------------------
#data cleaning

# dataset_clean<-dataset[!duplicated(dataset),]
# dataset_transpose<-dataset_clean[duplicated(t(dataset_clean)),]
# 
# 
# #converting dates
# dataset_clean$BirthDate
# library(lubridate)
# dataset_clean$BirthDate<-dmy(dataset_clean$BirthDate)
# dataset_clean$BirthDate
# 
# class(dataset_clean$BirthDate)
# 
# #sex
# levels(dataset_clean$Sex)
# library(plyr)
# count(dataset_clean, "Sex");
# levels(dataset_clean$Sex)<-tolower(levels(dataset_clean$Sex));levels(dataset_clean$Sex)
# levels(dataset_clean$Sex)[levels(dataset_clean$Sex)=='f']<-"female"
# levels(dataset_clean$Sex)[levels(dataset_clean$Sex)=='m']<-"male";levels(dataset_clean$Sex)
# 

# Feature engineering -----------------------------------------------------



# Feature Selection -------------------------------------------------------






# Data Partition ----------------------------------------------------------




