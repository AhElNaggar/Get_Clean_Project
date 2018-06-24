#This R script is design to get and clean data
#from the accelerometers from the Samsung Galaxy S smartphone

##download zip file containing dataset
#download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile = "original file.zip")

##unzip dowloaded file
#unzip(zipfile = "original file.zip")

##set working directory

setwd("UCI HAR Dataset")

##Load required files in R
features <- read.table("features.txt", quote="\"", comment.char="")
activity_labels <- read.table("activity_labels.txt", quote="\"", comment.char="")
X_train <- read.table("train/X_train.txt", quote="\"", comment.char="")
y_train <- read.table("train/y_train.txt", quote="\"", comment.char="")
X_test <- read.table("test/X_test.txt", quote="\"", comment.char="")
y_test <- read.table("test/y_test.txt", quote="\"", comment.char="")
subject_train <- read.table("train/subject_train.txt", quote="\"", comment.char="")
subject_test <- read.table("test/subject_test.txt", quote="\"", comment.char="")

##change X_test & X_train column names as per features list
colnames(X_test) <- features$V2
colnames(X_train) <- features$V2

##add activity and subject identifiers to X_test & X_train
Test_T <- cbind(SubjectID = subject_test$V1, ActivityID = y_test$V1, X_test)
Train_T <- cbind(SubjectID = subject_train$V1, ActivityID = y_train$V1, X_train)

##combine Test and Train tables
Complete_T <- rbind(Test_T,Train_T)

##Extract mean and standard deviation for each measurement
list_required <- grep("SubjectID|mean[^Freq]|[Ss]td|ActivityID",colnames(Complete_T),value = TRUE)
mean_std_t <- Complete_T[,list_required]
library(dplyr)

##add descriptive activity names
mean_std_t <- merge(x = activity_labels, y = mean_std_t, by.x = "V1", by.y = "ActivityID", all = TRUE)
mean_std_t <- rename(mean_std_t,"Activity" = V2)

##remove unused IDs
mean_std_t <- mean_std_t[,-(1)]

##create table including avarage variable for each activity and each subject
###create list with all subjects

###create list with all activities
ActivityL <- as.character(activity_labels[[2]])
###create a loop to collect the avarage by each activity and subject
z <- data.frame()
s1 <- character()
for (i in 1:30){
        for (ii in 1:6){
                z <- rbind(z,colMeans(filter(mean_std_t,SubjectID == SubjectL[i], Activity == ActivityL[ii])[,(3:68)]))
                s1 <- c(s1,ActivityL[ii])
        }
}
z <- cbind("SubjectID" = rep(1:30,each = 6), "Activity" = s1, z)
colnames(z) <- colnames(mean_std_t)



