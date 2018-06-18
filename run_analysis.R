library(data.table)
library(dplyr)
#clean Global environment and set wd
rm(list=ls())
setwd("/Users/ppellegrini/Desktop/Coursera/GettingCleaningData/final assignment")

#Download and Unzip files

url= "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "UCI HAR Dataset.zip")
unzip("UCI HAR Dataset.zip")
setwd("UCI HAR Dataset")

#Import files

features<- read.table("features.txt", header = FALSE)
activity_labels <- read.table("activity_labels.txt", header = FALSE )
subject_train <- read.table("train/subject_train.txt",header = FALSE)
X_train <- read.table("train/X_train.txt",header = FALSE)
y_train <- read.table("train/y_train.txt",header = FALSE)
subject_test <- read.table("test/subject_test.txt",header = FALSE)
X_test <- read.table("test/X_test.txt",header = FALSE)
y_test <- read.table("test/y_test.txt",header = FALSE)

####1. Merges the training and the test sets to create one data set.####

X.train.and.test <- rbind(X_train, X_test)
y.train.and.test <- rbind(y_train, y_test)
subject.train.and.test <- rbind(subject_train, subject_test)


#2. Extracts only the measurements on the mean and standard deviation 
#   for each measurement.

# Seach for those columns that include the 'mean' and 'std' in the name

mean_sd_cols <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X.measurements <- X.train.and.test[,mean_sd_cols]

#3. Uses descriptive activity names to name the activities in the 
# data set

activity_final <- merge(activity_labels, y.train.and.test, by = "V1")

#4. Appropriately labels the data set with descriptive variable names.


#name the column of subject and activity
names(subject.train.and.test)[1]<-"subject"
names(activity_final)[2]<-"activity"

#add feature names
X.measurements.names <- features[mean_sd_cols,]
merge(X.measurements,X.measurements.names, by = "V2")
names(X.measurements) <- X.measurements.names$V2

#merge all labeled dfs
dataset.final <- bind_cols(activity_final, subject.train.and.test, 
                           X.measurements)


#5. From the data set in step 4, creates a second, independent tidy 
#   data set with the average of each variable for each activity and 
#   each subject.

datase.final.tidy <- mutate(dataset.final, subject = as.factor(subject)) %>% 
  group_by(activity, subject) %>% 
  summarize_all(funs(mean)) %>% 
  ungroup
