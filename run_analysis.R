## Assignment 1 Getting and Cleaning Data
## You should create one R script called run_analysis.R that does the following. 

#loadpackages
library(plyr)
library(dplyr)
library(reshape2)
library(stringr)

#establish data file paths within working directory containing UCI HAR dataset
#test dataset file paths
dsxtest <- "UCI HAR Dataset/test/X_test.txt"
dsytest <- "UCI HAR Dataset/test/y_test.txt"
dssubject_test <- "UCI HAR Dataset/test/subject_test.txt"
#training dataset file paths
dsxtrain <- "UCI HAR Dataset/train/X_train.txt"
dsytrain <- "UCI HAR Dataset/train/y_train.txt"
dssubject_train <- "UCI HAR Dataset/train/subject_train.txt"
#other file paths
dsfeatures <- "UCI HAR Dataset/features.txt" #2nd column contains 561 variable names
dsactivity_labels <- "UCI HAR Dataset/activity_labels.txt" #labels for activities 1-6 in column 2

#note X datasets have 561 variables corresponding to varialbe names outlined in 'features.txt'
features <- as.vector(read.table(dsfeatures)[,2])

#load data individual data sets 
X_test <- read.table(dsxtest,col.names = features)
y_test <- read.table(dsytest, col.names ="activity")
subject_test <- read.table(dssubject_test,col.names = "subject")
X_train <- read.table(dsxtrain, col.names = features)
y_train <- read.table(dsytrain,col.names ="activity")
subject_train <- read.table(dssubject_train,col.names = "subject")

# 1.Merges the training and the test sets to create one data set.
#merged test/training datasets 
merged_X <- rbind(X_test,X_train)
merged_y <- rbind(y_test,y_train)
merged_subject <- rbind(subject_test,subject_train)
#bind into a single dataset 
c1 <- cbind(merged_y,merged_X)
merged_data <- cbind(merged_subject,c1)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
#subset of data using select in dplyr package 
subdata <- select(merged_data,subject,activity, contains(".mean."), contains(".std."))
#nb 68 variables in dataset

#3.Uses descriptive activity names to name the activities in the data set
#load activity labels from 'activity_labels.txt'
activity_labels <- as.vector(read.table(dsactivity_labels)[,2])
#replace elements 1:6 with activity labels using mapvalues in plyr package
subdata$activity <- mapvalues(subdata$activity, from = c(1:6), to = activity_labels)
#4.Appropriately labels the data set with descriptive variable names. 
#Data is already labelled but for the final tidy dataset the definition changes.
#see codebook.m

# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#convert subject and activity to factors as these are the basis of the new tidy dataset 
subdata$subject <- as.factor(subdata$subject)
subdata$activity <- as.factor(subdata$activity)
#split data set, apply mean() then recombine data. 
splitdata <- split(select(subdata, 3:68), list(subdata$subject, subdata$activity))
meandata <- lapply(splitdata, function(x) apply(x,2,mean,na.rm=TRUE))
tidydata <- data.frame(t(sapply(meandata,c)))
#uncombine activity and subject and reinstate as seperate columns 
factors <- data.frame(t(sapply(strsplit(rownames(tidydata), "[.]"),c)))
tidydata <- cbind(factors, tidydata)
tidydata <- dplyr::rename(tidydata,subject = X1, activity = X2)
tidydata$subject <- as.factor(tidydata$TestSubject)
tidydata$activity <- as.factor(tidydata$activity)
rownames(tidydata) <- NULL
#write data to new table 
write.table(tidydata,"tidy_data.txt",row.names = FALSE)
