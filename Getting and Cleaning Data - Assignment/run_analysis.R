##############################################################################
#
# run_analysis.R
#
# This program get, merge, clean the dataset provided
# i.e UCI HAR Dataset
#
##############################################################################
#
# Download the dataset using the link.
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# A file named "UCI HAR Dataset.zip" will get downloaded.
# Unzip(decompress) it.
# A Folder named "UCI HAR Dataset" is created which contains the dataset.
#
##############################################################################

library(dplyr)

trainingSubjects <- read.table(file.path("UCI HAR Dataset", "train", "subject_train.txt"))
trainingValues <- read.table(file.path("UCI HAR Dataset", "train", "X_train.txt"))
trainingActivity <- read.table(file.path("UCI HAR Dataset", "train", "y_train.txt"))
testSubjects <- read.table(file.path("UCI HAR Dataset", "test", "subject_test.txt"))
testValues <- read.table(file.path("UCI HAR Dataset", "test", "X_test.txt"))
testActivity <- read.table(file.path("UCI HAR Dataset", "test", "y_test.txt"))
features <- read.table(file.path("UCI HAR Dataset", "features.txt"), as.is = TRUE)
activities <- read.table(file.path("UCI HAR Dataset", "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")


##### STEP 1 ################################################################# 
#####     Merges the training and the test sets to create one data set   #####
##############################################################################

humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)
rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)
colnames(humanActivity) <- c("subject", features[, 2], "activity")

##### STEP 2 ################################################################# 
#####     Extracts only the measurements on the mean                     #####
#####     and standard deviation for each measurement.                   #####
##############################################################################

columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsToKeep]

##### STEP 3 ################################################################# 
#####     Uses descriptive activity names to name                        #####
#####     the activities in the data set                                 #####
##############################################################################

humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

##### STEP 4 ################################################################# 
#####     Appropriately labels the data set with descriptive             #####
#####     variable names                                                 #####
##############################################################################

humanActivityCols <- colnames(humanActivity)
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)
colnames(humanActivity) <- humanActivityCols

##### STEP 5 ################################################################# 
#####     From the data set in step 4, creates a second,                 #####
#####     independent tidy data set with the average of                  #####
#####     each variable for each activity and each subject.              #####
#####     variable names                                                 #####
##############################################################################

humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)

