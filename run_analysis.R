#Load library
library(dplyr)

#########################################################################################################
# 0. Download dataset
#########################################################################################################

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
filename <- "UCI HAR Dataset.zip"

# download zip file containing data
if (!file.exists(filename)) {
  download.file(fileURL, filename, mode = "wb")
}

# unzip zip file containing data 
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

#########################################################################################################
# 0. Read data and assign data frames
#########################################################################################################

# read test data
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
testValues <- read.table("UCI HAR Dataset/test/X_test.txt")
testActivity <- read.table("UCI HAR Dataset/test/y_test.txt")

# read training data
trainingSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
trainingValues <- read.table("UCI HAR Dataset/train/X_train.txt")
trainingActivity <- read.table("UCI HAR Dataset/train/y_train.txt")

# read features
features <- read.table("UCI HAR Dataset/features.txt", as.is = TRUE)

# read activity lables
activities <- read.table("UCI HAR Dataset/activity_labels.txt")
colnames(activities) <- c("activityId", "activityLabel")

#########################################################################################################
# 1: Merges the training and the test sets to create one data set.
#########################################################################################################

Merged_Data <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity))

# assign column names
colnames(Merged_Data) <- c("subject", features[, 2], "activity")

#########################################################################################################
# 2: Extracts only the measurements on the mean and standard deviation for each measurement.
#########################################################################################################

# determine columns of data set to keep based on column name
columnsToKeep <- grepl("subject|activity|mean|std", colnames(Merged_Data))

# and keep data in these columns only
Merged_Data <- Merged_Data[, columnsToKeep]

#########################################################################################################
## 3: Uses descriptive activity names to name the activities in the data set
#########################################################################################################

Merged_Data$activity <- factor(Merged_Data$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

#########################################################################################################
# 4: Appropriately labels the data set with descriptive variable names.
#########################################################################################################

# get column names
DataCols <- colnames(Merged_Data)
DataCols <- gsub("[\\(\\)-]", "", DataCols)

# expand abbreviations and clean up names                      
DataCols < -gsub("^f", "Frequency", DataCols)
DataCols <- gsub("^t", "Time", DataCols)
DataCols <- gsub("Acc", "Accelerometer", DataCols)
DataCols <- gsub("Gyro", "Gyroscope", DataCols)
DataCols <- gsub("Mag", "Magnitude", DataCols)
DataCols <- gsub("Freq", "Frequency", DataCols)
DataCols <- gsub("mean", "Mean", DataCols)
DataCols <- gsub("std", "STD", DataCols)
DataCols <- gsub("BodyBody", "Body", DataCols)

# use new labels as column names
colnames(Merged_Data) <- DataCols

#########################################################################################################
# 5: From the data set in step 4, creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject.
#########################################################################################################

Merged_DataMeans <- Merged_Data %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "TidyData.txt"
write.table(Merged_DataMeans, "TidyData.txt", row.names = FALSE, 
            quote = FALSE)
