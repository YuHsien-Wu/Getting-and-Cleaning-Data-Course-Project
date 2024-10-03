# You should create one R script called run_analysis.R that does the following. 

library(dplyr)

#Downloading all Data Sets

filename <- "Coursera_Week4_FinalAssignment.zip"

# Checking if the file already exists.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

# Merges the training and the test sets to create one data set.

# 1-1 read all tables and assign for each variables
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

features <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")

# 1-2 Name the columns by features file and 
colnames(activityLabels) <- c("activityID", "activityType")
colnames(x_train) <- features[, 2]
colnames(y_train) <- "activityID"
colnames(subject_train) <- "subjectID"

colnames(x_test) <- features[, 2]
colnames(y_test) <- "activityID"
colnames(subject_test) <- "subjectID"

# 1-3 Merge all data into one dataset
all_test <- cbind(y_test, subject_test, x_test)
all_train <- cbind(y_train, subject_train, x_train)
finalset <- rbind(all_train, all_test)

# Extracts only the measurements on the mean and standard deviation for each 
# measurement. 
mean_std_data <- grepl("activityID|subjectID|mean\\(\\)|std\\(\\)", colnames(finalset))
combineMeanStd <- finalset[, mean_std_data]

# Uses descriptive activity names to name the activities in the data set
withActivityName <- merge(combineMeanStd, activityLabels, by = "activityID", all.x = T)

# Appropriately labels the data set with descriptive variable names. 
colnames(withActivityName) <- gsub("^t", "Time_", colnames(withActivityName))
colnames(withActivityName) <- gsub("^f", "Frequency_", colnames(withActivityName))
colnames(withActivityName) <- gsub("Acc", "Accelerometer", colnames(withActivityName))
colnames(withActivityName) <- gsub("Gyro", "Gyroscope", colnames(withActivityName))
colnames(withActivityName) <- gsub("-mean\\(\\)", "_Mean", colnames(withActivityName))
colnames(withActivityName) <- gsub("-std\\(\\)", "_StandardDeviation", colnames(withActivityName))
colnames(withActivityName) <- gsub("-", "_", colnames(withActivityName))

# From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.
tidy_avg_set <- withActivityName |>
  group_by(activityID, subjectID, activityType) |> 
  summarise_all(mean)

write.table(tidy_avg_set, "tidy_avg_set.txt", row.names = FALSE)
