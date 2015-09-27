# file: run_analysis.R
# Merge data and alter with improved column names and descriptive activities; finally make data tidy and write to file.

# required libraries
library( data.table )
library( dplyr )

# grab data for column names
featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

# get the 3 parts of training (activity, features, subject)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)

# get the 3 parts of test data (activity, features, subject)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)



# 1. Merges the training and the test sets to create one data set.
activity <- rbind(activityTrain, activityTest)
subject <- rbind(subjectTrain, subjectTest)
features <- rbind(featuresTrain, featuresTest)

# rename columns; t returns the transpose of a dataframe
colnames( features ) <- t( featureNames[2] )

# Add columns: activity and subject
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
origData <- cbind(features,activity,subject)



# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
colNums <- grep(".*mean.*|.*std.*|activity|subject", names(origData), ignore.case=TRUE)
alteredData <- origData[,colNums]



# 3. Uses descriptive activity names to name the activities in the data set
# (the activities are just numbers 1-6 --- replace with labels)
alteredData$Activity <- as.character(alteredData$Activity)
for(i in 1:6){
  alteredData$Activity[alteredData$Activity == i] <- as.character(activityLabels[i, 2])
}
# coerce activity as a factor
alteredData$Activity <- as.factor(alteredData$Activity)



# 4 Appropriately labels the data set with descriptive variable names. 
names( alteredData ) <- gsub("^f", "Frequency", names(alteredData))
names( alteredData ) <- gsub("^t", "Time", names(alteredData))
names( alteredData ) <- gsub("-freq()", "Frequency", names(alteredData), ignore.case = TRUE)
names( alteredData ) <- gsub("-mean()", "Mean", names(alteredData), ignore.case = TRUE)
names( alteredData ) <- gsub("-std()", "STD", names(alteredData), ignore.case = TRUE)
names( alteredData ) <- gsub("Acc", "Accelerometer", names(alteredData))
names( alteredData ) <- gsub("angle", "Angle", names(alteredData))
names( alteredData ) <- gsub("BodyBody", "Body", names(alteredData))
names( alteredData ) <- gsub("gravity", "Gravity", names(alteredData))
names( alteredData ) <- gsub("Gyro", "Gyroscope", names(alteredData))
names( alteredData ) <- gsub("Mag", "Magnitude", names(alteredData))
names( alteredData ) <- gsub("tBody", "TimeBody", names(alteredData))

alteredData$Subject <- as.factor(alteredData$Subject)
alteredData <- data.table(alteredData)



# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidydataset <- aggregate(. ~Subject + Activity, alteredData, mean)

# order by subject then activity
tidydataset <- tidydataset[order(tidydataset$Subject,tidydataset$Activity),]

# write to file; to read use: read.table("tidydataset.txt", header = T)
write.table(tidydataset, file = "tidydataset.txt", row.names = FALSE)
