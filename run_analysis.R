## Getting and Cleaning Data Course Project

# Load packages
library(dplyr)

# download dataset from web and read dataset in R

FileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(FileUrl, destfile = "wearable.zip")
unzip("wearable.zip", exdir = "C:/Users/smacvane/OneDrive/R/Coursera/datasciencecoursera/getting_and_cleaning_data_course_project")
HARfiles <- file.path("C:/Users/smacvane/OneDrive/R/Coursera/datasciencecoursera/getting_and_cleaning_data_course_project" , "UCI HAR Dataset")
files <-list.files(HARfiles, recursive=TRUE)

#1 Merge the training and the test sets to create one data set

# features and activities labels

features <- read.table(file.path(HARfiles, "features.txt"), header = FALSE, col.names = c("n", "functions"))
activities <- read.table(file.path(HARfiles, "activity_labels.txt"), header = FALSE, col.names = c("code", "activity"))

# train data

x_train <- read.table(file.path(HARfiles, "train", "X_train.txt"),header = FALSE, col.names = features$functions)
y_train <- read.table(file.path(HARfiles, "train", "y_train.txt"), header = FALSE, col.names = "code")
subject_train <- read.table(file.path(HARfiles, "train", "subject_train.txt"), header = FALSE, col.names = "subject")

# test data

x_test <- read.table(file.path(HARfiles, "test", "X_test.txt"),header = FALSE, col.names = features$functions)
y_test <- read.table(file.path(HARfiles, "test", "y_test.txt"), header = FALSE, col.names = "code")
subject_test <- read.table(file.path(HARfiles, "test", "subject_test.txt"), header = FALSE, col.names = "subject")

# merge training and test data into one data set

merged_x <- rbind(x_train, x_test)
merged_y <- rbind(y_train, y_test)
merged_subject <- rbind(subject_train, subject_test)
merged_data <- cbind(merged_subject, merged_y, merged_x)

# 2 extract only the measurements on the mean and standard deviation for each measurement. 

# select names of features by measurements on the mean and standard deviation

Selected_Col <- features$functions[grep("mean\\(\\)|std\\(\\)", features$functions)]
Selected_ColNames <- c("subject", "activity", as.character(Selected_Col))

# 3 use descriptive activity names to name the activities in the data set
names(merged_data)[2] = "activity"
merged_data$activity <- activities[merged_data$activity, 2]

# 4 appropriately label the data set with descriptive variable names 

Selected_ColNames <- gsub("mean", "Mean", Selected_ColNames)
Selected_ColNames <- gsub("std", "Std", Selected_ColNames)
Selected_ColNames <- gsub("Mag", "Magnitude", Selected_ColNames, ignore.case = TRUE)
Selected_ColNames <- gsub("[-()]", "", Selected_ColNames, ignore.case = TRUE)
Selected_ColNames <- gsub("^t", "Time", Selected_ColNames)
Selected_ColNames <- gsub("^f", "Frequency", Selected_ColNames, ignore.case = TRUE)
Selected_ColNames <- gsub("Acc", "Accelerometer", Selected_ColNames)
Selected_ColNames <- gsub("Gyro", "Gyroscope", Selected_ColNames)
Selected_ColNames <- gsub("BodyBody", "Body", Selected_ColNames)


# Apply new names to data set

merged_data2 <- merged_data
names(merged_data2) <- Selected_ColNames

# Subset data for only mean and std columns

Tidy_data <- merged_data2[Selected_ColNames]

# 5 creates a second, independent tidy data set with the average of each variable for each activity and each subject

Tidy_data_Ind <- Tidy_data %>% group_by(subject, activity) %>% summarize_all(funs(mean))
write.table(Tidy_data_Ind, file = "tidydata.txt",row.name=FALSE)
data_summary <- read.table("tidydata.txt", header = TRUE)
View(data_summary)

# Produce Codebook

library(knitr)
knit2html("Coursera_Getting and Cleaning Data_CourseProject_V2.R")
