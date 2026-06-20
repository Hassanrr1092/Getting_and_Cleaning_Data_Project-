# Getting and Cleaning Data Project - John Hupkins University

# The goal is to repare tidy data that can be used for later analysis.

# The following Tasks to be done:
  ## 1. Merges the training and the test sets to create one data set.
  ## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
  ## 3. Uses descriptive activity names to name the activities in the data set
  ## 4. Appropriately labels the data set with descriptive variable names. 
  ## 5. From the data set in step 4, creates a second, independent tidy data set 
      # with the average of each variable for each activity and each subject.

# download the data for the project:

URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(URL, destfile = "Dataset.zip" )
unzip("Dataset.zip")

install.packages("data.table")
library(data.table)
library(tibble)

# I went on to apply the first 4 task on each test and train data
# and then combined them, Following that, lastly applied the 5th task on the combined data frame. 

# Task 1. Merges the training and the test sets to create one data set.

# Part a. Merged the train data (subject_test, X_test, Y_test)

            # read subject_test.txt that contains subject who perform the activities 
subjects <- fread("../UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
as_tibble(subjects)

            # read test set (X_text.txt) that contains values for the features 
test_set <- fread("../UCI HAR Dataset/test/X_test.txt")
as_tibble(test_set)

            # read test label (y_test.txt) that contains activity labels
test_label <- fread("../UCI HAR Dataset/test/y_test.txt", col.names = "activity")
as_tibble(test_label)

            # combined the train data (subject_test, X_test, Y_test) 
Merged_Test <- as_tibble(cbind(subjects, test_label, test_set))
Merged_Test

# Part b. Merged the train data (subject_train, X_train, Y_train)

            # read subject_test.txt that contains subject who perform the activities 
train_subjects <- fread("../UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
as_tibble(train_subjects)

            # read test set (X_text.txt) that contains values for the features 
train_set <- fread("../UCI HAR Dataset/train/X_train.txt")
as_tibble(train_set)

            # read test label (y_test.txt) that contains activity labels
train_label <- fread("../UCI HAR Dataset/train/y_train.txt", col.names = "activity")
as_tibble(train_label)

            # combined the test data (subject_test, X_test, Y_test) 
Merged_Train <- as_tibble(cbind(train_subjects, train_label, train_set))
Merged_Train

# Merged train and test sets

Merged_TrainTest <- rbind(Merged_Train, Merged_Test)
Merged_TrainTest

#Task 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
            # read features data (features.txt) and gave relavant names for the columns 
features <- fread("../UCI HAR Dataset/features.txt", col.names = c("index", "feature"))
as_tibble(features) 

            # filtered the feature and Extract only values with mean and standard deviation measurements in it
mean_stdMeasurementIndice <- grep("mean(?=\\(\\))|std(?=\\(\\))", features[[2]], perl = TRUE)
mean_stdMeasurementValue <- grep("mean(?=\\(\\))|std(?=\\(\\))", features[[2]], value = TRUE, perl = TRUE)
mean_stdMeasurementIndice
mean_stdMeasurementValue 


# task 3. Uses descriptive activity names to name the activities in the data set
Merged_TrainTest[[2]] <- factor(Merged_TrainTest[[2]], 
                                   levels = c(1, 2, 3, 4, 5, 6), 
                                   labels = c("WALKING",
                                              "WALKING_UPSTAIRS",
                                              "WALKING_DOWNSTAIRS",
                                              "SITTING",
                                              "STANDING",
                                              "LAYING")) 
Merged_TrainTest

# Task 4. Appropriately labels the data set with descriptive variable names. 
colnames(Merged_TrainTest)[(mean_stdMeasurementIndice + 2)] <- mean_stdMeasurementValue

Merged_TrainTest <- Merged_TrainTest[, c("subject", "activity", mean_stdMeasurementValue)]
colnames(Merged_TrainTest)

colnames(Merged_TrainTest)[-c(1, 2)] <- gsub("\\(\\)", "", colnames(Merged_TrainTest)[-c(1, 2)])
Merged_TrainTest


# Tasl 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

AvgVariable <- Merged_TrainTest %>% 
               group_by(subject, activity) %>%
               summarize(across(1:66, mean, na.rm=TRUE), .groups = "drop_last")
AvgVariable




