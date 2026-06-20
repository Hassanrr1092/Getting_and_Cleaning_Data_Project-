getwd()
setwd("./GettingAndCleaningData/Project/WorkingFolderProj")
getwd()

URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(URL, destfile = "Dataset.zip" )
unzip("Dataset.zip")

list.files("../UCI HAR Dataset")

if(!(file.exists("./WorkingFolderProj"))){
  dir.create("./WorkingFolderProj")
}

setwd("./WorkingFolderProj")
list.files()

getwd()
setwd("./GettingAndCleaningData/Project/WorkingFolderProj")
getwd()

# Task 1. Merges the training and the test sets to create one data set.

install.packages("data.table")
library(data.table)
library(tibble)

list.files("../UCI HAR Dataset/test")

#Part a. Read subject_test.txt
# read subjects who performs the activities
subjects <- fread("../UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
as_tibble(subjects)

dim(subjects)
str(subjects)

sum(is.na(subjects))
sum(!is.na(subjects))

#Part b. Reading X_test.txt

test_set <- fread("../UCI HAR Dataset/test/X_test.txt")
as_tibble(test_set)

dim(test_set)
str(test_set)

#Part c. Reading Y_test.txt

test_label <- fread("../UCI HAR Dataset/test/y_test.txt")
as_tibble(test_label)
dim(test_label)

colnames(test_label)[1] <- "test_label"



as_tibble(cbind(subjects, test_label, test_set))

# combined the three data (subject_test, X_test, Y_test) 
Merged_SubLabelSet <- as_tibble(cbind(subjects, test_label, test_set))
Merged_SubLabelSet

# Task 1 is competed; 

#-----------
#Task 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- fread("../UCI HAR Dataset/features.txt", col.names = c("index", "feature"))
as_tibble(features)

# Extract only mean and standard deviation measurements

mean_stdMeasurementIndice <- grep("mean(?=\\(\\))|std(?=\\(\\))", features[[2]], perl = TRUE)
mean_stdMeasurementValue <- grep("mean(?=\\(\\))|std(?=\\(\\))", features[[2]], value = TRUE, perl = TRUE)
mean_stdMeasurementIndice
mean_stdMeasurementValue

length(mean_stdMeasurementIndice)
length(mean_stdMeasurementValue)

# Task 3. Appropriately labels the data set with descriptive variable names. 
as_tibble(Merged_SubLabelSet)

Merged_SubLabelSet0 <- Merged_SubLabelSet

colnames(Merged_SubLabelSet0)[(mean_stdMeasurementIndice + 2)] <- mean_stdMeasurementValue
colnames(Merged_SubLabelSet0)

Merged_SubLabelSet1 <- Merged_SubLabelSet0
Merged_SubLabelSet1

Merged_SubLabelSet1 <- Merged_SubLabelSet1[c("subject", "test_label", mean_stdMeasurementValue)]

colnames(Merged_SubLabelSet1)
Merged_SubLabelSet1

install.packages("dplyr")
library(dplyr)

detach("package:rlang", unload = TRUE)
install.packages("rlang")
library(rlang)

# task 4. Uses descriptive activity names to name the activities in the data set
Merged_SubLabelSet2 <- Merged_SubLabelSet1
Merged_SubLabelSet2[[2]] <- factor(Merged_SubLabelSet2[[2]], 
                                  levels = c(1, 2, 3, 4, 5, 6), 
                                  labels = c("WALKING",
                                             "WALKING_UPSTAIRS",
                                             "WALKING_DOWNSTAIRS",
                                             "SITTING",
                                             "STANDING",
                                             "LAYING"))                                                                                            
Merged_SubLabelSet2

# Tasl 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
Merged_SubLabelSet2 %>% group_by(subject, test_label) %>%
                         summarize(across(1:66, mean, na.rm=TRUE), .groups = "drop_last")

Merged_SubLabelSet2 %>% group_by(subject, test_label) %>%
                         summarize(across(1:66, \(x) mean(x, na.rm=TRUE)), .groups = "drop_last")


#===================================
# Training dataset 
getwd()

# Task 1. Merges the training and the test sets to create one data set.
# Part a. reading subject_train data which contains subjects who perform the activities
subject_train <- fread("../UCI HAR Dataset/train/subject_train.txt", col.names = "subject" )  
as_tibble(Subject_train)

# Part b . when reading the y_train.txt, I named an only column in it. 
train_label <- fread("../UCI HAR Dataset/train/y_train.txt", col.names = "train_label")
as_tibble(train_label)

# Part c. read x_trains dataset which contains values for each feature
train_set <- fread("../UCI HAR Dataset/train/X_train.txt")
as_tibble(train_set)

             #filtered the columns based on the desired features (mean and std)
train_set <- train_set[, mean_stdMeasurementIndice, with = FALSE]
             # renamed columns data set with feature
colnames(train_set) <- mean_stdMeasurementValue
as_tibble(train_set)


MergedTr_SubLabelSet <- cbind(subject_train, train_label, train_set)
as_tibble(MergedTr_SubLabelSet)

MergedTr_SubLabelSet0 <- MergedTr_SubLabelSet

MergedTr_SubLabelSet0[[2]] <- factor(MergedTr_SubLabelSet0[[2]], 
                                        levels = c(1, 2, 3, 4, 5, 6),
                                        labels = c("WALKING",
                                                   "WALKING_UPSTAIRS",
                                                   "WALKING_DOWNSTAIRS",
                                                   "SITTING",
                                                   "STANDING",
                                                   "LAYING"))

as_tibble(MergedTr_SubLabelSet0)
#=================
as_tibble(Merged_SubLabelSet2)
as_tibble(MergedTr_SubLabelSet0)    

colnames(Merged_SubLabelSet2)[-c(1, 2)] <- gsub("\\(\\)", "", colnames(Merged_SubLabelSet2)[-c(1, 2)])
colnames(MergedTr_SubLabelSet0)[-c(1, 2)] <- gsub("\\(\\)", "", colnames(MergedTr_SubLabelSet0)[-c(1, 2)])

colnames(Merged_SubLabelSet2)[2] <- "activities"
colnames(MergedTr_SubLabelSet0)[2] <- "activities"

merged_TrainTest <- rbind(MergedTr_SubLabelSet0, Merged_SubLabelSet2)

rbind(as.data.frame(MergedTr_SubLabelSet0), as.data.frame(Merged_SubLabelSet2))

merged_TrainTest

as_tibble(merged_TrainTest)

# Task 5. From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.
AveSubActivity <-merged_TrainTest %>% group_by(subject, activities) %>%
                      summarise(across(1:66, mean, na.rm = TRUE), .groups = "drop_last" )
AveSubActivity


write.table(AveSubActivity, "tidy_data.txt", row.names = FALSE)
write.csv(AveSubActivity, "tidy_data.csv", row.names = FALSE)




