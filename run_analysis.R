# This R script called run_analysis.R does the following:
# 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Load needed packages.
library(data.table)
library(dplyr)

# Load files into R.
X_test <- fread("./UCI HAR Dataset/test/X_test.txt")
y_test <- fread("./UCI HAR Dataset/test/y_test.txt")
subject_test <- fread("./UCI HAR Dataset/test/subject_test.txt")
X_train <- fread("./UCI HAR Dataset/train/X_train.txt")
y_train <- fread("./UCI HAR Dataset/train/y_train.txt")
subject_train <- fread("./UCI HAR Dataset/train/subject_train.txt")
features <- fread("./UCI HAR Dataset/features.txt")
activity_labels <- fread("./UCI HAR Dataset/activity_labels.txt")

# 1. Merges the training and the test sets to create one data set.
X_join <- bind_rows(X_test, X_train)
y_join <- bind_rows(y_test, y_train)
subject_join <- bind_rows(subject_test, subject_train)
join <- bind_cols(X_join, subject_join, y_join)
colnames(join) <- c(features$V2, "Subject", "Activity")

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
cn <- colnames(join)
ms <- c(grep("[Mm]ean", cn), grep("std", cn), 562, 563)
meanstd <- join[, ..ms]

# 3. Uses descriptive activity names to name the activities in the data set
meanstd$activity <- factor(meanstd$activity, labels = activity_labels$V2)

# 4. Appropriately labels the data set with descriptive variable names.
names(meanstd) <- gsub("^f", "Frequency domain signal ", names(meanstd))
names(meanstd) <- gsub("^t", "Time domain signal ", names(meanstd))
names(meanstd) <- gsub("^angle", "Angle between the vector ", names(meanstd))
names(meanstd) <- gsub("-X$", " in the X direction", names(meanstd))
names(meanstd) <- gsub("-Y$", " in the Y direction", names(meanstd))
names(meanstd) <- gsub("-Z$", " in the Z direction", names(meanstd))
names(meanstd) <- gsub("-mean()", " mean function", names(meanstd))
names(meanstd) <- gsub("-std()", " standard deviation function", names(meanstd))
names(meanstd) <- gsub("-meanFreq()", " mean frequency function", names(meanstd))

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
average_meanstd <- meanstd %>% group_by(Subject, Activity) %>% summarise_all(mean)