library(dplyr)

# PROJECT - 03 GETTING AND CLEANING DATA


# Setting Working Directory

setwd('C:\\R\\Coursera\\3_GettingAndCleaningData\\FinalProject')


if(!dir.exists("UCI HAR Dataset")) {
    stop("Download and unpack project data!")
}



################## Reading and preparing the data

features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
features <- features$V2

alabels <- read.table("UCI HAR Dataset/activity_labels.txt")
names(alabels) <- c("id", "Activity")

# Read test-data
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt")
test_x <- read.table("UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("UCI HAR Dataset/test/y_test.txt")
names(test_y) <- "id"

# Read train-data
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt")
train_x <- read.table("UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("UCI HAR Dataset/train/y_train.txt")
names(train_y) <- "id"


################# [3] Uses descriptive activity names to name the activities in the data set
z1 <- join(test_y, alabels, by="id", type="left")
z2 <- join(train_y, alabels, by="id", type="left")




################## [1] Merges the training and the test sets to create one data set.

d1 <- cbind(test_subject, z1, test_x)
d2 <- cbind(train_subject, z2, train_x)

# Collecting to one dataset
dataset <- rbind(d1, d2)


################# [4] Appropriately labels the data set with descriptive variable names
names(dataset) <- c("Subject", "acid", "Activity", features)


################# [2] Extracts only the measurements on the mean and standard deviation
#################     for each measurement.

# Identify which columns contain mean og std
ind1 <- grep("mean()", features, fixed="TRUE")
ind2 <- grep("std()", features, fixed="TRUE")


# Include -2, -1, 0 and then add 3 to all indices. Necessary since we added
# the columns "Users", "acid" and "Activity". 
ind3 <- c(-2, -1, 0, unique(sort(c(ind1, ind2))))+3

dataset <- dataset[,ind3]

# Remove Activity ID.
dataset <- select(dataset, -acid)

# No missing values. The following returns TRUE:
# all(colSums(is.na(dataset))==0)



################# [5] From the data set in step 4, creates a second, independent
#################     tidy data set with the average of each variable for each activity
#################     and each subject.

n = length(dataset)

# Calculating the average/mean of all numeric values.
datamean <- aggregate(cbind(dataset[,3:n]),
                      by=list(dataset$Subject, dataset$Activity),
                      FUN = mean)


# Creating the second table
names(datamean) <- names(dataset)
datamean <- arrange(datamean, Subject, Activity)

# Store as txt
write.table(datamean, "datamean.txt", quote=FALSE, row.names = FALSE, sep=",")
