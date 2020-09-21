setwd("C:\\Users\\98269\\Desktop\\R\\Data Science Specialization\\3 Getting and Cleaning Data\\project")
library(dplyr)

# 1. Merges the training and the test sets to create one data set
train_data  <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_label <- read.table("./UCI HAR Dataset/train/y_train.txt")
train_subj  <- read.table("./UCI HAR Dataset/train/subject_train.txt")

test_data   <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_label  <- read.table("./UCI HAR Dataset/test/y_test.txt")
test_subj   <- read.table("./UCI HAR Dataset/test/subject_test.txt")

X <- rbind(train_data, test_data)
y <- rbind(train_label, test_label)

subj <- rbind(train_subj, test_subj)
names(subj) <- c("Subject")


# 2. Extracts only the measurements on the mean and standard deviation for each measurement
features  <- read.table("./UCI HAR Dataset/features.txt")
stats <- grep("-mean\\(\\)|-std\\(\\)", features[, 2]) 

X <- X[, stats]
names(X) <- features[stats, 2]


# 3. Uses descriptive activity names to name the activities in the data set
acti_labe <- read.table("./UCI HAR Dataset/activity_labels.txt")
y[, 1] <- acti_labe[y[, 1], 2]
names(y) <- c("Activity")


# 4. Appropriately labels the data set with descriptive variable names
data <- cbind(subj, y, X)


# 5. From the data set in step 4, creates a second, independent tidy data
# set with the average of each variable for each activity and each subject
data_mean <- data %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))

write.table(data_mean, "TidyData.txt", row.name=FALSE)
