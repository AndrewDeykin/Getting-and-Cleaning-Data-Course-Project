
# Read in the data

setwd("C:/Users/Andrew/Documents/UCI HAR Dataset")
features <- read.table("./features.txt", header = FALSE, stringsAsFactors = FALSE)
activity_labels <- read.table("./activity_labels.txt", header = FALSE, stringsAsFactors = FALSE)

setwd("C:/Users/Andrew/Documents/UCI HAR Dataset/train")
X_train <- read.table("./X_train.txt", header = FALSE)
Y_train <- read.table("./Y_train.txt", header = FALSE)
subject_train <- read.table("./subject_train.txt", header = FALSE)

setwd("C:/Users/Andrew/Documents/UCI HAR Dataset/test")
X_test <- read.table("./X_test.txt", header = FALSE)
Y_test <- read.table("./Y_test.txt", header = FALSE)
subject_test <- read.table("./subject_test.txt", header = FALSE)

# Reset working directory
setwd("C:/Users/Andrew/Documents/UCI HAR Dataset")


####################################################################
# (1) Merge the training and the test sets to create one data set. #
####################################################################

# Assign labels for X_train and X_test data
names(X_train) <- features[,2]
names(X_test) <- features[,2]

# Assign labels to Y_train and Y_test data
names(Y_train) <- "Activity_Index"
names(Y_test) <- "Activity_Index"

# Assign labels to subject_train and subject_test data
names(subject_train) <- "Subject"
names(subject_test) <- "Subject"

# Merge training data
train <- cbind(Y_train, subject_train, X_train)

# Merge test data
test <- cbind(Y_test, subject_test, X_test)

# Merge train and test data
data <- rbind(train, test)


##############################################################################################
# (2) Extract only the measurements on the mean and standard deviation for each measurement. #
##############################################################################################

# Logical vector representing elements of features that contain the mean or standard deviation
features_extracted <- grepl("mean|std", features[,2])

# Adjustment to make sure "Activity_Index" and "Subject" data in first column are retained
features_extracted <- c(TRUE, TRUE, features_extracted)

# Resulting data for mean and standard deviation only
data <- data[, features_extracted]


##############################################################################
# (3) Use descriptive activity names to name the activities in the data set. #
##############################################################################

# Assign column names to activity labels data
names(activity_labels) <- c("Activity_Index", "Activity")

# Merge descriptive labels to data set
data <- merge(activity_labels, data)

# Drop "Activity_Index" column
data$Activity_Index <- NULL


###########################################################
# (4) Label the data set with descriptive variable names. #
###########################################################

names_data <- names(data)

names_data <- gsub("^t", "Time", names_data)
names_data <- gsub("^f", "Frequency", names_data)
names_data <- gsub("Acc", "Accelerometer", names_data)
names_data <- gsub("Gyro", "Gyroscope", names_data)
names_data <- gsub("BodyBody", "Body", names_data)
names_data <- gsub("mean\\()", "Mean", names_data)
names_data <- gsub("std\\()", "StdDev", names_data)
names_data <- gsub("meanFreq\\()", "Meanfreq", names_data)
names_data <- gsub("-", "", names_data)

names(data) <- names_data


#############################################################################
# (5) From the data set in step 4, creates a second, independent tidy data  #
# set with the average of each variable for each activity and each subject. #
#############################################################################

# Create tidy data set - NARROW/TALL format
library(tidyr)
data_tidy <- gather(data, Features, Response, -Activity, -Subject)

head(data_tidy)

# Create averages of each variable for each activity and subject combination

# Attach dplyr package
library(dplyr)

# Use group_by commmand to generate data set that will allow you to create means for different groups
avgs <- group_by(data_tidy, Activity, Subject)

# Means data in tidy format - NARROW/TALL format
mean_summary <- summarize(avgs, Average_Response = mean(Response))

write.table(mean_summary, "./mean_summary.txt")



