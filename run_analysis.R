#   Quiz objectives
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#
#######################################
# License:
# Use of this dataset in publications must be acknowledged by referencing the following publication Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
#######################################
setwd("F:/Tom/R/DataScienceCourse/Final/Working/UCI HAR Dataset")
require(dplyr)
require(plyr)
require(stringr)
#######################################
#   read test subject file
testsubject <- read.csv(file = "./test/subject_test.txt", header = FALSE, sep = " ")
#   read test activity file
testactivity <- read.csv(file = "./test/y_test.txt", header = FALSE, sep = " ")
#   read train subject file
trainsubject <- read.csv(file = "./train/subject_train.txt", header = FALSE, sep = " ")
#   read train activity file
trainactivity <- read.csv(file = "./train/y_train.txt", header = FALSE, sep = " ")

#   Cleaning up input test flat file
#   read X_test.txt flat file
tx  <- readLines("./test/X_test.txt")
#   double spaces reduced to single spaces
tx2  <- gsub(pattern = "  ", replace = " ", x = tx)
#   single spaces changed to tabs
tx3 <- gsub(pattern = " ", replace = "\t", x = tx2)
#   beginning of line tab removed
tx4 <- gsub(pattern = "^\t", replace = "", x = tx3)
#   write out cleaned data to tmp file so won't overwrite original
writeLines(tx4, con="./test/X-test_flat.txt")
#   cleanup Environment
rm(tx);rm(tx2);rm(tx3);rm(tx4)

#   Cleaning up input train flat file
#   read X_test.txt flat file
tr  <- readLines("./train/X_train.txt")
#   double spaces reduced to single spaces
tr2  <- gsub(pattern = "  ", replace = " ", x = tr)
#   single spaces changed to tabs
tr3 <- gsub(pattern = " ", replace = "\t", x = tr2)
#   beginning of line tab removed
tr4 <- gsub(pattern = "^\t", replace = "", x = tr3)
#   write out cleaned data to tmp file so won't overwrite original
writeLines(tr4, con="./train/X-train_flat.txt")
#   cleanup Environment
rm(tr);rm(tr2);rm(tr3);rm(tr4)

#   read from cleaned up test tmp file
test <- read.csv(file = "./test/X-test_flat.txt", header = FALSE, sep = "\t")
#   read from cleaned up train tmp file
train <- read.csv(file = "./train/X-train_flat.txt", header = FALSE, sep = "\t")
activitiyLabels <- read.csv(file = "activity_labels.txt", header = FALSE, sep = " ")

features <- read.csv(file = "features.txt", header = FALSE, sep = " ")
myFeaturesCols <- features[grepl("(mean|std)", features$V2),]
myFeatures <- as.list(lapply(myFeaturesCols$V2, function(y) gsub("-", "", y)))
myFeatures <- (lapply(myFeatures, function(y) gsub("\\(", "", y)))
myFeatures <- (lapply(myFeatures, function(y) gsub("\\)", "", y)))
myFeatures <- (lapply(myFeatures, function(V2) tolower(V2)))

#   Reduce number of columns to mean and std (standard deviation columns)
#test2 <- test[, as.integer( myFeatures)]
test2 <- test[, myFeaturesCols$V1]
#   combine subject, activity test data columns
test2 <- cbind(testsubject, testactivity, test2)

#   Reduce number of columns to mean and std (standard deviation columns)
train2 <- train[, myFeaturesCols$V1]
#   combine subject, activity test data columns
train2 <- cbind(trainsubject, trainactivity, train2)
#colnames(train2) <- c("subject", "activity", myFeatures$V2)

combined <- rbind(test2, train2)
#   add labels
labels <- paste(myFeatures, sep = ",")
myLabels <- c("subject", "activity", labels)

colnames(combined) <- myLabels

# update values with correct activity names

combined[, 2] <- activitiyLabels[combined[, 2], 2]

write.csv(combined, file = "run_analysis_results.csv")
#   Task 5

#
myMeanData <- ddply(combined, .(subject, activity), function(x) colMeans(x[, 3:81]))
write.table(myMeanData, file = "Task5.csv", row.names = FALSE, col.names = TRUE)
#write.csv(myMeanData, file = "myMeanData.csv")
