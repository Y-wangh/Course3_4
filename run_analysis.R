url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "./coursework.zip", mode = "wb")
unzip("./coursework.zip")
setwd("./UCI HAR Dataset")

##0. Prep
##read data set 'train/X_train.txt' 'test/X_test.txt'
setTest <- read.table("./test/X_test.txt", header = FALSE)
setTrain <- read.table("./train/X_train.txt", header = FALSE)

##read lable 'train/y_train.txt''test/y_test.txt'
labelTest <- read.table("./test/Y_test.txt", header = FALSE)
labelTrain <- read.table("./train/Y_train.txt", header = FALSE)


## read subject 'train/subject_train.txt' 'test/subject_test.txt'
subjectTest <- read.table("./test/subject_test.txt", header = FALSE)
subjectTrain <- read.table("./train/subject_train.txt", header = FALSE)

## read features.txt

features <- read.table("./features.txt", header = FALSE)

## read activity_labels.txt

activity <- read.table("./activity_labels.txt", header = FALSE)
names(activity) <- c("class", "activityname")

##1. Merges the training and the test sets to create one data set.
#1-1. merge set X&Y, label X&Y, subject X&Y
setData <- rbind(setTest, setTrain)
names(setData) <- features$V2
labelData <- rbind(labelTest,labelTrain)
names(labelData) <- "class"
subjectData <- rbind(subjectTest, subjectTrain)
names(subjectData) <- "subject"

#1-2. creat a dataset incl. subject, set and feature 
data <- cbind(subjectData, labelData)
data <- cbind(data, setData)


##2. Extracts only the measurements on the mean and standard deviation for each measurement. 
extrfeature <- features$V2[grep("mean\\(\\)|std\\(\\)", features$V2)]
datanames <- c("subject", "class", as.character(extrfeature))
extrdata <- subset(data, select = datanames)


##3. Uses descriptive activity names to name the activities in the data set
extrdata$class <- factor(extrdata$class, levels = activity[,1], labels = activity[,2])


##4. Appropriately labels the data set with descriptive variable names. 
extrdata_col <- colnames(extrdata)
extrdata_col <- gsub("[\\(\\)-]", "", extrdata_col)
extrdata_col <- gsub("^f", "frequencyDomain", extrdata_col)
extrdata_col <- gsub("^t", "timeDomain", extrdata_col)
extrdata_col <- gsub("Acc", "Accelerometer", extrdata_col)
extrdata_col <- gsub("Gyro", "Gyroscope", extrdata_col)
extrdata_col <- gsub("Mag", "Magnitude", extrdata_col)
extrdata_col <- gsub("Freq", "Frequency", extrdata_col)
extrdata_col <- gsub("mean", "Mean", extrdata_col)
extrdata_col <- gsub("std", "StandardDeviation", extrdata_col)
extrdata_col <- gsub("BodyBody", "Body", extrdata_col)

colnames(extrdata) <- extrdata_col

##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
extrdata_mean <- extrdata %>% 
  group_by(subject, class) %>%
  summarise_each(funs(mean))

## export
write.table(extrdata, './tidyData.txt',row.names=FALSE)

