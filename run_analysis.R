## run_analysis.R
## Getting and Cleaning Data course assginment.
##
## Merges the training and the test sets to create one data set.
## Extracts only the measurements on the mean and standard deviation for each measurement. 
## Uses descriptive activity names to name the activities in the data set
## Appropriately labels the data set with descriptive variable names. 
## Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 



## library used to apply mean function to the tidy dataset
library(plyr)

## First of all, both datasets have been dowloaded from the given path, and read as tables:
## (notice the base directory for the data is data/UCIHARDataset)
worktest <- read.table("./data/UCIHARDataset/test/X_test.txt")
worktrain<- read.table("./data/UCIHARDataset/train/X_train.txt")

##variable names
possiblenames <- readLines("./data/UCIHARDataset/features.txt")

## function to remove numbers from variable names
secondElement <- function(x){x[2]}
## applied to the names dataset.
completenames <- sapply(strsplit(possiblenames, " "), secondElement)
## and each column on both datasets is named.
names(worktest) <- completenames
names(worktrain) <- completenames

## activity labels are read
y_test <- as.numeric (readLines("./data/UCIHARDataset/test/y_test.txt"))
y_train <- as.numeric (readLines("./data/UCIHARDataset/train/y_train.txt"))
## and so are their names, by removing their numbers
activitynames <- readLines("./data/UCIHARDataset/activity_labels.txt")
activitynames <- sapply(strsplit(activitynames, " "), secondElement)

## and replacing numbers by names
replacebyname <- function(x, names){names[x]}
testactivitylabels <- sapply (y_test, replacebyname, names=activitynames)
trainactivitylabels <- sapply (y_train, replacebyname, names=activitynames)
## a column is added to each dataset including the activity
worktest$activity <-testactivitylabels
worktrain$activity <-trainactivitylabels
## perhaps  another factor column  could have been added, specifying source, like in
##worktest$type <- rep ("TEST", length (testactivitylabels))
## but this is out of the project scope.

## subject Id labels are read
subject_test <- readLines("./data/UCIHARDataset/test/subject_test.txt")
subject_train <- readLines("./data/UCIHARDataset/train/subject_train.txt")

## and added as another column.
worktest$subject <-subject_test
worktrain$subject<- subject_train
completenames <- c(completenames, "activity", "subject")

##only the mean and std values will be taken into account
tidytest <- worktest[grep ("mean\\(\\)|Mean|std\\(\\)|activity|subject", completenames)]
tidytrain <- worktrain[grep ("mean\\(\\)|Mean|std\\(\\)|activity|subject", completenames)]
## factoring varialbles will be set as factors:
tidytest$activity =factor(tidytest$activity, labels=activitynames)
tidytrain$activity =factor(tidytrain$activity, labels=activitynames)
tidytest$subject =factor(tidytest$subject)
tidytrain$subject =factor(tidytrain$subject)

##and data is joined into one dataset.
tidydata<- rbind (tidytrain, tidytest)

## finally the mean for each variable, subject and activity.
meanresult <- ddply(tidydata, .(activity, subject), numcolwise(mean))

## and the result is loaded into a .txt file
write.table(meanresult, file="./data/tidymean.txt", sep = "\t", append=F)
