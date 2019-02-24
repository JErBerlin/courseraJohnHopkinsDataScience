######################################## run_analysis.R ####
#
# Getting and cleaning data. Final Assignment
#
# 1. Read and label data files  
# - read data from train subject number, train activity labels and pricipal training data 
# - combine it together
# - create a new column: whether the observation is a training or testing 
# - variable names are chosen according to the feature
# - do the same for test data 
#
# 2. Merging
# - merges train and test data sets by row bind method
#
# 3. Write activities labels
# - Edit activity labels to reflect the activity 
#
# 4. Summarizing by mean and sd
#
########################################.

## load libraries ####
library(dplyr)          # for data manipulation
require(formattable)    # for nice formatting of table views (not necessary for functionality)

## Read train data ####
trSubject  <-read.table("train/subject_train.txt")  # read train subject 
trLabels   <-read.table("train/y_train.txt")        # read train activity label 
trData     <-read.table("train/X_train.txt")        # read the rest of train data
isTrain <-c(rep("Training",nrow(trData)))           # to identify training dataset
trainDF    <-cbind(trSubject, trLabels, isTrain, trData) 
  # binded 3 cols as train data for subject, activity label and train data 

## Read features.txt to add heading names to trainDF ####
features <-read.table("features.txt")   # read features 
features <-as.character(features[,2])   # drops 1st column of corresponding numbers
                                        # because the vars in the DF are correctly sorted
cNames <-append(c("subjectId", "activityLabels", "TestOrTrain"),features) 
  # add 3 new categories for the column names
colnames(trainDF)<-cNames  

## Read test data ####
tsSubject <-read.table("test/subject_test.txt") # read test subject 
tsLabels  <-read.table("test/y_test.txt")       # read test activity label
tsData    <-read.table("test/X_test.txt")       # read the rest of test data
isTest    <-c(rep("Testing",nrow(tsData)))      # to identify test dataset
testDF    <-cbind(tsSubject, tsLabels, isTest, tsData)
colnames(testDF)<- cNames
##built second dataframe for Test data

## Bind both dataframes with rbind() ####
deviceData <- rbind(trainDF,testDF) 
  # row bind of train data on the top and test data at the bottom

## select columns of the dataframes containing particular string: ####
## we suppose that names(trainDF) == names(testDF)
grep("mean()\\b", names(trainDF))->meanStr   # containing "mean()" 
grep("std()\\b",  names(trainDF))->stdStr    # containing "std()" 
  # we have to add '\\b' for boundary, so that we don't match 'meanFreq()' for instance
sort(c(meanStr, stdStr))-> selectCols     # containing "mean()" or "std()"
deviceData.MeanStd <- deviceData[,c(1,2,3,selectCols)]   # 

## prepare a final summary of device data ####
## Replace activity values with an explicit description of the activity
activityDescription <- mapvalues(deviceData.MeanStd$activityLabels, c(1:6), 
  c("walking","walking_up","walking_dwn","sitting","standing","lying"))
deviceData.MeanStd$activityLabels <- activityDescription
deviceData.MeanStd$activityLabels <- as.factor(deviceData.MeanStd$activityLabels)

## summarise: grouping by subject, activity and test/train and compute the group mean
tblDeviceMeanStd<-tbl_df(deviceData.MeanStd) 
   # convert df to table to manipulate data with dyplr
deviceSummary <- group_by(tblDeviceMeanStd, subjectId, activityLabels, TestOrTrain)
deviceSummary <- summarise_all(deviceSummary, mean)

# Make names of the headers more explicit
names(deviceSummary) <- gsub('\\(|\\)',"",names(deviceSummary), perl = TRUE)
names(deviceSummary) <- gsub('-',"",names(deviceSummary), perl = TRUE)
names(deviceSummary) <- gsub("^t", "time", names(deviceSummary))
names(deviceSummary) <- gsub("^f", "frequency", names(deviceSummary))
names(deviceSummary) <- gsub("Mag", "Magnitude", names(deviceSummary))
names(deviceSummary) <- gsub("Freq", "Frequency", names(deviceSummary))
names(deviceSummary) <- gsub("Acc", "Acceleration", names(deviceSummary))
names(deviceSummary) <- gsub("Gyro", "Gyroscopic", names(deviceSummary))
names(deviceSummary) <- gsub("mean", "Mean", names(deviceSummary))
names(deviceSummary) <- gsub("std", "Stdev", names(deviceSummary))

## Summary output 
formattable(deviceSummary)

