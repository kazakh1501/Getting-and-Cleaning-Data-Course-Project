################################################################################
##
## Function : averageByActivityAndSubject()
##
## Description : 
## This function reads in the data collected from the accelerometers from the 
## Samsung Galaxy S smartphone of a group of 30 subjects within an 
## age bracket of 19-48 years. Each subject performed six activities 
## (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) 
## wearing a smartphone (Samsung Galaxy S II) on the waist.
##
## It computes average of the mean and standard deviation of the sensor signals  
## (accelerometer and gyroscope) for each activity and each subject. 
##
## Executing the function:
## The data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  
## (preserving the original folder structure shall be unzipped in the same 
## directory as this source code.
##
## Arguments:
## Not Applicable. 
##
## Return:
## A file "AverageByActivityAndSubject.txt" is created on the same directory
## as the source code.
##
################################################################################
averageByActivityAndSubject <- function() {

## Load libraries
library(dplyr)
library(plyr)
library(car)
library(reshape2)

## setup the data directories.
fSubjTst <- ".\\test\\subject_test.txt"
fDatTst <- ".\\test\\X_test.txt"
fActivityTst <- ".\\test\\y_test.txt"
fSubjTrain <- ".\\train\\subject_train.txt"
fDatTrain <- ".\\train\\X_train.txt"
fActivityTrain <- ".\\train\\y_train.txt"
fFeatures <- ".\\features.txt"
fActivityLabels <- ".\\activity_labels.txt"

## read features.txt
datFeatures <- read.table(fFeatures, col.names=c("id", "feature.name"))

## read activity_labels.txt
datActivityLabels <- read.table(fActivityLabels, col.names=c("Id", "Label"))
featureNames <- as.vector(datFeatures$feature.name)

## read subject_test.txt
datSubjTst <- read.table(fSubjTst, col.names=c("Subject"))

## read X_test.txt
datDatTst <- read.table(fDatTst, strip.white=TRUE, sep="")

## read y_test.txt
datActivityTst <- read.table(fActivityTst, col.names=c("Activity"))

## read subject_train.txt
datSubjTrain <- read.table(fSubjTrain, col.names=c("Subject"))

## read X_train.txt
datDatTrain <- read.table(fDatTrain, strip.white=TRUE, sep="")

## read y_train.txt
datActivityTrain <- read.table(fActivityTrain, col.names=c("Activity"))

## merge test and train sensor signals measurements data sets
datDatMerge <- rbind(datDatTst,datDatTrain)

## label sensor signals measurement with descriptive names
names(datDatMerge) <- featureNames

## select mean and standard deviation of the sensor signals measurements
datDatMerge <- datDatMerge[grepl("mean\\(\\)", colnames(datDatMerge))|grepl("std\\(\\)", colnames(datDatMerge))]

## get the list of sensor signals measurements
datVariables <- names(datDatMerge)

## merge test and train activities data sets
datActivityMerge <- rbind(datActivityTst,datActivityTrain)

## merge test and train subject data sets
datSubjMerge <- rbind(datSubjTst,datSubjTrain)

## merge subject, activities and measurement data sets
datTidy <- cbind(datSubjMerge,datActivityMerge,datDatMerge)

## recode activities with descriptive activity names
datTidy$Activity<-recode(datTidy$Activity,"1='WALKING';2='WALKING_UPSTAIRS';
                         3='WALKING_DOWNSTAIRS';4='SITTING';5='STANDING';6='LAYING'")

## melt subject & activity columns as index and measurements as measureable variables
datTidyMelt <- melt(datTidy, id.vars = c("Subject", "Activity"), measure.vars = datVariables)

## calculate mean of each measurements
aveActivityAndSubject <- dcast(datTidyMelt, Subject + Activity ~ variable, mean)

## melt subject & activity columns as index and mean of each measurements
aveActivityAndSubjectMelt <- melt(aveActivityAndSubject, id.vars = c("Subject", "Activity"), 
                          measure.vars = datVariables, variable.name = "Measurement", 
                          value.name = "Average")

## order each observations by first, Subject, then second, by Activity
aveActivityAndSubjectSorted <- arrange(aveActivityAndSubjectMelt, Subject, Activity)

## write to AverageByActivityAndSubject.txt file
write.table(aveActivityAndSubjectSorted, file ="AverageByActivityAndSubject.txt", row.name=FALSE)
}