## run_analysis.R script
## JHU Data Science Specialization 
## Getting & Cleaning Data Course Project
## R.W C
## August 2014

## The R script "run_analysis.R" starts with the assumption that data directory 
## "UCI HAR Dataset" and related files are in the current working directory
 
## Read in the data sets
ActivityLabels <- read.table("activity_labels.txt")
FeaturesColNames <- read.table("features.txt")
TestSubjects <- read.table("./test/subject_test.txt")
TestFeatures <- read.table("./test/X_test.txt")
TestActivity <- read.table("./test/y_test.txt")

TrainSubjects <- read.table("./train/subject_train.txt")
TrainFeatures <- read.table("./train/X_train.txt")
TrainActivity <- read.table("./train/y_train.txt")

## Combine test and train populations
Subjects <- rbind(TestSubjects, TrainSubjects)
Features <- rbind(TestFeatures, TrainFeatures) ## Merges the training and the test data sets to one
Activity <- rbind(TestActivity, TrainActivity)

## update column names of features files to match Features file
aa <- as.vector(FeaturesColNames[,2])   ## take ColNames into a vector to 
colnames(Features) <- aa                ## apply to colnames of the dataframe

## Extracts only the measurements on the mean and standard deviation for each measurement
mean_data <- Features[ , grepl("mean()", names(Features))]
std_data <- Features[ , grepl("std()", names(Features))]
Tidy1 <- cbind(mean_data, std_data)

## Uses descriptive activity names to rename the numeric activities in the data set to words,
#changing ActivityLabel from numeric value to char label and store into LabeledActivities
df2 <- as.data.frame(sapply(Activity, gsub, pattern = 1, replacement = "WALKING"))
df2 <- as.data.frame(sapply(df2, gsub, pattern = 2, replacement = "WALKING_UPSTAIRS"))
df2 <- as.data.frame(sapply(df2, gsub, pattern = 3, replacement = "WALKING_DOWNSTAIRS"))
df2 <- as.data.frame(sapply(df2, gsub, pattern = 4, replacement = "SITTING"))
df2 <- as.data.frame(sapply(df2, gsub, pattern = 5, replacement = "STANDING"))
df2 <- as.data.frame(sapply(df2, gsub, pattern = 6, replacement = "LAYING"))
colnames(df2) <- "Activity"
LabeledActivities <-df2

## rename Subjects column name to Subject
colnames(Subjects) <-"Subject"

## Tidying the column names
## Across all columns, replace all instances of "-|\\()" with ""
names(Tidy1) <- gsub("-|\\()", "", names(Tidy1))

## Rename Columns to clarify Frequency, Mean, Xaxis, Yaxis, Zaxis, StandardDeviation
names(Tidy1) <- gsub("Freq", "Frequency", names(Tidy1))
names(Tidy1) <- gsub("mean", "Mean", names(Tidy1))
names(Tidy1) <- gsub("X", "Xaxis", names(Tidy1))
names(Tidy1) <- gsub("Y", "Yaxis", names(Tidy1))
names(Tidy1) <- gsub("Z", "Zaxis", names(Tidy1))
names(Tidy1) <- gsub("std", "StandardDeviation", names(Tidy1))

## bind subject ID, Activity and consolidated train + test features from Tidy1
FullTidy <- cbind(Subjects, LabeledActivities, Tidy1)

## Summarize FullTidy data frame by Activity and SubjectID, returning means
## for each variable (feature / column)
TidyAvg <- aggregate.data.frame(FullTidy, by = list(FullTidy$Subject, FullTidy$Activity), FUN = mean)

##relabel column names
names(TidyAvg) <- gsub("Group.1", "Subject", names(TidyAvg))
names(TidyAvg) <- gsub("Activity", "NA", names(TidyAvg))
names(TidyAvg) <- gsub("Group.2", "Activity", names(TidyAvg))

### alternative method using ddply in plyr
### library(plyr)
### TidyAvg <- ddply(FullTidy, c("Subject", "Activity"), summarise,
###                  mean = mean(FullTidy[, ]))

## write TidyAvg to table
write.table(TidyAvg, 'TidyDataSetRWC.txt', row.names=FALSE)
