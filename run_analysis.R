## R script to get and clean data for Course project

library(sqldf);
library(plyr);
## First set the working directory
setwd("C:/Users/avalsarajan/Desktop/Data Science/Coursera/3.Getting and Cleaning Data/UCI HAR Dataset")


features     = read.table('./features.txt',header=FALSE); 
activity_labels = read.table('./activity_labels.txt',header=FALSE); 
subject_train = read.table('./train/subject_train.txt',header=FALSE); 
x_train       = read.table('./train/x_train.txt',header=FALSE);
y_train       = read.table('./train/y_train.txt',header=FALSE); 
subject_test = read.table('./test/subject_test.txt',header=FALSE); 
x_test       = read.table('./test/x_test.txt',header=FALSE); 
y_test       = read.table('./test/y_test.txt',header=FALSE);

## Using commands like head(),strt() and dim() we begin to get an understanding of the 
## data set we are working with. It appears that within Test and Train data, there are 3 levels of
## metadata that define the data set.
## 1.) The identity of the SUbject for which each of the feature observations were recorded
## 2.) The features that were recorded for each subject
## 3.) The activity being recorded for each feature, for each subject
## This helps us visualize the merge that we can perform for a unified dataset
## We can first create a unified data for all of the Test and Train data respectively, 
## by combining datasets at the column level.
## We can then merge the Test and Train data sets at the row level since they 
## will have the same columnar structure.

## Before we begin the merge process, we can assign meaningful names to our data,
## by swapping out identifiers for descriptors
## we use the features file to pick up the names and assign it to to the xTrain file

colnames(activity_labels)  = c('activity_ID','activity_Name');
colnames(subject_train)  = "subject_ID";
colnames(x_train)        = features[,2]; 
colnames(y_train)        = "activity_ID";

## perform the same operation on test data
colnames(subject_test)  = "subject_ID";
colnames(x_test)        = features[,2]; 
colnames(y_test)        = "activity_ID";

## First we merge the Train data files into a single data set
Train<-cbind(subject_train,x_train,y_train);
## Now we do the same to Test
Test<-cbind(subject_test,x_test,y_test);
# Finally, we combine training and test data to create the unified data set
Unified_Data = rbind(Train,Test);

## Step2
## We now need to subset the data to only include measurements involving mean and standard deviation
## First we store the column names in a dataframe so we can run a match on them
## Then use SQL to run a LIKE statement that does a match on column names to pick out mean and std realted ones

column_names<-as.data.frame(colnames(Unified_Data));
colnames(column_names) <- c("names");
subset_cols<-(sqldf("select * from column_names where names like '%-mean()%' or names like '%-std()%' or names ='subject_ID' or names='activity_ID' "));
## We then subset our original unified data set to only include the measurements with mean and std

mean_and_std_cols<-as.character(subset_cols$names);
Unified_Data<-Unified_Data[mean_and_std_cols];

## Step3
## To replace the activity_ID with the actual Activity Names, we use the ActivityType data as a lookup
## We then use SQL to join to the two data sets, and add the activity_Name
## Once the activity names have been added, we drop the activity_ID column

Unified_Data<-sqldf("select a.*,b.activity_Name from Unified_Data a inner join activity_labels b on a.activity_ID = b.activity_ID");
Unified_Data<-subset(Unified_Data,select=-c(activity_ID));

##Step4
## We now use the gsub command to replace the variable names with more descriptive values
names(Unified_Data)<-gsub("^t", "time", names(Unified_Data));
names(Unified_Data)<-gsub("^f", "frequency", names(Unified_Data));
names(Unified_Data)<-gsub("Acc", "Accelerometer", names(Unified_Data));
names(Unified_Data)<-gsub("Gyro", "Gyroscope", names(Unified_Data));
names(Unified_Data)<-gsub("Mag", "Magnitude", names(Unified_Data));
names(Unified_Data)<-gsub("BodyBody", "Body", names(Unified_Data));

##Step5
## Finally, we create a tidy data set that calculates all the feature means by Subject and Activity
## This tidy data set is then written to an output file
tidy_data<-aggregate(. ~subject_ID + activity_Name, Unified_Data, mean);
write.table (tidy_data, './tidy_data.txt',row.names=FALSE);


