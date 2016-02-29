##########################################################################################################
## Coursera Getting and Cleaning Data Course Project
##########################################################################################################
#
# This script will perform the following steps on the UCI HAR Dataset downloaded from
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement.
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names.
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#
##########################################################################################################

setwd('./UCI HAR Dataset');

##########################################################################################################
# 1. Merge the training and the test sets to create one data set.
##########################################################################################################

# load features and activities
features        = read.table('./features.txt', header=FALSE);
activity_labels = read.table('./activity_labels.txt', header=FALSE);

# load training data
subject_train   = read.table('./train/subject_train.txt', header=FALSE);
x_train         = read.table('./train/x_train.txt', header=FALSE);
y_train         = read.table('./train/y_train.txt', header=FALSE);

# set column names
colnames(activity_labels) = c('activityId', 'activity_labels');
colnames(subject_train)   = 'subjectId';
colnames(x_train)         = features[,2];
colnames(y_train)         = 'activityId';

# merge all training sets
training_data = cbind(y_train, subject_train, x_train);

# load test data
subject_test = read.table('./test/subject_test.txt', header=FALSE);
x_test       = read.table('./test/x_test.txt', header=FALSE);
y_test       = read.table('./test/y_test.txt', header=FALSE);

# set column names
colnames(subject_test) = "subjectId";
colnames(x_test)       = features[,2];
colnames(y_test)       = "activityId";

# merge all test data
test_data = cbind(y_test, subject_test, x_test);

# Combine training and test data to create a final data set
combined_data = rbind(training_data, test_data);

# Create a vector for the column names from the combined_data, which will be used
# to select the desired mean() & stddev() columns
columns = colnames(combined_data);

##########################################################################################################
# 2. Extract only the measurements on the mean and standard deviation for each measurement.
##########################################################################################################

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",columns) | grepl("subject..",columns) | grepl("-mean..",columns) & !grepl("-meanFreq..",columns) & !grepl("mean..-",columns) | grepl("-std..",columns) & !grepl("-std()..-",columns));

# Subset combined_data table based on the logicalVector to keep only desired columns
combined_data = combined_data[logicalVector==TRUE];

##########################################################################################################
# 3. Use descriptive activity names to name the activities in the data set
##########################################################################################################

# Merge the combined_data set with the acitivityType table to include descriptive activity names
combined_data = merge(combined_data, activity_labels, by='activityId', all.x=TRUE);

# Updating the columns vector to include the new column names after merge
columns  = colnames(combined_data);

##########################################################################################################
# 4. Appropriately label the data set with descriptive activity names.
##########################################################################################################

# clean up the variable names
for (i in 1:length(columns)) {
  columns[i] = gsub("\\()","", columns[i])
  columns[i] = gsub("-std$", "StdDev", columns[i])
  columns[i] = gsub("-mean", "Mean", columns[i])
  columns[i] = gsub("^(f)", "freq", columns[i])
  columns[i] = gsub("^(t)", "time", columns[i])
  columns[i] = gsub("[Gg]yro", "Gyro", columns[i])
  columns[i] = gsub("([Gg]ravity)", "Gravity", columns[i])
  columns[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)", "Body", columns[i])
  columns[i] = gsub("([Bb]odyaccjerkmag)", "BodyAccJerkMagnitude", columns[i])
  columns[i] = gsub("AccMag", "AccMagnitude", columns[i])
  columns[i] = gsub("JerkMag", "JerkMagnitude", columns[i])
  columns[i] = gsub("GyroMag", "GyroMagnitude", columns[i])
};

# use the new descriptive column names
colnames(combined_data) = columns;

##########################################################################################################
# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.
##########################################################################################################

# create a new table without the activity_labels column
combined_data_sans_labels = combined_data[,names(combined_data) != 'activity_labels'];

# calculate the mean values for the target columns
tidy_output = aggregate(combined_data_sans_labels[,names(combined_data_sans_labels) != c('activityId', 'subjectId')], by=list(activityId=combined_data_sans_labels$activityId, subjectId = combined_data_sans_labels$subjectId), mean);

# merge tidy_output with activity_labels
tidy_output = merge(tidy_output, activity_labels, by='activityId', all.x=TRUE);

# export the tidy_output set
write.table(tidy_output, './tidy_output.txt', row.names=TRUE, sep='\t');
