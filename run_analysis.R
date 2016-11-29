# Extracting data

Y_test  <- read.table(file.path("./UCI HAR Dataset", "test" , "Y_test.txt" ),header = FALSE)
Y_train <- read.table(file.path("./UCI HAR Dataset", "train", "Y_train.txt"),header = FALSE)

X_test  <- read.table(file.path("./UCI HAR Dataset", "test" , "X_test.txt" ),header = FALSE)
X_train <- read.table(file.path("./UCI HAR Dataset", "train", "X_train.txt"),header = FALSE)

subject_train <- read.table(file.path("./UCI HAR Dataset", "train", "subject_train.txt"),header = FALSE)
subject_test  <- read.table(file.path("./UCI HAR Dataset", "test" , "subject_test.txt"),header = FALSE)

features <- read.table("./UCI HAR Dataset/features.txt")[,2]
activityLabels = read.table('./UCI HAR Dataset/activity_labels.txt')

# Assigning names
colnames(X_train) <- features 
colnames(Y_train) <-"activityId"
colnames(X_test) <- features
colnames(Y_test) <- "activityId"
colnames(subject_test) <- "subjectId"
colnames(subject_train) <- "subjectId"
colnames(activityLabels) <- c('activityId','activityType')

# 1. Merges the training and the test sets to create one data set.
merge_train = cbind(Y_train, subject_train, X_train)
merge_test = cbind(Y_test, subject_test, X_test)
merge_all = rbind(merge_train, merge_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
colNames <- colnames(merge_all)
mean_and_std <- (grepl("activityId" , colNames) | 
                   grepl("subjectId" , colNames) | 
                   grepl("mean.." , colNames) | 
                   grepl("std.." , colNames) 
)
merge_all <- merge_all[ , mean_and_std == TRUE]

# 3. Uses descriptive activity names to name the activities in the data set
merge_all <- merge(merge_all, activityLabels, by='activityId', all.x=TRUE)

# 4. Appropriately labels the data set with descriptive variable names.
names(merge_all)<-gsub("^t", "time", names(merge_all))
names(merge_all)<-gsub("^f", "frequency", names(merge_all))
names(merge_all)<-gsub("Acc", "Accelerometer", names(merge_all))
names(merge_all)<-gsub("Gyro", "Gyroscope", names(merge_all))
names(merge_all)<-gsub("Mag", "Magnitude", names(merge_all))
names(merge_all)<-gsub("BodyBody", "Body", names(merge_all))

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
newData<-aggregate(. ~subjectId + activityId, merge_all, mean)
newData<-newData[order(newData$subjectId,newData$activityId),]
write.table(newData, file = "tidydata.txt",row.name=FALSE,quote = FALSE, sep = '\t')