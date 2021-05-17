library(reshape2)

#Part 1.Merging train & test datasets
#******************************************************************

# 1.1 loading data files

x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

features <- read.table('./UCI HAR Dataset/features.txt')

activityLabels = read.table('./UCI HAR Dataset/activity_labels.txt')

colnames(x_train) <- features[,2]
colnames(y_train) <-"A_Id"
colnames(subject_train) <- "S_Id"

colnames(x_test) <- features[,2] 
colnames(y_test) <- "A_Id"
colnames(subject_test) <- "S_Id"

colnames(activityLabels) <- c('A_Id','activityType')

# 1.2 Giving names to test and train datasets:

mrg_train <- cbind(y_train, subject_train, x_train)
mrg_test <- cbind(y_test, subject_test, x_test)
part_1 <- rbind(mrg_train, mrg_test)

#dim(setAllInOne)
#[1] 10299   563

#**************************************
#Part 2.-Extracts   measurements on  mean and sd  for train/test
#*******************************************

#2.1 Reading column names:

colNames <- colnames(part_1)

#2.2 defining logical vector for neccasery data

mean_std <- (grepl("A_Id" , colNames) | 
                   grepl("S_Id" , colNames) | 
                   grepl("mean.." , colNames) | 
                   grepl("std.." , colNames) 
)

#2.3 Making nessesary subset from part_1:

Part_2 <- part_1[ , mean_std == TRUE]

#******************************************************************
#Part 3. Uses descriptive activity names to name the activities in the data set
#******************************************************************

Part_3 <- merge(Part_2, activityLabels,
                              by='A_Id',
                              all.x=TRUE)

#******************************************************************
#Part 4. Appropriately labels the data set with descriptive variable names.
#******************************************************************

#Done in previous steps, see 1.3,2.2 and 2.3!

#******************************************************************
#Part 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#******************************************************************

#5.1 Making a second tidy data set, averaging while removing NAs


Part_5 <- melt(Part_3, id.vars = c('A_Id', 'S_Id'))
Part_5$value <- as.numeric(Part_5$value)
Part_5 <- na.exclude(Part_5)
Sec_Set <- dcast(Part_5, S_Id + A_Id ~ variable, mean, na.rm = TRUE)

#5.2 Writing second tidy data set in txt file

write.table(Sec_Set, "Sec_Set.txt", row.name=FALSE)