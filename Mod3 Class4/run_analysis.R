

#1.Merges the training and the test sets to create one data set.
library(data.table)
setwd("C:/Users/bib1/Documents/RStudio/MyProject/Module3Class4")
fileurl = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
if (!file.exists('./UCI HAR Dataset.zip')){
  download.file(fileurl,'./UCI HAR Dataset.zip', mode = 'wb')
  unzip("UCI HAR Dataset.zip", exdir = getwd())
}
features<- read.csv("./UCI HAR Dataset/features.txt",header=FALSE,sep=" ")
features<-as.character(features[,2])
training.subject<- read.csv("./UCI HAR Dataset/train/subject_train.txt",header=FALSE)
training.x<- read.table("./UCI HAR Dataset/train/X_train.txt")
training.y<- read.csv("./UCI HAR Dataset/train/y_train.txt",header=FALSE)
training.df<-data.frame(training.subject,training.y,training.x)
test.subject<- read.csv("./UCI HAR Dataset/test/subject_test.txt",header=FALSE)
test.x<- read.table("./UCI HAR Dataset/test/X_test.txt")
test.y<- read.csv("./UCI HAR Dataset/test/y_test.txt",header=FALSE)
test.df<-data.frame(test.subject,test.y,test.x)
combine.df<-rbind(training.df,test.df)
names(combine.df)<-c(c("subject","activity"),features)

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
mean_std<-grep("mean|std",features)
mean_std
combine.df<-combine.df[,c(1,2,mean_std+2)]

#3.Uses descriptive activity names to name the activities in the data set
activity.labels<-read.table("./UCI HAR Dataset/activity_labels.txt",header=FALSE)
activity.labels<-as.character(activity.labels[,2])
combine.df$activity<-activity.labels[combine.df$activity]

#4.Appropriately labels the data set with descriptive variable names.
combine.name<-names(combine.df)
combine.name<-gsub("[(][)]","",combine.name)
combine.name<-gsub("-","_",combine.name)
combine.name<-gsub("^t","time_",combine.name)
combine.name<-gsub("^f","frequency_",combine.name)
combine.name<-gsub("Acc","Accelerometer",combine.name)
combine.name<-gsub("Gyro","Gyroscope",combine.name)
combine.name<-gsub("Mag","Magnitude",combine.name)
combine.name<-gsub("std","standarddeviation",combine.name)

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
combine.newdf<-aggregate(combine.df[,3:81], by = list(activity = combine.df$activity, subject = combine.df$subject),FUN = mean)
write.table(x = combine.newdf, file = "data_tidy.txt", row.names = FALSE)