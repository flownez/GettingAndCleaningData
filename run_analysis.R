# This script creates a tidy data set of the wearable data set supplied
# for the week 4 assignment for the Coursera course "Getting and Cleaning
# Data "
# Author : Ben Jones
# Date: 9/05/2016

library(dplyr)

#Root dataset Folder
d_folder <- "UCI HAR Dataset"

#File name templates
f_activity_labels <- "UCI HAR Dataset/activity_labels.txt"   
f_feature_labels  <- "UCI HAR Dataset/features.txt"          

f_x_test          <- "UCI HAR Dataset/test/X_test.txt"           
f_y_test          <- "UCI HAR Dataset/test/y_test.txt"           
f_subject_test    <- "UCI HAR Dataset/test/subject_test.txt"     

f_x_train         <- "UCI HAR Dataset/train/X_train.txt"           
f_y_train         <- "UCI HAR Dataset/train/y_train.txt"           
f_subject_train   <- "UCI HAR Dataset/train/subject_train.txt"     

#read a whitespace delimited file given the filename
loadFile <- function(name){
        in_file <- read.delim(
                name,
                header = FALSE,
                sep = "",
                stringsAsFactors = FALSE)
        in_file
}

#Setup activity factors feature names
activity_labels_raw <- loadFile(f_activity_labels)
activity_labels <- as.factor(activity_labels_raw[,2])

feature_labels_raw  <- loadFile(f_feature_labels)
feature_labels <- feature_labels_raw[,2]

# merge component x,y and subject files
# x
x_test <- loadFile(f_x_test)
x_train <- loadFile(f_x_train)
x <- rbind(x_test,x_train)
rm(x_test,x_train)  # clean up memory

# apply label names and filter for mean and stdev
names(x) <- as.character(sub("()","",feature_labels,fixed = TRUE))
x <- x[,grep("mean\\(\\)|std\\(\\)",feature_labels)]

#y
y_test <- loadFile(f_y_test)
y_train <- loadFile(f_y_train)
y <- rbind(y_test,y_train)
rm(y_test,y_train)

#setup activity labels
y <- mutate(y,activity = levels(activity_labels)[y$V1]) 
activity <- y$activity

#subject
subject_test <- loadFile(f_subject_test)
subject_train <- loadFile(f_subject_train)
subject <- rbind(subject_test,subject_train)
rm(subject_test,subject_train)
subject <- subject$V1

#merge component data into a single data frame
wearable_data <- cbind(activity, subject, x)

#summarised mean of dataset grouped by subject and activity
summary_data <- wearable_data %>%
        group_by(subject,activity) %>% 
        summarise_each(funs(mean))
write.table(summary_data,"tidy_wearable_dataset.txt", 
            row.names  = FALSE)


