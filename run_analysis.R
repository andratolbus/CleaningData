#You should create one R script called run_analysis.R that does the following. 
#---Merges the training and the test sets to create one data set.
#---Extracts only the measurements on the mean and standard deviation for each measurement. 
#---Uses descriptive activity names to name the activities in the data set
#---Appropriately labels the data set with descriptive variable names. 
#---From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#the code runs assuming we are in the UCI HAR Dataset unzipped folder 

require("data.table")
require("reshape2")




################ 1. Merges the training and the test sets to create one data set ##############

# Load: data column names
features <- read.table("features.txt",stringsAsFactors=F) #reading features
features<-features$V2 #working with the labels alone, excluding indexes from the table

x_test <- read.table("./test/X_test.txt",stringsAsFactors=F) #reading test set
colnames(x_test)<-features #labeling data with descriptive variable names
y_test <- read.table("./test/y_test.txt",stringsAsFactors=F) #reading test labels 
subject_test <- read.table("./test/subject_test.txt",stringsAsFactors=F)
x_test$subject<-subject_test[,1] #getting the ids for the subjects in the test data set
x_test$activity = y_test[,1] #getting the ids for the activities in the training data set 

x_train<-read.table("./train/X_train.txt",stringsAsFactors=F) #reading training set
colnames(x_train)<-features #labeling data with descriptive variable names
y_train<-read.table("./train/y_train.txt") #reading training labels
subject_train<-read.table("./train/subject_train.txt",stringsAsFactors=F)
x_train$subject<-subject_train[,1] #getting the ids for the subjects in the test data set
x_train$activity = y_train[,1] #getting the ids for the subjects in the test data set

merged<-rbind(x_train,x_test) #merginf training and test data, binding by row


################  2. Get mean and standard deviation columns  #########




sd.mean.features<-subset(features, (grepl("std|mean",features)==TRUE)) #extracting features containing only the mean and standard deviation

extracted<-merged[,sd.mean.features] #extracting from the main dataset the columns corresponding
                                     #to mean and standard deviation only 


extracted$subject<-merged$subject #add subject from main data frame, excluded above
extracted$activity<-merged$activity # add activity from main data frame excluded above

############# 3 Uses descriptive activity names to name the activities in the data set ######

activity_labels <- read.table("activity_labels.txt",stringsAsFactors=F) #reading activity labels
activity_labels<-activity_labels$V2 #working with the labels alone, excluding indexes from the table
activity_labels<-as.factor(activity_labels) # converting column to factor 
extracted$activity<-as.factor(extracted$activity) #converting column to factor 
extracted$activity_label = factor(extracted$activity, levels=c(1,2,3,4,5,6), 
                                       labels=activity_labels) # applying corresponding labels to the activity ids
                                                               #using factor(); each id c(1,2,3,4,5,6) corresponds to a single activity 
                                                               


############# 4 Appropriately labels the data set with descriptive variable names.  ######

### achievied at step 1: by assigning the column names with the second column from the features table


#########  5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  


id_labels   = c("subject", "activity", "activity_label") # creating a labels vector with the measures
labels = setdiff(colnames(extracted), id_labels) #extracting columns corresponding to variables, excluding the list above
melted      = melt(extracted, id = id_labels, measure.vars = labels) #re-arranging dataframe - measures, variables and values

# Generating the tidy dataset with the average of each variable for each activity and each subject
# The dataset is aggregated by the casting formula and applying the mean function  to the re-arranged dataset using melt()


tidy_data   = dcast(melted, subject + activity_label ~ variable, mean)
write.table(tidy_data,"tidy_data.txt",sep="\t",row.names=F,col.names=T,quote=F)
