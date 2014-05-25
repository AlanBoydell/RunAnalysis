# Script for processing Human Activity Recognition Using Smartphones Dataset
# This script will carry out the following:
# 1. Merge training and test sets 
# 2. Extract the measurements on the mean and standard deviation 
# 3. Appropriately label data set with descriptive activity names. 
# 4. Create a second, independent tidy data set with the mean of each variable for each activity and each subject. 

# Check for appropriate Packages
if (!require("data.table")) {
        install.packages("data.table")
        require("data.table")
}
if (!require("reshape2")) {
        install.packages("reshape2") 
        require("reshape2")
}

# Function to load data and merge it with labels
getCleanData <- function(x_file, y_file, subj_file, features, activity) {
        # Save labels that we need for processing
        k_features <- grepl("mean|std", features)
        
        # Load whole data file
        x_dat <- read.table(x_file)
        names(x_dat) = features
        
        # Filter only mean and std columns
        x_dat = x_dat[,k_features]
        
        # Read activities data
        y_dat <- read.table(y_file)
        y_dat[,2] = activity[y_dat[,1]]
        names(y_dat) = c("Activity_ID", "Activity_Label")
        
        # Read subject IDs data
        subject <- read.table(subj_file)
        names(subject) = "subject"
        
        # Link all columns toghether
        cbind(as.data.table(subject), y_dat, x_dat)
}

# Main function for processing dataset
processDataSet <- function(data_path = "UCI HAR Dataset/") {
        # Load data column names for later use
        features <- read.table(paste(data_path,"features.txt", sep="")) [,2]
        # Load names of activities
        activity <- read.table(paste(data_path,"activity_labels.txt", sep="")) [,2]
        
        # Load test and train data
        test_data = getCleanData(x_file = paste(data_path,"test/X_test.txt", sep=""), y_file = paste(data_path,"test/y_test.txt", sep=""), subj_file = paste(data_path,"test/subject_test.txt", sep=""), features, activity)
        train_data = getCleanData(x_file = paste(data_path,"train/X_train.txt", sep=""), y_file = paste(data_path,"train/y_train.txt", sep=""), subj_file = paste(data_path,"train/subject_train.txt", sep=""), features, activity)
        
        # Merge test and train data
        dat = rbind(test_data, train_data)
        
        # Aggregate data grouping by id_labels
        id_labels   = c("subject", "Activity_ID", "Activity_Label")
        data_labels = setdiff(colnames(dat), id_labels)
        redata      = melt(dat, id = id_labels, measure.vars = data_labels)
        # Apply mean function to dataset
        tidy_data   = dcast(redata, subject + Activity_Label ~ variable, mean)
        
        write.table(tidy_data, file = paste(data_path,"cleanDataSet.txt", sep=""))
}

# Execute processing 
processDataSet()