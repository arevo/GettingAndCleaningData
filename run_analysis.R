run_analysis <- function(directory){
  ## Get all filenames to download
  subject_test_filename <- paste(directory, "/test/subject_test.txt", sep="")
  x_features_test_filename <- paste(directory, "/test/X_test.txt", sep="")
  y_activity_test_filename <- paste(directory, "/test/y_test.txt", sep="")
  
  subject_train_filename <- paste(directory, "/train/subject_train.txt", sep="")
  x_features_train_filename <- paste(directory, "/train/X_train.txt", sep="")
  y_activity_train_filename <- paste(directory, "/train/y_train.txt", sep="")
  ## Download tables
  subject_test <- read.table(subject_test_filename)
  x_features_test <- read.table(x_features_test_filename)
  y_activity_test <- read.table(y_activity_test_filename)
  
  subject_train <- read.table(subject_train_filename)
  x_features_train <- read.table(x_features_train_filename)
  y_activity_train <- read.table(y_activity_train_filename)
  
  features_list <- paste(directory, "/features.txt", sep="")
  features <- read.table(features_list)
  
  ## Combine tables
  combined_features <- rbind(x_features_test, x_features_train)
  combined_subjects <- rbind(subject_test, subject_train)
  combined_activity <- rbind(y_activity_test, y_activity_train)
 
  ## Get index of columns corresponding to means and std. deviations
  column_labels <- as.character()
  
  extract_row_index <- as.numeric()
  for(i in 1:nrow(features)){
    if(length(grep("mean()",features[i,2]))>0) {
      extract_row_index <- c(extract_row_index, i)
      column_labels <- c(column_labels,as.character(features[i,2]))
    }
    if(length(grep("std()",features[i,2]))>0) {
      extract_row_index <- c(extract_row_index, i)
      column_labels <- c(column_labels,as.character(features[i,2]))
    }
  }
  
  ## Extract mean and std deviations
  extract_features <- combined_features[,extract_row_index]
  extract_features <- cbind(extract_features, combined_subjects[,1])
  ## Uses descriptive activity names to name the activities in the data set
  descriptive_features <- as.character()
  
  for(i in 1:nrow(combined_activity)){
    if(combined_activity[i,1]==1){
      descriptive_features[i] = "WALKING"
    }
    if(combined_activity[i,1]==2){
      descriptive_features[i] = "WALKING_UPSTAIRS"
    }
    if(combined_activity[i,1]==3){
      descriptive_features[i] = "WALKING_DOWNSTAIRS"
    }
    if(combined_activity[i,1]==4){
      descriptive_features[i] = "SITTING"
    }
    if(combined_activity[i,1]==5){
      descriptive_features[i] = "STANDING"
    }
    if(combined_activity[i,1]==6){
      descriptive_features[i] = "LAYING"
    }
  }
  
  extract_features <- cbind(extract_features,descriptive_features)
  
  ## Appropriately labels the data set with descriptive variable names.
  column_labels <- c(column_labels,"Subject_by_index","Activity_description")
  #print(column_labels)
  #print(ncol(extract_features))
  colnames(extract_features)<- column_labels
  
  ## Export as txt
  write.table(extract_features, file="tidy.txt", row.names = FALSE)
}

