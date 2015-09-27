run_analysis <- function () {
  #library
  require(data.table)
  require(bit64)
  require(dplyr)
  
  ###-----------------------------------------------------------------------
  ###  1. Merges the training and the test sets to create one data set.
  ###-----------------------------------------------------------------------
  ###common files
  features <- fread(".\\UCI HAR Dataset\\features.txt", data.table = FALSE)
  ### TEST  read all the datasets. assume files are at working directory
  test_subj <- fread(".\\UCI HAR Dataset\\test\\subject_test.txt", data.table = FALSE)
  test_actv <- fread(".\\UCI HAR Dataset\\test\\y_test.txt", data.table = FALSE)
  test_data <- fread(".\\UCI HAR Dataset\\test\\x_test.txt", data.table = FALSE)
  # put name in columns
  names(test_data) <- features[,2]
  names(test_subj) <- "subject"
  names(test_actv) <- "activity"
  # aggregate test data frame
  test <- bind_cols(test_subj,test_actv) %>% mutate(type="test") %>% bind_cols(test_data)

  ### TRAIN  read all the datasets. assume files are at working directory
  train_subj <- fread(".\\UCI HAR Dataset\\train\\subject_train.txt", data.table = FALSE)
  train_actv <- fread(".\\UCI HAR Dataset\\train\\y_train.txt", data.table = FALSE)
  train_data <- fread(".\\UCI HAR Dataset\\train\\x_train.txt", data.table = FALSE)
  # put name in columns
  names(train_data) <- features[,2]
  names(train_subj) <- "subject"
  names(train_actv) <- "activity"
  # aggregate train data frame
  train <- bind_cols(train_subj,train_actv) %>% mutate(type="train") %>% bind_cols(train_data)
  ## merged data test and train
  dt <- bind_rows(test,train)   

  ###-----------------------------------------------------------------------
  ###  2. Extracts only the measurements on the mean and standard deviation for each measurement
  ###-----------------------------------------------------------------------
  dt_mean <- dt %>% select(contains("mean",ignore.case=TRUE))
  dt_std  <- dt %>% select(contains("std",ignore.case=TRUE))
  dt2  <- bind_cols(dt[,1:3],dt_mean,dt_std)  
  
  ###-----------------------------------------------------------------------
  ###  3. Uses descriptive activity names to name the activities in the data set
  ###-----------------------------------------------------------------------
  activities <- features <- fread(".\\UCI HAR Dataset\\activity_labels.txt", data.table = FALSE)
  dt3 <- dt2
  for (i in 1:nrow(dt3)) {
    v <- as.numeric(dt3[i,2])
    dt3[i,2] <- activities[v,2]
  }

  ###-----------------------------------------------------------------------
  ###  4. Appropriately labels the data set with descriptive variable names. 
  ###-----------------------------------------------------------------------  
  dt4 <- dt3
  n <- names(dt4)
  n <- gsub("tBody","Time.Body.",n)
  n <- gsub("tGravity","Time.Gravity.",n)  
  n <- gsub("fBody","Frecuency.Body.",n)
  n <- gsub("Acc","Accelerometer.",n)
  n <- gsub("Body.Body","Body.",n)
  n <- gsub("Gyro","Gyroscope.",n)
  n <- gsub("Mag-","Magnitude.",n)
  n <- gsub("Jerk-","Jerk.",n)
  n <- gsub("JerkMagnitude","Jerk.Magnitude",n)
  names(dt4) <- n
  
  ###-----------------------------------------------------------------------
  ###  5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  ###-----------------------------------------------------------------------
  dt5 <- dt4 %>% 
    mutate(subject = as.factor(subject)) %>% 
    mutate(activity = as.factor(activity)) %>%
    select(-matches("type")) %>%
    group_by(activity,subject) %>%
    summarise_each(funs(mean))
  
  write.table(dt5,".\\analized.txt",row.name = FALSE)
}