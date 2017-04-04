## read a single data set by type: 'train' or 'test'
read.activity_folder <- function(type) {
  library(data.table)
  library(dplyr)
  
  prefix <- file.path('./UCI HAR Dataset', type)
  
  # load X, features
  X <- fread(file.path(prefix, paste('X_', type, '.txt', sep='')))
  names(X) <- fread('./UCI HAR Dataset/features.txt')$V2
  # filter out the features with
  
  # load y, activity labels
  labels <- fread(file.path('./UCI HAR Dataset/activity_labels.txt'))
  y <- fread(file.path(prefix, paste('y_', type, '.txt', sep='')))
  names(y) <- 'activity'
  y <- mutate(y, activity = factor(labels$V2[activity]))
  
  # load subjects
  subject <- fread(file.path(prefix, paste('subject_', type, '.txt', sep='')))
  names(subject) <- 'subject'

  # bind everything together
  dt <- bind_cols(subject, y, X)
}

## Read both train and test activity data set from 'UCI HAR Dataset' folder.
read.activity_data <- function() {
  library(data.table)
  library(dplyr)
  
  dt <- bind_rows(read.activity_folder('train'), read.activity_folder('test')) %>%
    select(subject, activity, matches('(mean|std)\\(\\)'))
  
  # clean up names
  n <- names(dt)
  n <- gsub('^t', 'linear_', n)
  n <- gsub('^f', 'fourier_', n)
  n <- gsub('Body', 'body_', n)
  n <- gsub('Acc', 'acceleration_', n)
  n <- gsub('Gyro', 'gyro_', n)
  n <- gsub('Jerk', 'jerk_', n)
  n <- gsub('Mag', 'magnitude_', n)
  
  n <- gsub('-mean\\(\\)-([XYZ])', '\\1_mean', n)
  n <- gsub('-std\\(\\)-([XYZ])', '\\1_std', n)
  n <- gsub('-mean\\(\\)', 'mean', n)
  n <- gsub('-std\\(\\)', 'std', n)
  
  names(dt) <- n
  
  dt
}

summarize.activity_data <- function(dt) {
  library(dplyr)
  dt %>%
    group_by(subject, activity) %>%
    summarize_each(funs(mean))
}

# Create the data tables
result <- read.activity_data()
result_sum <- summarize.activity_data(result)

write.csv(result, './activity_data.csv', row.names = FALSE)
write.csv(result_sum, './activity_summary.csv', row.names = FALSE)
