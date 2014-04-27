
# read files from "test", and data file names suffixed with "test"
loadTestData <- function() 
{
    loadDataFromFiles("test", "test")
}

# read files from "train", and data file names suffixed with "train"
loadTrainData <- function() 
{
    loadDataFromFiles("train", "train")
}

# Add the activity names as another column
addLabels <- function(data) 
{
    activity_labels <- read.table("activity_labels.txt", header=F, as.is=T, col.names=c("ActivityID", "ActivityName"))
    activity_labels$ActivityName <- as.factor(activity_labels$ActivityName)
    data_labeled <- merge(data, activity_labels)
    data_labeled
}

# Merge both train and test data sets and improve column names
mergeRawData <- function() 
{
    data <- rbind(loadTestData(), loadTrainData())
    cnames <- colnames(data)
    cnames <- gsub("\\.+mean\\.+", cnames, replacement="Mean")
    cnames <- gsub("\\.+std\\.+",  cnames, replacement="Std")
    colnames(data) <- cnames
    data
}

# This function will encapsulate all logic to merge the data
# This will merge data from training and test data sets 
# And then applies activity labels as another column
loadMergedAndLabeledData <- function() 
{
    addLabels(mergeRawData())
}

# Main function to encapsulate the data merging and cleansing
# fname represents the name of the output file
generateTidyFile <- function(fname) 
{
	# Call methods which encapsulate the data merging step
	# push that merged data into a cleaning method
	mergedData <- loadMergedAndLabeledData()
	
	# Use the labeled data to obtain the final tidied set
     final_data <- loadTidiedData(mergedData)
     
     # Write the output to the final file on disk
     write.table(final_data, fname)
}

# This function will generate a tidied data set 
# It will contain the average of each variable 
# for each activity and each subject.
loadTidiedData <- function(merged_data) 
{
	# load the support library
    library(reshape2)
    
    # melt the dataset
    id_vars = c("ActivityID", "ActivityName", "SubjectID")
    # columns we will be working with
    measure_vars = setdiff(colnames(merged_data), id_vars)
    # new combining
    melted_data <- melt(merged_data, id=id_vars, measure.vars=measure_vars)
    
    # recast 
    dcast(melted_data, ActivityName + SubjectID ~ variable, mean)    
}


# Returns one data set by reading and merging all component files.
# Data set comprises of the X values, Y values and Subject IDs.
# The path_prefix indicates the path where the data files can be found.
# The fname_suffix indicates the file name suffix to be used to create the complete file name.
#
# This also subsets the data to extract only the measurements on the mean and standard deviation for each measurement.
# The required columns in the subset is determined by selecting only those columns that have either "mean()" or "std()" in their names.
# Subsetting is done early on to help reduce memory requirements.
loadDataFromFiles <- function(fname_suffix, path_prefix) 
{
	# obtain the raw files, by generating the path pattern
    fpath <- file.path(path_prefix, paste0("y_", fname_suffix, ".txt"))
    # read the data into a frame
    y_data <- read.table(fpath, header=F, col.names=c("ActivityID"))
    
    # Do the same with the subject data
    fpath <- file.path(path_prefix, paste0("subject_", fname_suffix, ".txt"))
    # load it into a data frame
    subject_data <- read.table(fpath, header=F, col.names=c("SubjectID"))
    
    # Load the Column Descriptions
    data_cols <- read.table("features.txt", header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))
    
    # read the X data file
    fpath <- file.path(path_prefix, paste0("X_", fname_suffix, ".txt"))
    data <- read.table(fpath, header=F, col.names=data_cols$MeasureName)
    
    # names of subset columns required
    subset_data_cols <- grep(".*mean\\(\\)|.*std\\(\\)", data_cols$MeasureName)
    
    # subset the data (done early to save memory)
    data <- data[,subset_data_cols]
    
    # append the activity id and subject id columns
    data$ActivityID <- y_data$ActivityID
    data$SubjectID <- subject_data$SubjectID
    
    # return the data
    data
}

print("This script will read data from files located under UCI HAR Dataset folder obtained from downloading data from:")
print("    https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")
print("    The desciprtion of the above data set is obtained from: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones")
print("Generating the cleaned output...")
generateTidyFile("tidy.txt")
print("Final output generated.")
