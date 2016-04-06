# ReadMe

This repository is for the **Coursera - Data Science - 03 Getting and Cleaning Data** Project.

### Files
Here is a list of the following files included in this repository:
* README.md - you are reading it, buddy!
* codebook.md - A brief instruction on data.frames and columns in the final result.
* features_info_mod.txt - Modified file with info regarding the data.
* run_analysis.R - Project R-script for getting and cleaning data.

### Analysis Files
Download the project data from the following URL:  
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

After extracting the data, set the location of the folder "UCI HAR Dataset" as the working directory in the R-script run_analysis.R and run it. It will produce the following two data.frames with tidy data:

**dataset**  
This data.frame contains the data from both the test and trial. The columns are Subject, Activity and one column for every mean/standard deviation.
    
**datamean**  
An aggregate table of dataset. Contains Subject and Activity, but now the mean of all the mean/standard deviation values. There is one row of means for every subject and activity (with a total of 180 rows).

### Description of R-script
The R-script starts by checking that the data is located in the working directory. The script does a simple check to see if the folder "UCI HAR Dataset" exists. If the folder is found, the script assumes all necessary files are present and starts reading in the required data.

The TEST and TRAIN data is combined through rbind() and we include information on subject and activity with a cbind(). The labels for the columns is read from the file **features.txt** and we join in the Activity type from the file **activity_labels.txt**. Regular expressions are used to identify all columns that contain the strings "mean()" and "std()" and the final data.frame **dataset** is a subset of the train/test data.

Finally the data.frame **datamean** is created by aggregating the table **dataset**: we take the mean of all measurements for subject and activity.

