DataCleansing
=============

Obtaining Raw Data, clean and arrange data


### Running the script

- Download the [data set](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip) and extract. It should result in a `UCI HAR Dataset` folder that has all the files in the required structure.
- Change current directory to the `UCI HAR Dataset` folder.
- Run `Rscript <path to>/run_analysis.R`
- The tidy dataset should get created in the current directory as `tidy.txt`

### Assumptions

- The training and test data are available in folders named `train` and `test` respectively.
- Measurements are present in `X_<dataset>.txt` file
- Subject information is present in `subject_<dataset>.txt` file
- Activity codes are present in `y_<dataset>.txt` file
- All activity codes and their labels are in a file named `activity_labels.txt`.
- Names of all measurements taken are present in file `features.txt` ordered and indexed as they appear in the `X_<dataset>.txt` files.
- All columns representing means contain `...mean()` in them.
- All columns representing standard deviations contain `...std()` in them.
