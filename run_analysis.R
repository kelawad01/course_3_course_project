#Course Project
#Khaid Elawad

fileurl <-
        "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile = "./galaxysdata.zip")
unzip("galaxysdata.zip")
activity_labels <-
        fread(
                file.path(getwd(), "UCI HAR Dataset/activity_labels.txt"),
                col.names = c("classlabel", "activity")
        )
features <-
        fread(
                file.path(getwd(), "UCI HAR Dataset/features.txt"),
                col.names = c("featurenumber", "featurename")
        )

#For #2: Extracts only the measurements on the mean and standard deviation for each measurement

isolatefeatures <- grep("mean|std", features[, featurename])

#For #4: Appropriately labels the data set with descriptive vairable names
selectfeatures <- features[isolatefeatures, featurename]

#Prepares the training data

trainingdata <-
        fread(file.path(getwd(), "UCI HAR Dataset/train/X_train.txt"))
trainingdata <- trainingdata[, isolatefeatures, with = FALSE]
data.table::setnames(trainingdata, colnames(trainingdata), selectfeatures)

trainingactivity <-
        fread(file.path(getwd(), "UCI HAR Dataset/train/Y_train.txt"),
              col.names = "Activity")

trainingsubjects <-
        fread(file.path(getwd(), "UCI HAR Dataset/train/subject_train.txt"),
              col.names = "Subject")

#Create a data table of all the components of the training data: subjects, activity, and the feature (training) data

training <- cbind(trainingsubjects, trainingactivity, trainingdata)

#Prepares the test data

testdata <-
        fread(file.path(getwd(), "UCI HAR Dataset/test/X_test.txt"))
testdata <- testdata[, isolatefeatures, with = FALSE]
data.table::setnames(testdata, colnames(testdata), selectfeatures)

testactivity <-
        fread(file.path(getwd(), "UCI HAR Dataset/test/y_test.txt"),
              col.names = "Activity")

testsubjects <-
        fread(file.path(getwd(), "UCI HAR Dataset/test/subject_test.txt"),
              col.names = "Subject")

#Like before we create a test data table this time with the components: subject, activity and feature (test) data

test <- cbind(testsubjects, testactivity, testdata)

#For #1: Merge the training and the test sets to create one data set

mergeddata <- rbind(training, test)

#For #3: Use descriptive activity names to name the activities in the data set
library(reshape2)

mergeddata[["Activity"]] <-
        factor(mergeddata[, Activity], levels = activity_labels[["classlabel"]], labels = activity_labels[["activity"]])
mergeddata[["Subject"]] <- as.factor(mergeddata[, Subject])
mergeddatamelt <-
        reshape2::melt(data = mergeddata, id = c("Subject", "Activity"))
mergeddata <-
        reshape2::dcast(data = mergeddatamelt,
                        Subject + Activity ~ variable,
                        fun.aggregate = mean)

#Writing the new data table containing the reshaped/merged training and test data sets to a new file

write.table(mergeddata, "mergedataset.txt", row.names = FALSE)
