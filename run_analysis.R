library(stringi)    # read and parse data
library(dplyr)      # Work on the data

# read features
features.file= "C:/Users/Massimo/OneDrive/Documents/GitHub/Coursera/DataScience/CleanData/Project/UCI HAR Dataset/features.txt"
features= stri_read_lines(features.file)
# change the format  of features from: "NUM Label"  to "Label" since the number is not informative
features=  sub(pattern = "[0-9]* ",replacement = "", x = features)

data.set.path="C:/Users/Massimo/OneDrive/Documents/GitHub/Coursera/DataScience/CleanData/Project/UCI HAR Dataset"

test.data.set.name= "test"
train.data.set.name= "train"

## read  datta
read.data <- function (label, path=data.set.path) {
  # set all files names right
  file.X= paste("X_",label,".txt",sep="")
  file.Y= paste("Y_",label,".txt",sep="")
  file.subject= paste("subject_",label,".txt",sep="")
  #
  # read all files
  lapply(c(file.X, file.Y, file.subject),
         function(file.name) {
           # get a file
           file=  paste(data.set.path,label,file.name,sep="/")
           # read the file
           stri_read_lines(file)
         })
}

clean.data  <- function (data.raw,features) {
  
  ## clean activities list
  ## q3:Uses descriptive activity names to name the activities in the data set
  activities= lapply(data.raw[[2]],
                     function(activity) {
                       switch(activity,
                              "1"= "WALKING",
                              "2"= "WALKING_UPSTAIRS",
                              "3"= "WALKING_DOWNSTAIRS",
                              "4"= "SITTING",
                              "5"= "STANDING",
                              "6"= "LAYING")
                     })
  
  # Parsing X_test
  
  ## Extract data points ..................................
  # split at blanks
  data.rows= unlist(data.raw[[1]])
  data.rows= sapply(data.rows,   #data.rows, 
                    function(data) {
                      stri_split(str = data,regex = " ")
                    })
  # remove empty elements
  clean.data.rows=c()
  for(data in data.rows) {
    nums= as.numeric(data)  # find all blanks
    nums=  na.omit(nums)
    clean.data.rows= c(clean.data.rows, nums) #  add all number found
  }
  # generate a matrix of all numbers
  data.matrix= matrix(data = clean.data.rows,
                      nrow = length(activities),
                      byrow = T)
  
  ## configure the  data table ........................
  # data table with all data
  df= data.frame(user=unlist(data.raw[[3]]), 
                   activity=unlist(activities), 
                   data.matrix)
  # use correct  col  names
  ## q4:Appropriately labels the data set with descriptive variable names.
  names(df)= c("user", "activities", features)
  # return the data frame
  df
}

# get test data
test.data= clean.data(read.data("test"),features) 
# get train data
train.data= clean.data(read.data("train"),features) 

##  ------------------------- COMPLETE Q1: 
## Merges the training and the test sets to create one data set.

# merge data
data.q1= rbind(test.data,train.data)

##  ------------------------- COMPLETE Q2: 
## Extracts only the measurements on the mean and standard deviation for each measurement. 

# the task  requires to deal with only mean and std (standard deviation) data
# features.toSelect contains the numeration of those rows
features.toSelect= grep(pattern = "mean|std",x = features)
# apply to data: keep user and activity plus all  means and stds
data.q2= data.q1[c(1,2, features.toSelect+2)]

##  ------------------------- COMPLETE Q3:
## Uses descriptive activity names to name the activities in the data set

# done  above while constucting the data frame

##  ------------------------- COMPLETE Q4:
## Appropriately labels the data set with descriptive variable names.

# done  above while constucting the data frame

##  ------------------------- COMPLETE Q5:
## From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.

# compute the requested summary on the  bases of both user namer and activity
data.q5= data.q2 %>% 
  group_by(user ,activities) %>% 
  summarise_each(funs(mean))

# write the file
write.table(data.q5,
            "C:/Users/Massimo/OneDrive/Documents/GitHub/Coursera/DataScience/CleanData/Project/data_q5.txt",
            row.name=FALSE)
