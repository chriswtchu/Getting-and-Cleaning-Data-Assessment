setwd("/Users/chrischu/Documents/Data Science/3. Getting and Cleaning Data/UCI HAR Dataset/")

# Input Data
features        <-  read.table("features.txt")
activity_label  <-  read.table("activity_labels.txt")

xtest           <-  read.table("test/X_test.txt")
ytest           <-  read.table("test/y_test.txt")
subjecct_test   <-  read.table("test/subject_test.txt")

xtrain          <-  read.table("train/X_train.txt")
ytrain          <-  read.table("train/y_train.txt")
subjecct_train  <-  read.table("train/subject_train.txt")

# Combine Data
allx            <-  rbind(xtest,xtrain)
ally            <-  rbind(ytest,ytrain)
allsubject      <-  rbind(subjecct_test,subjecct_train)

# Extract Data
meanstd         <-  grep("mean\\(\\)|std\\(\\)", features[, 2])
allx            <-  allx[, meanstd]
names(allx)     <-  gsub("\\(\\)", "", features[meanstd, 2])
names(allx)     <-  gsub("-", "", names(allx))

newlabel        <-  activity_label[ally[, 1], 2]
ally[, 1]       <-  newlabel
names(ally)     <-  "activity"

# Consolidate Data: 1st Dataset
names(allsubject) <- "subject"
dataone <- cbind(allsubject, ally, allx)
write.table(dataone, "dataone.txt")

# Consolidate Data: 2nd Dataset
datatwo <- matrix(NA, nrow=180, ncol=68) 
result <- as.data.frame(datatwo)
colnames(datatwo) <- colnames(dataone)

row <- 1
for(i in 1:30) {
    for(j in 1:6) {
        datatwo[row, 1] <- sort(unique(allsubject)[, 1])[i]
        datatwo[row, 2] <- activity_label[j, 2]
        bool1 <- i == dataone$subject
        bool2 <- activity_label[j, 2] == dataone$activity
        datatwo[row, 3:68] <- colMeans(dataone[bool1&bool2, 3:68])
        row <- row + 1
    }
}
write.table(datatwo, "datatwo.txt")
