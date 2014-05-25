setwd("C:/Users/Pascal/Desktop/COURSEera/Data Science/Getting and Cleaning Data/project")
ytrain<-read.table("./Dataset/train/y_train.txt")
xtrain<-read.table("./Dataset/train/X_train.txt")
subtrain<-read.table("./Dataset/train/subject_train.txt")
ytest<-read.table("./Dataset/test/y_test.txt")
xtest<-read.table("./Dataset/test/X_test.txt")
subtest<-read.table("./Dataset/test/subject_test.txt")
feature<-read.table("./Dataset/features.txt")
names(xtest)<-feature[,2]
names(xtrain)<-feature[,2]
names(ytrain)<-"Label"
names(subtrain)<-"Subject"
xtrain<-cbind(subtrain,ytrain,xtrain)
xtrain$Experience_type<-"train"
names(ytest)<-"Label"
names(subtest)<-"Subject"
xtest<-cbind(subtest,ytest,xtest)
xtest$Experience_type<-"test"
names(xtrain)<-names(xtest)
mergedata<-rbind(xtrain,xtest)
grepstd<-grepl("std()",colnames(mergedata))
datastd<-mergedata[,grepstd]
grepm<-grepl("mean",colnames(mergedata))
datam<-mergedata[,grepm]
grepmfreq<-grepl("meanFreq",colnames(datam))
datafreq<-datam[,grepmfreq]
datamean<-setdiff(datam,datafreq)
MergeSTD_Mean<-cbind(datastd,datamean)
mergelabel<-rbind(ytrain,ytest)
names(mergelabel)<-"Label"
mergesubject<-rbind(subtrain,subtest)
names(mergesubject)<-"Subject"
MergeSTD_Mean<-cbind(mergesubject,mergelabel,datastd,datamean)
for (i in 1:length(MergeSTD_Mean$Label))
{
  if(MergeSTD_Mean$Label[i]=="1")
  {
    MergeSTD_Mean$Label[i]<-"WALKING"
  }
  
  if(MergeSTD_Mean$Label[i]=="2")
  {
    MergeSTD_Mean$Label[i]<-"WALKING_UPSTAIRS"
  }
  
  if(MergeSTD_Mean$Label[i]=="3")
  {
    MergeSTD_Mean$Label[i]<-"WALKING_DOWNSTAIRS"
  }
  
  if(MergeSTD_Mean$Label[i]=="4")
  {
    MergeSTD_Mean$Label[i]<-"SITTING"
  }
  
  if(MergeSTD_Mean$Label[i]=="5")
  {
    MergeSTD_Mean$Label[i]<-"STANDING "
  }
  
  if(MergeSTD_Mean$Label[i]=="6")
  {
    MergeSTD_Mean$Label[i]<-"LAYING"
  }
  
}

head(MergeSTD_Mean,20)



setwd("C:/Users/Pascal/Desktop/COURSEera/Data Science/Getting and Cleaning Data/project")
ytrain<-read.table("./Dataset/train/y_train.txt")
xtrain<-read.table("./Dataset/train/X_train.txt")
subtrain<-read.table("./Dataset/train/subject_train.txt")
ytest<-read.table("./Dataset/test/y_test.txt")
xtest<-read.table("./Dataset/test/X_test.txt")
subtest<-read.table("./Dataset/test/subject_test.txt")
feature<-read.table("./Dataset/features.txt")
names(ytrain)<-"Label"
names(xtrain)<-feature[,2]
names(subtrain)<-"Subject"
xtrain<-cbind(subtrain,ytrain,xtrain)
names(xtest)<-feature[,2]
names(ytest)<-"Label"
names(subtest)<-"Subject"
xtest<-cbind(subtest,ytest,xtest)
names(xtrain)<-names(xtest)
mergedata<-rbind(xtrain,xtest)
library(reshape2)
datamelt<-melt(mergedata,id=colnames(mergedata[,1:2]),measure.vars=colnames(mergedata[,3:300]))
datacast<-dcast(datamelt,Subject+Label~variable,mean)

for (i in 1:length(datacast$Label))
{
  if(datacast$Label[i]=="1")
  {
    datacast$Label[i]<-"WALKING"
  }
  
  if( datacast$Label[i]=="2")
  {
    datacast$Label[i]<-"WALKING_UPSTAIRS"
  }
  
  if( datacast$Label[i]=="3")
  {
    datacast$Label[i]<-"WALKING_DOWNSTAIRS"
  }
  
  if( datacast$Label[i]=="4")
  {
    datacast$Label[i]<-"SITTING"
  }
  
  if( datacast$Label[i]=="5")
  {
    datacast$Label[i]<-"STANDING "
  }
  
  if( datacast$Label[i]=="6")
  {
    datacast$Label[i]<-"LAYING"
  }
  
}

datacast