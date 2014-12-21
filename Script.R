setwd("/Users/liang.huang/Documents/R/Coursera/machine learning/")

library(caret)
data<-read.csv("pml-training.csv", na.strings=c("NA",""))
testing <- read.csv("pml-testing.csv", na.strings=c("NA",""))
dim(data)
dim(testing)
# create training set and validation set
set.seed(13398)
inTraining<-createDataPartition(data$classe, p=0.7, list=FALSE)
training<-data[inTraining,]
validation<-data[-inTraining,]

# remove columns with all N/A
training<-training[,colSums(is.na(training)) == 0]
training<-training[,-c(1:7)]
model<-randomForest(classe~.,data=training)

# model results
table(predict(model,validation),validation$classe)
confusionMatrix(validation$classe,predict(model,validation))

model<-train(classe~.,method="rf",data=training)
confusionMatrix(validation$classe,predict(model,validation))
varImp(model)


training[,c(1:10),with=FALSE]
dim(training[,colSums(is.na(training)) < nrow(training),with=FALSE])
dim(training[, (colSums(is.na(training))==0),with=FALSE])
colSums(is.na(training))
colSums(is.na(training))
dim(training)
training$var_total_accel_belt
head(training$user)
