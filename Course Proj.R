library(AppliedPredictiveModeling)
library(caret)
library(randomForest)


trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

training <- read.csv(url(trainUrl))
testing <- read.csv(url(testUrl))
setdiff(colnames(training),colnames(testing))

str(training)

training <- training[,8:160]
testing <- testing[,8:160]
training <- training[,colSums(is.na(training)) == 0]

nzvCol <- nearZeroVar(training,saveMetrics=T)
training <-training[,which(nzvCol$nzv=="FALSE")]

plot(training$classe,main="Frequency by Classe")

set.seed(1)

myTesting <- training[createDataPartition(y=training$classe, p=0.3, list=FALSE),]
myTraining <- training[-createDataPartition(y=training$classe, p=0.3, list=FALSE),]

?randomForest
model <- randomForest(classe~.,data=myTraining,method="Class")

predict <- predict(model,myTesting,type="class")

confusionMatrix(predict,myTesting$classe)

predict_test <- predict(model,testing,type="class")
predict_test

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predict_test)

getwd()

setwd("C:/Users/knobbe/Documents/PTO, Time Sheets, etc/Coursera/Data Science Specialization/8. Practical Machine Learning")
