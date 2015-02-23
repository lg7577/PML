#Loading required librariries
library(caret)
library(ggplot2)
library(rpart)
library(randomForest)

#Downloading files
training_url  <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testing_url  <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(url=training_url,destfile="./PML_C/training.csv")
download.file(url=testing_url,destfile="./PML_C/testing.csv")

#Reading files
training  <- read.csv("./PML_C/training.csv",na.strings=c("NA",""))
testing  <- read.csv("./PML_C/testing.csv",na.strings=c("NA",""))

#Removing NA values
count <- dim(training)[1]*0.9
indices <- which(colSums(is.na(training))>count)
training <- training[,-indices]

#Excluding prediction unrelated variables
bad  <- 1:7
training  <- training[,-bad]

#Deleting highly correlated variables
cor_matrix  <- cor(training[,-53])
hcor  <- findCorrelation(cor_matrix, cutoff = .80, verbose=FALSE)
training  <- training[,-hcor]

#Cross-validation (Random resampling x10)
n  <- 10
model1_acc  <- vector()
model2_acc  <- vector()
for(i in 1:n){
      inTrain  <- createDataPartition(training$classe, p = 0.7, list=FALSE)
      mytraining  <- training[inTrain,]
      mytesting  <- training[-inTrain,]
      
      model1 <- rpart(classe ~ ., data=mytraining)
      pred1  <- predict(model1,newdata=mytesting, type="class")
      acc1  <- confusionMatrix(pred1,mytesting$classe)$overall[1]
      model1_acc[i]  <- acc1
      
      model2 <- randomForest(classe ~. , data=mytraining)
      pred2  <- predict(model2,newdata=mytesting)
      acc2  <- confusionMatrix(pred2,mytesting$classe)$overall[1]
      model2_acc[i]  <- acc2
      
}
round((1 - mean(model1_acc)) * 100,1)
round((1 - mean(model2_acc)) * 100,1)

#Preprocesing th testing dataset the same way as training
testing  <- testing[,-indices]
testing  <- testing[,-bad]
testing  <- testing[,-hcor]

#Predicting final results
pred_final  <- predict(model2,newdata=testing)

#Saving prediction results in separate .txt files
pml_write_files = function(x){
      n = length(x)
      for(i in 1:n){
            filename = paste0("problem_id_",i,".txt")
            write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
      }
}

pml_write_files(pred_final)
