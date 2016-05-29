#Executive Summary & Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.

The goal of this project is to predict the manner in which they did the exercise. This is the “classe” variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

About the Data
The data for this project are available here: . Training dataset : “https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv” . Testing dataset: “https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv” The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment. So let’s have a look on the dataset and on the classe variable.

##Data Partioning

> library(elasticnet)
> plot.enet(mod_lasso$finalModel, xvar="penalty", use.color = TRUE)
> set.seed(325)
> library(e1071)
> library(caret)
> ptrain<-read.csv("pml-training.csv")
> ptest<-read.csv("pml-testing.csv")
> set.seed(10)
> inTrain<-createDataPartition(y=ptrain$classe, p=0.7, list=F)
> ptrain1<-ptrain[inTrain,]
> ptrain2<-ptrain[-inTrain,]
> nzv<-nearZeroVar(ptrain1)
> ptrain1<-ptrain1[,-nzv]
> ptrain2<-ptrain2[,-nzv]
> mostlyNA<-sapply(ptrain1, function(x)mean(is.na(x)))>0.95
> ptrain1<-ptrain1[,mostlyNA==F]
> ptrain2<-ptrain2[,mostlyNA==F]
> ptrain1<-ptrain1[,-(1:5)]
> ptrain2<-ptrain2[,-(1:5)]

##Model building

fitControl<-trainControl(method="cv", number=3, verboseIter=F)
> fit<- train(classe~ ., data=ptrain1, method="rf", trControl=fitControl
> fit$finalModel

Call:
 randomForest(x = x, y = y, mtry = param$mtry) 
               Type of random forest: classification
                     Number of trees: 500
No. of variables tried at each split: 27

        OOB estimate of  error rate: 0.26%
Confusion matrix:
     A    B    C    D    E  class.error
A 3905    0    0    0    1 0.0002560164
B    7 2647    3    1    0 0.0041384500
C    0    7 2389    0    0 0.0029215359
D    0    0    9 2242    1 0.0044404973
E    0    0    0    7 2518 0.0027722772
> preds<- predict(fit, newdata=ptrain2)
> confusionMatrix(ptrain2$calsse, preds)

nzv <- nearZeroVar(ptrain)
> ptrain <- ptrain[, -nzv]
> ptest <- ptest[, -nzv]
> 
> mostlyNA <- sapply(ptrain, function(x) mean(is.na(x))) > 0.95
> ptrain <- ptrain[, mostlyNA==F]
> ptest <- ptest[, mostlyNA==F]
> ptrain <- ptrain[, -(1:5)]
> ptest <- ptest[, -(1:5)]
> 
> fitControl <- trainControl(method="cv", number=3, verboseIter=F)
> fit <- train(classe ~ ., data=ptrain, method="rf", trControl=fitControl)
> preds<-predict(fit,newdata=ptest)
> preds<-as.character(preds)

## Generating Files to answer for the assignment questions
> pm1_write_files<-function(x){ n<- length(x)
 for(i in 1:n){
  filename<- paste0("problem_id_", i, ".txt")
  write.table(x[i], file=filename, quote=F, row.names=F, col.names=F)
  }
 }

