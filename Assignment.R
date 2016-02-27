library(caret)
library(rpart)
library(Hmisc)

trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
pml.training <- read.csv(url(trainUrl), stringsAsFactors = FALSE)
pml.testing <- read.csv(url(testUrl), stringsAsFactors = FALSE)

pml.training<-read.csv("data/pml-training.csv", stringsAsFactors = FALSE)
pml.testing<-read.csv("data/pml-testing.csv", stringsAsFactors = FALSE)
table(pml.training$classe) 

#Data Cleaning
na_count <-sapply(pml.training, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
table(na_count) #67 columns have 19216 NAs
null_count <-sapply(pml.training, function(y) sum(length(which(y==""))))
null_count <- data.frame(null_count)
table(null_count) #33 columns have 19216 ""s

#select only columns that have complete data - 60 columns remaining
pml.training<-pml.training[,na_count==0 & null_count==0] 
pml.testing<-pml.testing[,na_count==0 & null_count==0]
pml.training<-pml.training[,8:60] #remove first 7 cols
pml.testing<-pml.testing[,8:60] #remove first 7 cols
testing <- pml.testing

#split training into training and validation 75/25
set.seed(3458)
inTrain <- createDataPartition(pml.training$classe, p = 0.75, list = FALSE)
training <- pml.training[inTrain,]
validation <- pml.training[-inTrain,]

#Random Forest: training
control <- trainControl(method = "cv", number = 5)
model_rf <- train(classe ~ ., data = training, method = "rf", trControl = control)
print(model_rf)

predict_rf_v <- predict(model_rf, validation)
# Show prediction result on validation set
matrix_rf_v <- confusionMatrix(validation$classe, predict_rf_v)
matrix_rf_v
matrix_rf_v$overall[1]

# Predict on test set
predict_rf_t <- predict(model_rf, testing)
predict_rf_t
