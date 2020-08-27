library(caret) 
#install.packages("rpart")
library(rpart) 
library(e1071)
library(DMwR)
library(PRROC)
library(randomForest)
library(dplyr)

rm(list=ls())
#normalized_multi_data_agnes.csv,normalized_multi_data_dbscan.csv,normalized_multi_data_kmed.csv
#normalized_multi_data_hap.csv

setwd("C:/Users/MURTHY/Desktop/project")
#clusteredData = read.csv("normalized_multi_indoor_datakmeans.csv", header=T)
#clusteredData = read.csv("normalized_multi_data_agnes.csv", header=T) #2 anomaly, 1 normal

clusteredData = read.csv("normalized_multi_data_dbscan.csv", header=T) #0 anomaly, 1 normal

#clusteredData = read.csv("normalized_multi_data_kmed.csv", header=T) #2 anomaly, 1 normal
#clusteredData = read.csv("normalized_multi_data_hap.csv", header=T)

clusteredData$V6
clusteredData<-select(clusteredData,-c(3))

table(clusteredData$V6) ## "1" = "anomaly" class And "2" = "normal"

#remove the id
clusteredData <- clusteredData[,-1]


str(clusteredData)

#convert the label to factor
clusteredData$V6<- factor(clusteredData$V6)
clusteredData$V6

#SMOTE the data

set.seed(10)
smoteData <- SMOTE(V6 ~., clusteredData, perc.over=100, perc.under=600)
table(smoteData$V6)

#evaluation metrics 
fpr <- rep(NA, len=5)
fm <- rep(NA, len=5)
pri <- rep(NA, len=5)
pr_c <- rep(NA, len=5)
Sensitivity <- rep(NA, len=5)
seed <- c(10,100,1000,2000,3500)

#train the model 
for (i in 1:5) {
  set.seed(seed[i])
  n <- nrow(smoteData)
  # Shuffle the dataset, call the result shuffled
  shuffled <- smoteData[sample(n),]
  validation_index <- createDataPartition(shuffled$V6, p = 0.7, list = FALSE)
  train <- shuffled[validation_index ,]
  test<- shuffled[-validation_index ,]
  
  # train
  #fit <- rpart(V6 ~ . ,train,  method="class")
  fit<-svm(V6 ~ . ,data=train,gamma=0.02,nu=0.5)
  
  #fit<- randomForest(V6 ~ . ,data=train)
  
  print(fit)
  # Predict 
  #predicted<- predict(fit,test[,1:2], type="class") # give me predict calss label
  predicted<- predict(fit,test[,1:2])
  predicted
  conf.mat<- table(Predicted_class = predicted,
                   Actual_class = test$V6) [2:1,2:1]
  conf.mat
  results <- confusionMatrix(conf.mat)
  print(conf.mat)
  fpr[i] <- 1-specificity(conf.mat)
  fm[i] <- results$byClass['F1']
  pri[i]<- precision(conf.mat)
  Sensitivity[i] <- sensitivity(conf.mat)
  
  
  
  
  
  
  
  
}

#results

cat("False Positive Rate : Mean",mean(fpr)) 


cat("F-measure :",mean (fm)) 

cat("Sensitivity :",mean (Sensitivity)) 

cat("Precision :",mean(pri)) 

cat("The area under the precision-recall curve (AUCPR) :",mean(pr_c)) 
