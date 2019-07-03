library(caret)
library(randomForest)
library(MLmetrics)
library(rpart)
library(ROSE)

set.seed(12345)

setwd("/home/pranav/Desktop/web marketing/datasets/")
df_master <- read.csv2("df_master.csv")

train_index <- createDataPartition(df_master$TARGET, 
                                   p = .60, 
                                   list = FALSE, 
                                   times = 1)
df_master[,'TARGET'] <- as.factor(df_master[,'TARGET'])


#elimino gli indici nella prima colonna che sono ID
# e le colonne ripetitive

total_rf <- df_master[,-c(1,7,8,9,10,21)]

train_set_rf <- total_rf[train_index,]
#train_set_rf <- SMOTE(TARGET ~ ., train_set_rf, k=3,
#                      perc.over = 100, perc.under=100) #toppo lento (toppe prove)
#train_set_ROSE <- ROSE(TARGET~.,train_set_rf)$data #un altro possibile metodo

#Questo è quello che ha funzionato "meglio"
#oversampling e undersampling per equilibrare le classi
train_set_rf <- ovun.sample(TARGET~.,train_set_rf,method="both",p=0.5)$data 
test_set_rf  <- total_rf[-train_index,]

rf_mdl = randomForest(x=train_set_rf[,-1],y=train_set_rf[,'TARGET'],
                     ntree=100, trControl = trainControl(method="none"),
                     keep.forest=TRUE,importance=TRUE)

rf_mdl_2 = randomForest(x=train_set_rf[,-1],y=train_set_rf[,'TARGET'],
                      ntree=100, trControl = trainControl(),
                      keep.forest=TRUE,importance=TRUE)


#print(rf_mdl)
pred <- predict(rf_mdl, test_set_rf[,-1])
confusionMatrix(pred,test_set_rf[,1],positive='1')
recall(pred,test_set_rf[,1],relevant = '1')
precision(pred,test_set_rf[,1],relevant = '1')
F1_Score(pred,test_set_rf[,1],positive = '1')
Accuracy(pred,test_set_rf[,1])

pred_2 <- predict(rf_mdl_2, test_set_rf[,-1])
confusionMatrix(pred_2,test_set_rf[,1],positive='1')
recall(pred_2,test_set_rf[,1],relevant = '1')
precision(pred_2,test_set_rf[,1],relevant = '1')
F1_Score(pred_2,test_set_rf[,1],positive = '1')
Accuracy(pred_2,test_set_rf[,1])
