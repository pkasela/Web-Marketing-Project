library(caret)
library(randomForest)
library(MLmetrics)
library(rpart)
library(ROSE)
library(gbm)
library(xgboost)
library(ROCR)
library(ggplot2)

set.seed(12345)

setwd("/home/pranav/Desktop/web marketing/datasets/")
df_master <- read.csv2("df_master.csv")

train_index <- createDataPartition(df_master$TARGET, 
                                   p = .60, 
                                   list = FALSE, 
                                   times = 1)
#df_master[,'TARGET'] <- as.factor(df_master[,'TARGET'])

#elimino gli indici nella prima colonna che sono ID
total <- df_master[,-c(1,7,8,9,10,11,14,20,21,22,23)]

train_set <- total[train_index,]
test_set  <- total[-train_index,]

#dopo diversi tentativi questo è il migliore
xgb <- xgboost(data = as.matrix(train_set[,-1]), 
               label = train_set[,'TARGET'], 
               max.depth = 15, eta = 0.1, nthread = 4, nrounds = 100,
               objective = "binary:logistic"
)

#esiste un modo
xgb2 <- xgboost(data = as.matrix(train_set[,-1]), 
               label = train_set[,'TARGET'], 
               max.depth = 20, booster = "gblinear", nthread = 4, nrounds = 50,
               objective = "binary:logistic"
)

y_pred <- predict(xgb, as.matrix(test_set[,-1]))
pred <- as.factor(as.numeric(y_pred > 0.5))
recall(as.factor(test_set[,1]),pred,relevant = '1')
precision(as.factor(test_set[,1]),pred,relevant = '1')
F1_Score(as.factor(test_set[,1]),pred,positive = '1')
Accuracy(pred,test_set[,1])
ROC1 <- performance(prediction(y_pred,test_set$TARGET), 'tpr', 'fpr')
ROC_DF1 <- data.frame(x=ROC1@x.values[[1]], y=ROC1@y.values[[1]])

y_pred_2 <- predict(xgb, as.matrix(test_set[,-1]))
pred_2 <- as.factor(as.numeric(y_pred_2 > 0.5))
recall(as.factor(test_set[,1]),pred_2,relevant = '1')
precision(as.factor(test_set[,1]),pred_2,relevant = '1')
F1_Score(as.factor(test_set[,1]),pred_2,positive = '1')
Accuracy(pred_2,test_set[,1])
ROC2 <- performance(prediction(y_pred_2,test_set$TARGET), 'tpr', 'fpr')
ROC_DF2 <- data.frame(x=ROC2@x.values[[1]], y=ROC2@y.values[[1]])

ggplot() + 
  geom_line(data = ROC_DF1,aes(x,y,col="A")) +
  xlab('False Positive Rate') + ylab('True Positive Rate') +
  geom_line(data=ROC_DF2,aes(x,y,col="B")) +
  scale_colour_manual(name = "Models:", values = c("A"="red","B"="blue"),
                      labels = c("xgBoost 1", "xgBoost 2")) 
