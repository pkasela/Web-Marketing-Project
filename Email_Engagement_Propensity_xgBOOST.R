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

setwd("/home/pranav/Desktop/Web-Marketing-Project/datasets/")
df_master <- read.csv2("df_master.csv")

train_index <- createDataPartition(df_master$TARGET, 
                                   p = .60, 
                                   list = FALSE, 
                                   times = 1)

#elimino gli indici nella prima colonna che sono ID
# e gli altri che sono o categorici (si potrebbe fare one hot encoding oppure
#label encoding, ma in questo caso preferiamo toglierle
total <- df_master[,-c(1,7,8,9,10,11,14,20,21,22,23)]

train_set <- total[train_index,]
test_set  <- total[-train_index,]

#dopo diversi tentativi questo è il migliore
xgb <- xgboost(data = as.matrix(train_set[,-1]), 
               label = train_set[,'TARGET'], 
               max.depth = 15, eta = 0.1, nthread = 4, nrounds = 100,
               objective = "binary:logistic"
)

#esiste un modo per il booster che è lineare quindi molto più rapido ma meno preciso
xgb2 <- xgboost(data = as.matrix(train_set[,-1]), 
               label = train_set[,'TARGET'], 
               max.depth = 20, booster = "gblinear", nthread = 4, nrounds = 50,
               objective = "binary:logistic"
)

y_pred <- predict(xgb, as.matrix(test_set[,-1]))
pred <- as.factor(as.numeric(y_pred > 0.5))
recall(pred,as.factor(test_set[,1]),relevant = '1')
precision(pred,as.factor(test_set[,1]),relevant = '1')
F1_Score(pred,as.factor(test_set[,1]),positive = '1')
Accuracy(pred,test_set[,1])
ROC1 <- performance(prediction(y_pred,test_set$TARGET), 'tpr', 'fpr')
ROC_XG1 <- data.frame(x=ROC1@x.values[[1]], y=ROC1@y.values[[1]])
AUC_XG1 <- round(performance(prediction(y_pred, 
                                      test_set$TARGET),'auc')@y.values[[1]],3)

y_pred_2 <- predict(xgb2, as.matrix(test_set[,-1]))
pred_2 <- as.factor(as.numeric(y_pred > 0.5))
recall(pred_2,as.factor(test_set[,1]),relevant = '1')
precision(pred_2,as.factor(test_set[,1]),relevant = '1')
F1_Score(pred_2,as.factor(test_set[,1]),positive = '1')
Accuracy(pred_2,test_set[,1])
ROC2 <- performance(prediction(y_pred_2,test_set$TARGET), 'tpr', 'fpr')
ROC_XG2 <- data.frame(x=ROC2@x.values[[1]], y=ROC2@y.values[[1]])
AUC_XG2 <- round(performance(prediction(y_pred_2, 
                                        test_set$TARGET),'auc')@y.values[[1]],3)

ggplot() + 
  geom_line(data = ROC_XG1,aes(x,y,col="A")) +
  xlab('False Positive Rate') + ylab('True Positive Rate') +
  geom_line(data=ROC_XG2,aes(x,y,col="B")) +
  scale_colour_manual(name = "Models:", values = c("A"="red","B"="blue"),
                      labels = c("xgBoost 1", "xgBoost 2")) +
  annotate("text", x=0.6, y=0, 
           label= paste0("AUC XG1 = ",AUC_XG2),
           col="blue") +
  annotate("text", x=0.6, y=0.05, 
           label= paste0("AUC XG2 = ",AUC_XG1),
           col="red")
