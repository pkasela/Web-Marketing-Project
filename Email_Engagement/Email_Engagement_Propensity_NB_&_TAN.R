library(caret)
library(randomForest)
library(MLmetrics)
library(rpart)
library(ROSE)
library(bnlearn)
library(ROCR)

set.seed(12345)

df_master <- read.csv2("datasets/df_master.csv")

train_index <- createDataPartition(df_master$TARGET, 
                                   p = .60, 
                                   list = FALSE, 
                                   times = 1)
df_master_trees <- df_master[,-c(1,7,8,9,10,17,21)]
df_master_trees[,'TARGET'] <- as.factor(df_master_trees[,'TARGET'])
df_master_trees[,'NUM_SEND_PREV'] <- as.factor(df_master_trees[,'NUM_SEND_PREV'])
df_master_trees[,'NUM_OPEN_PREV'] <- as.factor(df_master_trees[,'NUM_OPEN_PREV'])
df_master_trees[,'NUM_CLICK_PREV'] <- as.factor(df_master_trees[,'NUM_CLICK_PREV'])
df_master_trees[,'NUM_FAIL_PREV'] <- as.factor(df_master_trees[,'NUM_FAIL_PREV'])
df_master_trees[,'SEND_WEEKDAY'] <- as.factor(df_master_trees[,'SEND_WEEKDAY'])
df_master_trees[,'ID_NEG'] <- as.factor(df_master_trees[,'ID_NEG'])
df_master_trees[,'TYP_CLI_FID'] <- as.factor(df_master_trees[,'TYP_CLI_FID'])
df_master_trees[,'COD_FID'] <- as.factor(df_master_trees[,'COD_FID'])
df_master_trees[,'STATUS_FID'] <- as.factor(df_master_trees[,'STATUS_FID'])
df_master_trees[,'NUM_FIDs'] <- as.factor(df_master_trees[,'NUM_FIDs'])
df_master_trees[,'W_PHONE'] <- as.factor(df_master_trees[,'W_PHONE'])
df_master_trees[,'TYP_CLI_ACCOUNT'] <- as.factor(df_master_trees[,'TYP_CLI_ACCOUNT'])
df_master_trees[,'EMAIL_PROVIDER_CLEAN'] <- as.factor(df_master_trees[,'EMAIL_PROVIDER_CLEAN'])
df_master_trees[,'REGION'] <- as.factor(df_master_trees[,'REGION'])
df_master_trees[,'FLAG_PRIVACY_1'] <- as.factor(df_master_trees[,'FLAG_PRIVACY_1'])
df_master_trees[,'FLAG_PRIVACY_2'] <- as.factor(df_master_trees[,'FLAG_PRIVACY_2'])
df_master_trees[,'FLAG_DIRECT_MKT'] <- as.factor(df_master_trees[,'FLAG_DIRECT_MKT'])
str(df_master_trees)

#elimino gli indici nella prima colonna che sono ID
# e le colonne ripetitive

train_set_rf <- df_master_trees[train_index,]
#train_set_rf <- SMOTE(TARGET ~ ., train_set_rf, k=3,
#                      perc.over = 100, perc.under=100) #toppo lento (toppe prove)
#train_set_ROSE <- ROSE(TARGET~.,train_set_rf)$data #un altro possibile metodo

#Questo è quello che ha funzionato "meglio"
#oversampling e undersampling per equilibrare le classi
train_set_rf <- ovun.sample(TARGET~.,train_set_rf,method="both",p=0.5)$data 
test_set_rf  <- df_master_trees[-train_index,]

bn <- naive.bayes(train_set_rf, "TARGET")
fitted2 <- bn.fit(bn,train_set_rf, method = "mle")

tan <- tree.bayes(train_set_rf, "TARGET")
fitted <- bn.fit(tan, train_set_rf, method = "bayes")


pred <- predict(fitted2, test_set_rf)
confusionMatrix(pred,test_set_rf[,1],positive='1')
recall(pred,test_set_rf[,1],relevant = '1')
precision(pred,test_set_rf[,1],relevant = '1')
F1_NB <- F1_Score(pred,test_set_rf[,1],positive = '1')
Accuracy(pred,test_set_rf[,1])

ROC1 <- performance(prediction(as.numeric(pred), test_set_rf$TARGET), 'tpr', 'fpr')
ROC_NB <- data.frame(x=ROC1@x.values[[1]], y=ROC1@y.values[[1]])
AUC_NB <- round(performance(prediction(as.numeric(pred),
                                       test_set_rf$TARGET),'auc')@y.values[[1]],3)


pred_2 <- predict(fitted, test_set_rf,prob = TRUE)
confusionMatrix(pred_2,test_set_rf[,1],positive='1')
recall(pred_2,test_set_rf[,1],relevant = '1')
precision(pred_2,test_set_rf[,1],relevant = '1')
F1_TAN <- F1_Score(pred_2,test_set_rf[,1],positive = '1')
Accuracy(pred_2,test_set_rf[,1])

ROC2 <- performance(prediction(as.numeric(pred_2), test_set_rf$TARGET), 'tpr', 'fpr')
ROC_TAN <- data.frame(x=ROC2@x.values[[1]], y=ROC2@y.values[[1]])
AUC_TAN <- round(performance(prediction(as.numeric(pred_2),
                                       test_set_rf$TARGET),'auc')@y.values[[1]],3)


ggplot() + 
  geom_line(data = ROC_NB,aes(x,y,col="A"),show.legend = TRUE) +
  xlab('False Positive Rate') + ylab('True Positive Rate') +
  geom_line(data=ROC_TAN,aes(x,y,col="B"),show.legend = TRUE) +
  scale_colour_manual(name = "Model", values = c("A"="red","B"="blue"),
                      labels = c("NB", "TAN")) +
  annotate("text", x=0.6, y=0, 
           label= paste0("AUC DT = ",AUC_NB),
           col="red") +
  annotate("text", x=0.6, y=0.05, 
           label= paste0("AUC RF = ",AUC_TAN),
           col="blue")