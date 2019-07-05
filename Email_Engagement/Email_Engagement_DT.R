library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
library(caret)
library(MLmetrics)
library(rpart)

set.seed(12345)
df_master = read.csv2("datasets/df_master.csv")

#elimino gli indici nella prima colonna che sono ID
# e le colonne non accettate dal modello oppure con tantissimi factor
df_master_trees <- df_master[,-c(1,7,8,9,10,21)]
 
df_master_trees[,'TARGET'] <- as.factor(df_master_trees[,'TARGET'])
df_master_trees[,'NUM_SEND_PREV'] <- as.factor(df_master_trees[,'NUM_SEND_PREV'])
df_master_trees[,'NUM_OPEN_PREV'] <- as.factor(df_master_trees[,'NUM_OPEN_PREV'])
df_master_trees[,'NUM_CLICK_PREV'] <- as.factor(df_master_trees[,'NUM_CLICK_PREV'])
df_master_trees[,'NUM_FAIL_PREV'] <- as.factor(df_master_trees[,'NUM_FAIL_PREV'])
df_master_trees[,'ID_NEG'] <- as.factor(df_master_trees[,'ID_NEG'])
df_master_trees[,'TYP_CLI_FID'] <- as.factor(df_master_trees[,'TYP_CLI_FID'])
df_master_trees[,'STATUS_FID'] <- as.factor(df_master_trees[,'STATUS_FID'])
df_master_trees[,'NUM_FIDs'] <- as.factor(df_master_trees[,'NUM_FIDs'])
df_master_trees[,'TYP_CLI_ACCOUNT'] <- as.factor(df_master_trees[,'TYP_CLI_ACCOUNT'])
df_master_trees[,'FLAG_PRIVACY_1'] <- as.factor(df_master_trees[,'FLAG_PRIVACY_1'])
df_master_trees[,'FLAG_PRIVACY_2'] <- as.factor(df_master_trees[,'FLAG_PRIVACY_2'])
df_master_trees[,'FLAG_DIRECT_MKT'] <- as.factor(df_master_trees[,'FLAG_DIRECT_MKT'])
df_master_trees[,'W_PHONE'] <- as.factor(df_master_trees[,'W_PHONE'])

str(df_master_trees)

#Train e Test Set 
train_index <- createDataPartition(df_master$TARGET, 
                                   p = .60, 
                                   list = FALSE, 
                                   times = 1)

train_set <- df_master_trees[train_index,]
test_set <- df_master_trees[-train_index,]

#Decision tree considerando tutte le variabili 
#Parte Train
tree_model <- rpart(TARGET ~ ., data= train_set)
rpart.plot(tree_model, extra = 106)

summary(tree_model)
printcp(tree_model)

#Parte Test 
pred <- rpart.predict(tree_model, test_set[,-1],type = "class")
confusionMatrix(pred, test_set[,1],positive='1') 

recall(pred,test_set[,1],relevant = '1')
precision(pred,test_set[,1],relevant = '1')
F1_DT <- F1_Score(pred,test_set[,1],positive = '1')
Accuracy(pred,test_set[,1])

#ROC curve 
pred_with_prob <- rpart.predict(tree_model, test_set[, -1], type = "prob")[,2]
#viene salvato qua, nel file source verrà unito in un unico grafico ad altri modelli di ROC
ROC1 <- performance(prediction(pred_with_prob, test_set$TARGET), 'tpr', 'fpr')
ROC_DT <- data.frame(x=ROC1@x.values[[1]], y=ROC1@y.values[[1]])
AUC_DT <- round(performance(prediction(pred_with_prob, 
                                  test_set$TARGET),'auc')@y.values[[1]],3)

#Random Forest 
#memory.limit(100000) #se si usa Windows
tree_model_rf <- randomForest(TARGET ~ ., data= train_set, ntree = 100)
print(tree_model_rf)
importance(tree_model_rf)

pred_rf <- rpart.predict(tree_model_rf, test_set[,-1], type = "class")
confusionMatrix(pred_rf, test_set[,1],positive='1')
recall(pred_rf, test_set[,1],relevant = '1')
precision(pred_rf ,test_set[,1],relevant = '1')
F1_RF <- F1_Score(test_set[,1],pred_rf,positive = '1')
Accuracy(pred_rf, test_set[,1])

pred_with_prob_rf <- rpart.predict(tree_model_rf, test_set[, -1], type = "prob")[,2]

ROC2 <- performance(prediction(pred_with_prob_rf, test_set$TARGET),  
                    measure='tpr', x.measure='fpr')
ROC_RF <- data.frame(x=ROC2@x.values[[1]], y=ROC2@y.values[[1]])
AUC_RF <- round(performance(prediction(pred_with_prob_rf, 
                                  test_set$TARGET),'auc')@y.values[[1]],3)

#ROC_RF + ROC_DT
ggplot() + 
  geom_line(data = ROC_DT,aes(x,y,col="A"),show.legend = TRUE) +
  xlab('False Positive Rate') + ylab('True Positive Rate') +
  geom_line(data=ROC_RF,aes(x,y,col="B"),show.legend = TRUE) +
  scale_colour_manual(name = "Model", values = c("A"="red","B"="blue"),
                      labels = c("DT", "RF")) +
  annotate("text", x=0.6, y=0, 
           label= paste0("AUC DT = ",AUC_DT),
           col="red") +
  annotate("text", x=0.6, y=0.05, 
           label= paste0("AUC RF = ",AUC_RF),
           col="blue")