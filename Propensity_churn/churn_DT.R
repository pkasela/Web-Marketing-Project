library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(MLmetrics)
library(rpart)

set.seed(12345)

df_master_churner <- read.csv("datasets/df_master_churner.csv")

#Si elimina Client ID, LAST_PURCHASE (che indica i churners) e PRV (troppi fattori)
df_master_churn_trees <- df_master_churner[,-c(2,3,13)]

#Traformazione variabili in factor
df_master_churn_trees[,'CHURN'] <- as.factor(df_master_churn_trees[,'CHURN'])
df_master_churn_trees[,'ID_NEG'] <- as.factor(df_master_churn_trees[,'ID_NEG'])
df_master_churn_trees[,'TYP_CLI_FID'] <- as.factor(df_master_churn_trees[,'TYP_CLI_FID'])
df_master_churn_trees[,'STATUS_FID'] <- as.factor(df_master_churn_trees[,'STATUS_FID'])
df_master_churn_trees[,'W_PHONE'] <- as.factor(df_master_churn_trees[,'W_PHONE'])
df_master_churn_trees[,'TYP_CLI_ACCOUNT'] <- as.factor(df_master_churn_trees[,'TYP_CLI_ACCOUNT'])
df_master_churn_trees[,'FLAG_PRIVACY_1'] <- as.factor(df_master_churn_trees[,'FLAG_PRIVACY_1'])
df_master_churn_trees[,'FLAG_PRIVACY_2'] <- as.factor(df_master_churn_trees[,'FLAG_PRIVACY_2'])
df_master_churn_trees[,'FLAG_DIRECT_MKT'] <- as.factor(df_master_churn_trees[,'FLAG_DIRECT_MKT'])
df_master_churn_trees[,'TOTAL_PURCHASE'] <- as.numeric(df_master_churn_trees[,'TOTAL_PURCHASE'])

str(df_master_churn_trees)

#Train e Test set 
train_index <- createDataPartition(df_master_churn_trees$CHURN, 
                                   p = .60, 
                                   list = FALSE, 
                                   times = 1)

train_set <- df_master_churn_trees[train_index,]
test_set <- df_master_churn_trees[-train_index,]

#DT con tutte le variabili 
tree_model_churn <- rpart(CHURN ~ ., data= train_set)
rpart.plot(tree_model_churn, extra = 106)

summary(tree_model_churn)
printcp(tree_model_churn)

#Parte Test 
pred <- rpart.predict(tree_model_churn, test_set[,-1],type = "class")
confusionMatrix(pred, test_set[,1],positive='1')

recall(pred,test_set[,1],relevant = '1')
precision(pred,test_set[,1],relevant = '1')
F1_Score(pred,test_set[,1],positive = '1')
Accuracy_dt_all <- Accuracy(pred,test_set[,1])
cat(Accuracy_dt_all)

#Tolgo Number of Purchase perché tutto l'albero dipende praticamente da quello 
tree_model_churn1 <- rpart(CHURN ~ ., data= train_set[,-c(3,4)])
rpart.plot(tree_model_churn1, extra = 106,roundint=FALSE)

summary(tree_model_churn1)
printcp(tree_model_churn1)

#Test 
pred <- rpart.predict(tree_model_churn1, test_set[,-1],type = "class")
confusionMatrix(pred, test_set[,1],positive='1')

recall(pred,test_set[,1],relevant = '1')
precision(pred,test_set[,1],relevant = '1')
F1_Score(pred,test_set[,1],positive = '1')
Accuracy_dt_less_one <- Accuracy(pred,test_set[,1])
cat(Accuracy_dt_less_one)

#Random Forest 
#memory.limit(100000) #solo per Windows
tree_model_churn_rf <- randomForest(CHURN ~ ., data= train_set, ntree = 100)

print(tree_model_churn_rf)
importance(tree_model_churn_rf)

pred_churn_rf <- rpart.predict(tree_model_churn_rf, test_set[,-1], type = "class")
confusionMatrix(pred_churn_rf, test_set[,1],positive='1')

recall(pred_churn_rf, test_set[,1],relevant = '1')
precision(pred_churn_rf ,test_set[,1],relevant = '1')
F1_Score(pred_churn_rf ,test_set[,1],positive = '1')
Accuracy_rf <- Accuracy(pred_churn_rf, test_set[,1])
cat(Accuracy_rf)
Accuracy_total <- data.frame(model = c("DT_all", "DT_less_one", "RF"), Accuracy = c(Accuracy_dt_all, 
                                                                                    Accuracy_dt_less_one, 
                                                                                    Accuracy_rf))
