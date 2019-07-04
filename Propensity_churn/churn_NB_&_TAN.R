library(caret)
library(randomForest)
library(MLmetrics)
library(rpart)
library(ROSE)
library(bnlearn)

set.seed(12345)

setwd("/home/pranav/Desktop/Web-Marketing-Project/datasets/")
df_master_churner <- read.csv("df_master_churner.csv")

train_index <- createDataPartition(df_master_churner$CHURN, 
                                   p = .70, 
                                   list = FALSE, 
                                   times = 1)


df_master_churn_trees <- df_master_churner[,-c(2,3,4,5,13)]

df_master_churn_trees[,'CHURN'] <- as.factor(df_master_churn_trees[,'CHURN'])
df_master_churn_trees[,'ID_NEG'] <- as.factor(df_master_churn_trees[,'ID_NEG'])
df_master_churn_trees[,'TYP_CLI_FID'] <- as.factor(df_master_churn_trees[,'TYP_CLI_FID'])
df_master_churn_trees[,'STATUS_FID'] <- as.factor(df_master_churn_trees[,'STATUS_FID'])
df_master_churn_trees[,'W_PHONE'] <- as.factor(df_master_churn_trees[,'W_PHONE'])
df_master_churn_trees[,'TYP_CLI_ACCOUNT'] <- as.factor(df_master_churn_trees[,'TYP_CLI_ACCOUNT'])
df_master_churn_trees[,'FLAG_PRIVACY_1'] <- as.factor(df_master_churn_trees[,'FLAG_PRIVACY_1'])
df_master_churn_trees[,'FLAG_PRIVACY_2'] <- as.factor(df_master_churn_trees[,'FLAG_PRIVACY_2'])
df_master_churn_trees[,'FLAG_DIRECT_MKT'] <- as.factor(df_master_churn_trees[,'FLAG_DIRECT_MKT'])
#df_master_churn_trees[,'TOTAL_PURCHASE'] <- as.numeric(df_master_churn_trees[,'TOTAL_PURCHASE'])

train_set <- df_master_churn_trees[train_index,]
test_set  <- df_master_churn_trees[-train_index,]

bn <- naive.bayes(train_set, "CHURN")
fitted2 <- bn.fit(bn,train_set, method = "mle")

tan <- tree.bayes(train_set, "CHURN")
fitted <- bn.fit(tan, train_set, method = "bayes")


pred <- predict(fitted2, test_set)
confusionMatrix(pred,test_set[,1],positive='1')
recall(pred,test_set[,1],relevant = '1')
precision(pred,test_set[,1],relevant = '1')
F1_Score(pred,test_set[,1],positive = '1')
Acc_BN <- Accuracy(pred,test_set[,1])


pred_2 <- predict(fitted, test_set,prob = TRUE)
confusionMatrix(pred_2,test_set[,1],positive='1')
recall(pred_2,test_set[,1],relevant = '1')
precision(pred_2,test_set[,1],relevant = '1')
F1_Score(pred_2,test_set[,1],positive = '1')
Acc_TAN <- Accuracy(pred_2,test_set[,1])

setwd('/home/pranav/Desktop/Web-Marketing-Project/')