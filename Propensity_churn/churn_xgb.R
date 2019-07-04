library(caret)
library(MLmetrics)
library(rpart)
library(gbm)
library(xgboost)

set.seed(12345)

df_master_churner <- read.csv("datasets/df_master_churner.csv")

train_index <- createDataPartition(df_master_churner$CHURN, 
                                   p = .70, 
                                   list = FALSE, 
                                   times = 1)


total <- df_master_churner[,-c(2,3,12,13)]
#label encoding, since it's already a factor all we need is as.numeric
total$COD_FID <- as.numeric(total$COD_FID) 
total$REGION <- as.numeric(total$REGION)

train_set <- total[train_index,]
test_set  <- total[-train_index,]

#dtrain <- xgb.DMatrix(data = as.matrix(train_set_rf[,-1]), label = train_set_rf$TARGET)
xgb <- xgboost(data = as.matrix(train_set[,-1]), 
               label = train_set[,'CHURN'], 
               max.depth = 3, eta = 0.3, nthread = 4, nrounds = 400,
               objective = "binary:logistic"
               
)

y_pred <- predict(xgb, as.matrix(test_set[,-1]))
pred <- as.factor(as.numeric(y_pred > 0.5))
Acc_xgb <- Accuracy(pred,test_set[,1])
confusionMatrix(pred,as.factor(test_set$CHURN),positive = '1')