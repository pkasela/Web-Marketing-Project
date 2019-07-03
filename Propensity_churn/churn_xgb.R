require(caret)
require(randomForest)
require(MLmetrics)
require(rpart)
require(ROSE)
require(gbm)
require(xgboost)

set.seed(12345)

dir = "/Users/antonellazaccaria"
setwd(dir)
df_master_churner <- read.csv("df_master_churner.csv")

train_index <- createDataPartition(df_master_churner$CHURN, 
                                   p = .60, 
                                   list = FALSE, 
                                   times = 1)
#df_master[,'TARGET'] <- as.factor(df_master[,'TARGET'])

#f1 <- function(data, lev = NULL, model = NULL) {
#  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1])
#  c(F1 = f1_val)
#}

#elimino gli indici nella prima colonna che sono ID
#train_set <- df_master[train_index,-c(1,3,4,5,6,9,10)]
#test_set  <- df_master[-train_index,-c(1,3,4,5,6,9,10)] 

colonne_na <- sapply(colnames(df_master_churner[,-c(2,3,8,13,14,15,16)]),
                     function(x) any(is.na(df_master_churner[,x])))
colonne_na[colonne_na == TRUE]
#EMAIL_PROVIDER_CLEAN, PRV, W_PHONE, TYP_JOB
#Li elimino

total_rf <- df_master_churner[,-c(2,3,8,13,14,15,16)][,!colonne_na]

train_set_rf <- total_rf[train_index,]
test_set_rf  <- total_rf[-train_index,]

#dtrain <- xgb.DMatrix(data = as.matrix(train_set_rf[,-1]), label = train_set_rf$TARGET)
xgb <- xgboost(data = as.matrix(train_set_rf[,-1]), 
               label = train_set_rf[,'CHURN'], 
               max.depth = 20, eta = 0.5, nthread = 4, nrounds = 100,
               objective = "binary:logistic"
               
)
xgb2 <- xgboost(data = as.matrix(train_set_rf[,-1]), 
               label = train_set_rf[,'CHURN'], 
               max.depth = 30, booster = "gblinear", nthread = 4, nrounds = 100,
               objective = "binary:logistic"
)

y_pred <- predict(xgb, as.matrix(test_set_rf[,-1]))
pred <- as.factor(as.numeric(y_pred > 0.5))
Accuracy(pred,test_set_rf[,1])

y_pred <- predict(xgb2, as.matrix(test_set_rf[,-1]))
pred <- as.factor(as.numeric(y_pred > 0.5))
Accuracy(pred,test_set_rf[,1])
