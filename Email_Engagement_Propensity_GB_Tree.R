require(caret)
require(randomForest)
require(MLmetrics)
require(rpart)
require(ROSE)
require(gbm)
require(xgboost)

set.seed(12345)

setwd("/home/pranav/Desktop/web marketing/datasets/")
df_master <- read.csv("df_master.csv")

train_index <- createDataPartition(df_master$TARGET, 
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

colonne_na <- sapply(colnames(df_master[,-c(1,3,4,9,10,11,14,20,21,22,23)]),
                     function(x) any(is.na(df_master[,x])))
colonne_na[colonne_na == TRUE]
#EMAIL_PROVIDER_CLEAN, PRV, W_PHONE, TYP_JOB
#Li elimino

total_rf <- df_master[,-c(1,3,4,9,10,11,14,20,21,22,23)][,!colonne_na]

train_set_rf <- total_rf[train_index,]
#train_set_rf <- SMOTE(TARGET ~ ., train_set_rf, k=3,
#                      perc.over = 100, perc.under=100)
#train_set_ROSE <- ROSE(TARGET~.,train_set_rf)$data
#train_set_rf <- ovun.sample(TARGET~.,train_set_rf,method="both",p=0.5)$data #Questo è quello che ha funzionato "meglio"

test_set_rf  <- total_rf[-train_index,]

#dtrain <- xgb.DMatrix(data = as.matrix(train_set_rf[,-1]), label = train_set_rf$TARGET)
xgb <- xgboost(data = as.matrix(train_set_rf[,-1]), 
               label = train_set_rf[,'TARGET'], 
               max.depth = 10, eta = 1, nthread = 4, nrounds = 50,
               objective = "binary:logistic",
)

xgb <- xgboost(data = as.matrix(train_set_rf[,-1]), 
               label = train_set_rf[,'TARGET'], 
               max.depth = 20, booster = "gblinear", nthread = 4, nrounds = 50,
               objective = "binary:logistic",
)

y_pred <- predict(xgb, as.matrix(test_set_rf[,-1]))
pred <- as.factor(as.numeric(y_pred > 0.5))
recall(pred,as.factor(test_set_rf[,1]),relevant = '1')
precision(pred,as.factor(test_set_rf[,1]),relevant = '1')
F1_Score(pred,as.factor(test_set_rf[,1]),positive = '1')
Accuracy(pred,test_set_rf[,1])

