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
colonne_na <- sapply(colnames(df_master_churner[,-c(2,3,8,13,14,15,16)]),
                     function(x) any(is.na(df_master_churner[,x])))
colonne_na[colonne_na == TRUE]

total_rf <- df_master_churner[,-c(2,3,8,13,14,15,16)][,!colonne_na]

train_set_rf <- total_rf[train_index,]
test_set_rf  <- total_rf[-train_index,]

rf_mdl =randomForest(x=train_set_rf[,-1],y=train_set_rf[,'CHURN'],
                     ntree=100, trControl = trainControl(method="none"),
                     keep.forest=TRUE,importance=TRUE)

y_pred <- predict(rf_mdl, as.matrix(test_set_rf[,-1]))
pred <- as.factor(as.numeric(y_pred > 0.5))
Accuracy(pred,test_set_rf[,1])
