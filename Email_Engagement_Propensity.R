require(caret)
require(randomForest)
require(MLmetrics)
require(rpart)
require(ROSE)

set.seed(12345)

setwd("/home/pranav/Desktop/web marketing/datasets/")
df_master <- read.csv("df_master.csv")

train_index <- createDataPartition(df_master$TARGET, 
                                   p = .60, 
                                   list = FALSE, 
                                   times = 1)
df_master[,'TARGET'] <- as.factor(df_master[,'TARGET'])

#f1 <- function(data, lev = NULL, model = NULL) {
#  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1])
#  c(F1 = f1_val)
#}

#elimino gli indici nella prima colonna che sono ID
#train_set <- df_master[train_index,-c(1,3,4,5,6,9,10)]
#test_set  <- df_master[-train_index,-c(1,3,4,5,6,9,10)] 

colonne_na <- sapply(colnames(df_master[,-c(1,5,6,9,10)]),
                     function(x) any(is.na(df_master[,x])))
colonne_na[colonne_na == TRUE]
#EMAIL_PROVIDER_CLEAN, PRV, W_PHONE, TYP_JOB
#Li elimino

total_rf <- df_master[,-c(1,5,6,9,10)][,!colonne_na]

train_set_rf <- total_rf[train_index,]
#train_set_rf <- SMOTE(TARGET ~ ., train_set_rf, k=3,
#                      perc.over = 100, perc.under=100)
#train_set_ROSE <- ROSE(TARGET~.,train_set_rf)$data

train_set_rf <- ovun.sample(TARGET~.,train_set_rf,method="both",p=0.5)$data #Questo è quello che ha funzionato "meglio"
test_set_rf  <- total_rf[-train_index,]

TreeBag_model  <- train(x=train_set_ROSE[,-1],y=train_set_ROSE[,'TARGET'], method = "treebag",
                   trControl=trainControl(method="none"),
                   metric="Accuracy") 

rf_mdl =randomForest(x=train_set_ROSE[,-1],y=train_set_ROSE[,'TARGET'],
                     ntree=100, trControl = trainControl(method="none"),
                     keep.forest=TRUE,importance=TRUE)


rf_mdl_2 =randomForest(x=train_set_ROSE[,-1],y=train_set_ROSE[,'TARGET'],
                     ntree=100, trControl = trainControl(method="none"),
                     keep.forest=TRUE,importance=TRUE,class_weight=c(1,150))

print(rf_mdl)
pred <- predict(rf_mdl, test_set_rf[,-1])
confusionMatrix(pred,test_set_rf[,1],positive='1')
recall(pred,test_set_rf[,1],relevant = '1')
precision(pred,test_set_rf[,1],relevant = '1')
F1_Score(pred,test_set_rf[,1],positive = '1')
Accuracy(pred,test_set_rf[,1])

print(rf_mdl_2)
pred2 <- predict(rf_mdl_2, test_set_rf[,-1])
confusionMatrix(pred2,test_set_rf[,1],positive='1')
recall(pred2,test_set_rf[,1],relevant = '1')
precision(pred2,test_set_rf[,1],relevant = '1')
F1_Score(pred2,test_set_rf[,1],positive = '1')
Accuracy(pred2,test_set_rf[,1])

print(TreeBag_model)
pred3 <- predict(TreeBag_model, test_set_rf[,-1])
confusionMatrix(pred3,test_set_rf[,1],positive='1')
recall(pred3,test_set_rf[,1],relevant = '1')
precision(pred3,test_set_rf[,1],relevant = '1')
F1_Score(pred3,test_set_rf[,1],positive = '1')
Accuracy(pred3,test_set_rf[,1])