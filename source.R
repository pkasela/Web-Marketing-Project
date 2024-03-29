set.seed(12345)

setwd('/home/pranav/Desktop/Web-Marketing-Project/')
#Nella cartella del codice ci deve essere una cartella con nome "datasets"
#contenente le 7 tabelle dei dati

#Script preparation as done in class
source('Email_Engagement/script_preparation.R')
#model using Decisition Trees(DT) and RandomForest(RF)
source('Email_Engagement/Email_Engagement_DT.R')
#model using NaiveBayes and TreeAugmentedNB(TAN)
source('Email_Engagement/Email_Engagement_Propensity_NB_&_TAN.R')
#model using xgBOOST
source('Email_Engagement/Email_Engagement_Propensity_xgBOOST.R')
#plot ROC  curve with AUC measure
ggplot() + 
  geom_line(data = ROC_DT,aes(x,y,col="A"),show.legend = TRUE) +
  xlab('False Positive Rate') + ylab('True Positive Rate') +
  geom_line(data=ROC_RF,aes(x,y,col="B"),show.legend = TRUE) +
  geom_line(data=ROC_NB,aes(x,y,col="C"),show.legend = TRUE) +
  geom_line(data=ROC_TAN,aes(x,y,col="D"),show.legend = TRUE) +
  geom_line(data=ROC_XG1,aes(x,y,col="E"),show.legend = TRUE) +
  geom_line(data=ROC_XG2,aes(x,y,col="F"),show.legend = TRUE) +
  scale_colour_manual(name = "Model", 
                      values = c("A"="red","B"="blue","C"="green",
                                 "D"="brown","E"="purple","F"="orange"),
                      labels = c("DT", "RF","NB","TAN","xgBoost 1","xgBoost 2")) +
  annotate("text", x=0.8, y=0, 
           label= paste0("AUC DT = ",AUC_DT),
           col="red") +
  annotate("text", x=0.8, y=0.05, 
           label= paste0("AUC RF = ",AUC_RF),
           col="blue") +
  annotate("text", x=0.8, y=0.1, 
           label= paste0("AUC NB = ",AUC_NB),
           col="green") +
  annotate("text", x=0.8, y=0.15, 
           label= paste0("AUC TAN = ",AUC_TAN),
           col="brown") +
  annotate("text", x=0.8, y=0.2, 
           label= paste0("AUC xgBoost1 = ",AUC_XG1),
           col="purple") +
  annotate("text", x=0.8, y=0.25, 
           label= paste0("AUC xgBoost2 = ",AUC_XG2),
           col="orange")

#barplot delle F1-measure
F1_measure_DF <- data.frame(Model=c("DT","RF","NB","TAN","xgBoost 1","xgBoost 2"),
                        value=round(c(F1_DT,F1_RF,F1_NB,F1_TAN,F1_xg1,F1_xg2),3))

ggplot(data=F1_measure_DF,aes(x=reorder(Model, -value),y=value)) +
  geom_bar(stat = 'identity', fill="steelblue")+
  geom_text(aes(label=value), vjust=1.6, color="white", size=5)+
  xlab("Model") + ylab("F1-Measure")+
  theme_minimal()

#Script prepartion for the churners
source('Propensity_churn/Creazione_churner.R')
#model using Decisition Trees(DT) and RandomForest(RF)
source('Propensity_churn/churn_DT.R')
#model using NaiveBayes and TreeAugmentedNB(TAN)
source('Propensity_churn/churn_NB_&_TAN.R')
#model using xgBOOST
source('Propensity_churn/churn_xgb.R')

#bar plot delle accuracy 
Accuracy_total <- rbind(Accuracy_total,data.frame(model = c("BN","TAN","xgBoost"),
                                                  Accuracy = c(Acc_BN,Acc_TAN,Acc_xgb)))
Accuracy_total$Accuracy <- round(Accuracy_total$Accuracy,3)

ggplot(data=Accuracy_total,aes(x=reorder(model, -Accuracy),y=Accuracy)) +
  geom_bar(stat = 'identity', fill="steelblue")+
  geom_text(aes(label=Accuracy), vjust=1.6, color="white", size=5)+
  xlab("Model") + ylab("Accuracy")+
  theme_minimal()

#model to predict the future income using ARIMA and LSTM (RNN)
source('Time_Series_Model/time_Series.R')

#daily prevision ARIMA
plot(forecast(mod,10),include=30,main="Daily Forecast",
     xlab="Days",ylab="Total Revenue")
lines(331:341,time_series[332:342])

#weekly prevision ARIMA
plot(forecast(mod1,2),main="Weekly Forecast",
     xlab="Weeks",ylab="Total Revenue")
lines((nrow(df_weekly)-2):nrow(df_weekly),
      df_weekly$weekly_income_not_scaled[(nrow(df_weekly)-2):nrow(df_weekly)])

#weekly predictions with LSTM
ggplot(data.frame(x=1:length(y_test_no_scaled),y=y_test_no_scaled)) + 
  geom_line(aes(x,y,col="A")) +
  geom_line(data=data.frame(x=1:length(y_pred_no_scaled),y=y_pred_no_scaled),
            aes(x,y,col="B")) +
  scale_colour_manual(name = "Revenue", 
                      values = c("A"="red","B"="blue"),
                      labels = c("Actual", "Predicted")) +
  ylab("Total Revenue") + xlab("Weeks") +
  ggtitle("Weekly Predictions") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 2.5,y=2000000,label=paste0("MAE: ", round(MAE,4)*100,"%"))

#sink("sessionInfo.txt")
#sessionInfo()
#sink()
