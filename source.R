setwd('/home/pranav/Desktop/Web-Marketing-Project/')

#Script preparation as done in class
source('Email_Engagement/script_preparation.R')
#model using DT and RF
source('Email_Engagement/Email_Engagement_DT.R')
#model using RF
source('Email_Engagement/Email_Engagement_Propensity.R')
#model using xgBOOST
source('Email_Engagement/Email_Engagement_Propensity_xgBOOST.R')
#plot accuracy

#Script prepartion for the churners
source('Propensity_churn/Creazione_churner.R')
#model using DT and RF
source('Propensity_churn/churn_DT.R')
#model using RF
source('Propensity_churn/churn_RF.R')
#model using xgBOOST
source('Propensity_churn/churn_xgb.R')

#model to predict the future income using ARIMA and LSTM (RNN)
source('Time_Series_Model/time_Series.R')
