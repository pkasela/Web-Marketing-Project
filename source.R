setwd('/home/pranav/Desktop/Web-Marketing-Project/')

#Script preparation as done in class
source('script_preparation.R')
#model using DT
source('Email_Engagement_DT.R')
#model using RF
source('Email_Engagement_Propensity.R')
#model using xgBOOST
source('Email_Engagement_Propensity_xgBOOST.R')
#plot accuracy

#Script prepartion for the churners
source('Creazione_churner.R')
#model using DT
source('churn_DT.R')
#model using RF
source('churn_RF.R')
#model using xgBOOST
source('churn_xgb.R')

#model to predict the future income using ARIMA and LSTM (RNN)
source('time_Series.R')
