library(tidyr)
library(dplyr)
library(forecast)
library(ggplot2)
library(keras)

set.seed(12345)

df_7_scrontrini <- read.csv2("datasets/raw_7_tic.csv")

df_7_scrontrini$DATETIME <- as.Date(df_7_scrontrini$DATETIME)
df_7_scrontrini$IMPORTO_LORDO <- as.numeric(gsub(",",".",df_7_scrontrini$IMPORTO_LORDO))
df_7_scrontrini['IMPORTO_NETTO'] = df_7_scrontrini['IMPORTO_LORDO']-df_7_scrontrini['SCONTO']

df_7 <- df_7_scrontrini %>%
  group_by(DATETIME) %>%
  summarise(IMPORTO_TOTALE=sum(IMPORTO_NETTO))

minday=min(df_7$DATETIME)
plot(ts(df_7$IMPORTO_TOTALE,frequency=365, start=c(2018, as.numeric(format(minday, "%j")))))

#elimino gli outlier pesanti
df_7_no_out <- df_7[df_7$IMPORTO_TOTALE<525190 & df_7$IMPORTO_TOTALE>150382,]


time_series <- ts(df_7_no_out$IMPORTO_TOTALE,freq=30)
plot(time_series)

Acf(time_series[2:342],80)
Pacf(time_series[2:342],80)
mod <- Arima(time_series[2:332],c(7,0,0),c(0,0,0))
pred <- forecast(mod,10)
plot(pred)
lines(332:342,time_series[332:342])
#stima giornaliera non si riesce benissimo anche con auto.arima
#proviamo con la stima settimanale

df_weekly <- df_7 %>% 
  group_by(week = cut(DATETIME, "week")) %>% 
  mutate(weekly_income = sum(IMPORTO_TOTALE)) %>%
  select(week,weekly_income) %>%
  unique() 

plot((df_weekly$weekly_income),type="l")
#notimo che la prima e l'ultima settimana non ha dati completi della settimana allora va eliminati
df_weekly=df_weekly[2:(nrow(df_weekly)-1),]

plot(df_weekly$weekly_income,type="l")

Acf(df_weekly$weekly_income)
#si nota un ritardo fino a due
Pacf(df_weekly$weekly_income)
#Sembra un mix tra AR2 e AR1 quindi si testano entrambi e si usa come misura AIC

mod1 <- Arima(df_weekly$weekly_income[0:(nrow(df_weekly)-2)],c(1,0,0))
Acf(mod1$residuals)
Pacf(mod1$residuals)
plot(forecast(mod1,2))
lines((nrow(df_weekly)-2):nrow(df_weekly),
      df_weekly$weekly_income[(nrow(df_weekly)-2):nrow(df_weekly)])

mod2 <- Arima(df_weekly$weekly_income[0:(nrow(df_weekly)-2)],c(2,0,0))
Acf(mod2$residuals)
Pacf(mod2$residuals)
plot(forecast(mod2,2))
lines((nrow(df_weekly)-2):nrow(df_weekly),
      df_weekly$weekly_income[(nrow(df_weekly)-2):nrow(df_weekly)])

#Hanno un AIC :mod1 --> 1377.23 < mod2 --> 1378.03,
#quindi mod1 è migliore inoltre le 2 previsioni migliori sono quelli di mod1, 
#poichè stanno nell'intervallo di confidenza più piccolo.
#il modello scelto è mod1

#Modello LSTM (Reti Neurali):
df_weekly$weekly_income_not_scaled <- df_weekly$weekly_income
max_df <- max(df_weekly$weekly_income_not_scaled[0:40])
min_df <- min(df_weekly$weekly_income_not_scaled[0:40])
df_weekly$weekly_income <- (df_weekly$weekly_income_not_scaled-min_df)/(max_df-min_df)

look_behind <- 5 #purtroppo per la scarsità dei dati non posso allargare la finestra

lung_train <- 36 #settimane, predizione sulle succesive 11

x_train <- matrix(df_weekly$weekly_income[0:look_behind],
                  ncol=look_behind)
y_train <- matrix(df_weekly$weekly_income[look_behind+1],ncol=1)

for (i in 1:(lung_train-1)){
  x_train <- rbind(x_train,
                   df_weekly$weekly_income[(i+1):(look_behind+i)])
  y_train <- rbind(y_train,
                   df_weekly$weekly_income[look_behind+i+1])
}

dim(x_train) <- c(nrow(x_train), look_behind, 1)
X_shape2 = dim(x_train)[2]
X_shape3 = dim(x_train)[3]
batch_size = 3
units = 15 

model <- keras_model_sequential() 
model%>%
  layer_lstm(units=units, activation = 'tanh',
             batch_input_shape = c(batch_size, X_shape2, X_shape3),stateful = TRUE)%>%
  layer_dense(units = 1)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_rmsprop(lr=0.01),  
  metrics = c('mae')
)

model %>% fit(x_train, y_train, epochs=30, batch_size=batch_size, verbose=1, shuffle=FALSE)

lung_test <- 9
x_test <- matrix(df_weekly$weekly_income[(lung_train+1):
                                           (lung_train+look_behind)],
                            ncol=look_behind)
y_test <- matrix(df_weekly$weekly_income[look_behind+lung_train+1],ncol=1)

for (i in 1:(lung_test-1)){
  x_test <- rbind(x_test,
                   df_weekly$weekly_income[(lung_train+i+1):(lung_train+look_behind+i)])
  y_test <- rbind(y_test,
                   df_weekly$weekly_income[look_behind+lung_train+i+1])
}

dim(x_test) <- c(nrow(x_test), look_behind, 1)


y_pred <- predict(model,x_test,batch_size = batch_size)

y_pred_no_scaled <- (y_pred * (max_df-min_df)) +min_df
y_test_no_scaled <- (y_test * (max_df-min_df)) + min_df

plot(y_test_no_scaled,type="l",col="red",ylim = c(min_df,max_df),)
points(y_test_no_scaled,col="red")
lines(y_pred_no_scaled)
points(y_pred_no_scaled)

MAE <- mean(abs(y_test_no_scaled-y_pred_no_scaled)/y_pred_no_scaled)