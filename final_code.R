library("anytime")
library("bsts")
library("car")
library("caret")
library("forecast")
library("keras")
library("MCMCpack")
library("smooth")
library("tensorflow")
library("tseries")
library("TTR")
library("ggplot2")
library("dplyr")
library("rvest")
library("anytime")
library("corrplot")
library("coinmarketcapr")
library("formatR")
library("yaml")  
library("googleVis")
library("knitr")

bitcoin_data = read.csv('bitcoin_updated.csv')
ethereum_data = read.csv('ethereum_updated.csv')

head(ethereum_data)
summary(ethereum_data)

colnames(bitcoin_data) = c("Date","Open","High","Low","Close","Volume","Market Cap")
colnames(ethereum_data) = c("Date","Open","High","Low","Close","Volume","Market Cap")

#Data Cleaning

#formating date in standard format from character
bitcoin_data$Date <- as.Date(anytime(bitcoin_data$Date))
head(bitcoin_data)

#formating market cap as numeric from character by replacing ',' from the data
bitcoin_data$'Market Cap' <- gsub(",","",bitcoin_data$'Market Cap')
bitcoin_data$`Market Cap` <- as.numeric(bitcoin_data$`Market Cap`)
#Market: total amount of bitcoin on circulation which is to be capped at 21 million.

#formating volume as numeric from character by replacing ',' from the data
bitcoin_data$Volume <- gsub(",","",bitcoin_data$Volume)
bitcoin_data$Volume <- as.numeric(bitcoin_data$Volume)
#volume: volume of bitcoin transferred in the market on a particular day

bitcoin_data$Open <- gsub(",","",bitcoin_data$Open)
bitcoin_data$Open <- as.numeric(bitcoin_data$Open)
#Open: Opening price of Bitcoin against the USD on that particular day

bitcoin_data$High <- gsub(",","",bitcoin_data$High)
bitcoin_data$High <- as.numeric(bitcoin_data$High)
#High: Highest price of Bitcoin against the USD on that particular day

bitcoin_data$Low <- gsub(",","",bitcoin_data$Low)
bitcoin_data$Low <- as.numeric(bitcoin_data$Low)
#Low: lowest price of Bitcoin against the USD on that particular day

bitcoin_data$Close <- gsub(",","",bitcoin_data$Close)
bitcoin_data$Close <- as.numeric(bitcoin_data$Close)
#Close: Closing price of Bitcoin against the USD on that particular day


head(bitcoin_data)

ethereum_data$Date <- as.Date(anytime(ethereum_data$Date))
head(ethereum_data)

#formating market cap as numeric from character by replacing ',' from the data
ethereum_data$'Market Cap' <- gsub(",","",ethereum_data$'Market Cap')
ethereum_data$`Market Cap` <- as.numeric(ethereum_data$`Market Cap`)

#formating volume as numeric from character by replacing ',' from the data
ethereum_data$Volume <- gsub(",","",ethereum_data$Volume)
ethereum_data$Volume <- as.numeric(ethereum_data$Volume)

ethereum_data$Open <- gsub(",","",ethereum_data$Open)
ethereum_data$Open <- as.numeric(ethereum_data$Open)

ethereum_data$High <- gsub(",","",ethereum_data$High)
ethereum_data$High <- as.numeric(ethereum_data$High)

ethereum_data$Low <- gsub(",","",ethereum_data$Low)
ethereum_data$Low <- as.numeric(ethereum_data$Low)

ethereum_data$Close <- gsub(",","",ethereum_data$Close)
ethereum_data$Close <- as.numeric(ethereum_data$Close)

#calculating total number of NA values in each column
colSums(is.na(bitcoin_data))

row.num <- c(nrow(bitcoin_data) : 11 )
train_bitcoin <- bitcoin_data[row.num,]
test_bitcoin <- bitcoin_data[10:1,]

row.num <- c(nrow(ethereum_data) : 11 )
train_ethereum <- ethereum_data[row.num,]
test_ethereum <- ethereum_data[10:1,]

#plot of train dataset
pdf('trace_train_Bitcoin.pdf')
ggplot(train_bitcoin, aes(train_bitcoin$Date, train_bitcoin$`Close`)) +
  geom_line(color = 'blue') + scale_x_date("year")+ ylim(0,20000) + ylab("Closing Price") + 
  ggtitle('train_dataset Bitcoin')
dev.off()

#plot of test dataset
pdf('trace_test_Bitcoin.pdf')
ggplot(test_bitcoin, aes(test_bitcoin$Date, test_bitcoin$`Close`)) +
  geom_line(color = 'blue') + scale_x_date("year") + ylab("Closing Price") + 
  ggtitle('test_dataset Bitcoin')
dev.off()

#plot of train dataset
pdf('trace_train_Ethereum.pdf')
ggplot(train_ethereum, aes(train_ethereum$Date, train_ethereum$`Close`)) +
  geom_line(color = 'blue') + scale_x_date("year") + ylab("Closing Price") + 
  ggtitle('train_dataset Ethereum')
dev.off()

#plot of test dataset
pdf('trace_test_Ethereum.pdf')
ggplot(test_ethereum, aes(test_ethereum$Date, test_ethereum$`Close`)) +
  geom_line(color = 'blue') + scale_x_date("year") + ylab("Closing Price") + 
  ggtitle('test_dataset')
dev.off()

#Holt's Forecasting model:

dim(train_bitcoin)

holt_model <- holt(train_bitcoin[,'Close'],type="additive", damped =F, h=10)
holt_forecast <- forecast(holt_model,h = 10)
holt_df <- as.data.frame(holt_forecast)

#Forecast plot
plot(holt_forecast, ylab = 'Closing Prices(in USD)', xlab = 'Total number of Days' , main = 'Holts Forecast')

holtfdf <- cbind(test_bitcoin[1:10,], holt_df[,1])
testdata <- test_bitcoin[1:10,5]

accuracy(holt_df[,1], testdata) #accuracy

pdf('Holt_bitcoin.pdf')
ggplot() + geom_line(data = holtfdf, aes(Date, holtfdf[,5], color = "blue")) + ylab('Closing Price') +
  geom_line(data = holtfdf, aes(Date, holtfdf[,8], color = "Dark Red")) + 
  ggtitle('Predicted vs Actual for Holts Method') +
  scale_color_discrete(name = "", labels = c("Actual", "Predicted")) 
dev.off()

dim(train_ethereum)

holt_model <- holt(train_ethereum[,'Close'],type="additive", damped =F, h=10)
holt_forecast <- forecast(holt_model,h = 10)
holt_df <- as.data.frame(holt_forecast)

#Forecast plot
plot(holt_forecast, ylab = 'Closing Prices(in USD)', xlab = 'Total number of Days' , main = 'Holts Forecast')

holtfdf <- cbind(test_ethereum[1:10,], holt_df[,1])
testdata <- test_ethereum[1:10,5]

accuracy(holt_df[,1], testdata) #accuracy

pdf('Holt_ethereum.pdf')
ggplot() + geom_line(data = holtfdf, aes(Date, holtfdf[,5],color = "blue")) + ylab('Closing Price') +
  geom_line(data = holtfdf, aes(Date, holtfdf[,8], color = "Dark Red") ) + 
  ggtitle('Predicted vs Actual for Holts Method') +
  scale_color_discrete(name = "", labels = c("Actual", "Predicted"))
dev.off()


# ETS 
ETS <- ets((train_bitcoin[,'Close']),allow.multiplicative.trend=TRUE)
ETS_forecast <- forecast(ETS, h = 10)
ETS_df <- as.data.frame(ETS_forecast)

plot(forecast(ETS, h =10), ylim = c(0,20000), xlab = 'Closing Prices(in USD)', 
     ylab = 'Total number of Days' ,main = 'ETS Forecast')

ETS_p <- predict(ETS, n.ahead = 10, prediction.interval = T, level = 0.95)

testdata <- test_bitcoin[1:10,5]

accuracy(ETS_df[,1], testdata)

etsfdf <- cbind(test_bitcoin[1:10,], ETS_df[,1])

pdf('ETS_bitcoin.pdf')
ggplot() + geom_line(data = etsfdf, aes(Date, etsfdf[,5],color = "blue")) + ylab('Closing Price') +
  geom_line(data = etsfdf, aes(Date, etsfdf[,8], color = "Dark Red") ) + 
  ggtitle('Predicted vs Actual for ETS method') +
  scale_color_discrete(name = "", labels = c("Actual", "Predicted"))
dev.off()

ETS <- ets((train_ethereum[,'Close']),allow.multiplicative.trend=TRUE)
ETS_forecast <- forecast(ETS, h = 10)
ETS_df <- as.data.frame(ETS_forecast)

plot(forecast(ETS, h =10), ylim = c(0,20000), xlab = 'Closing Prices(in USD)', 
     ylab = 'Total number of Days' ,main = 'ETS Forecast')

ETS_p <- predict(ETS, n.ahead = 10, prediction.interval = T, level = 0.95)

testdata <- test_ethereum[1:10,5]

accuracy(ETS_df[,1], testdata)

etsfdf <- cbind(test_ethereum[1:10,], ETS_df[,1])

pdf('ETS_ethereum.pdf')
ggplot() + geom_line(data = etsfdf, aes(Date, etsfdf[,5], color = "blue")) + ylab('Closing Price') +
  geom_line(data = etsfdf, aes(Date, etsfdf[,8], color = "Dark Red")) + 
  ggtitle('Predicted vs Actual for ETS method') +
  scale_color_discrete(name = "", labels = c("Actual", "Predicted"))
dev.off()


tsdf <- diff(train_bitcoin[,5], lag = 1)
tsdf <- tsdf[!is.na(tsdf)]
adf.test(tsdf)
plot(tsdf,type = "l", xlab = 'Closing Price',ylab='Number_of_Days',ylim = c(-5000, 5000), main='Difference Plot')

#As well as looking at the time plot of the data, the ACF plot is also useful for identifying 
#non-stationary time series. For a stationary time series, the ACF will drop to zero relatively quickly,
#while the ACF of non-stationary data decreases slowly.

#Auto- and Cross- Covariance and -Correlation Function Estimation
#plots the estimates 
pdf('ACF_bitcoin.pdf')
acf(tsdf)
dev.off()
#pacf is used for partial auto correlation function
pdf('PACF_bitcoin.pdf')
pacf(tsdf)
dev.off()

tsdf <- diff(train_ethereum[,5], lag = 1)
tsdf <- tsdf[!is.na(tsdf)]
adf.test(tsdf)
plot(tsdf,type = "l", xlab = 'Closing Price',ylab='Number_of_Days',ylim = c(-5000, 5000), main='Difference Plot')

#As well as looking at the time plot of the data, the ACF plot is also useful for identifying 
#non-stationary time series. For a stationary time series, the ACF will drop to zero relatively quickly,
#while the ACF of non-stationary data decreases slowly.

#Auto- and Cross- Covariance and -Correlation Function Estimation
#plots the estimates 
pdf('ACF_ethereum.pdf')
acf(tsdf)
dev.off()
#pacf is used for partial auto correlation function
pdf('PACF_ethereum.pdf')
pacf(tsdf)  
dev.off()


auto.arima(train_bitcoin[,5])

bitcoin_time_series_forecast_auto <- forecast(auto.arima(train_bitcoin[,5]), h=10)
plot(bitcoin_time_series_forecast_auto, ylab = 'Closing Price', xlab = 'Number_of_Days', main='Forecast AUTO-ARIMA (1,1,1)')

bts_f_df_auto <- as.data.frame(bitcoin_time_series_forecast_auto)

testdata <- test_bitcoin[1:10,5]
accuracy(bts_f_df_auto[,1],testdata)

pdf('auto_arima_bitcoin.pdf')
gegefct_auto <- cbind(test_bitcoin[1:10,], bts_f_df_auto[,1])
ggplot() + geom_line(data = gegefct_auto, aes(Date, gegefct_auto[,5], color = "blue")) + ylab('Closing Price') +
  geom_line(data = gegefct_auto, aes(Date, gegefct_auto[,8], color = "Dark Red")) +  
  ggtitle('Predicted vs Actual for Auto-ARIMA') +
  scale_color_discrete(name = "", labels = c("Actual", "Predicted"))
dev.off()

auto.arima(train_ethereum[,5])

ethereum_time_series_forecast_auto <- forecast(auto.arima(train_ethereum[,5]), h=10)
plot(ethereum_time_series_forecast_auto, ylab = 'Closing Price', xlab = 'Number_of_Days', main='Forecast AUTO-ARIMA (1,1,1)')

bts_f_df_auto <- as.data.frame(ethereum_time_series_forecast_auto)

testdata <- test_ethereum[1:10,5]
accuracy(bts_f_df_auto[,1],testdata)

pdf('auto_arima_ethereum.pdf')
gegefct_auto <- cbind(test_ethereum[1:10,], bts_f_df_auto[,1])
ggplot() + geom_line(data = gegefct_auto, aes(Date, gegefct_auto[,5],color = "blue") ) + ylab('Closing Price') +
  geom_line(data = gegefct_auto, aes(Date, gegefct_auto[,8], color = "Dark Red") ) +  
  ggtitle('Predicted vs Actual for Auto-ARIMA') +
  scale_color_discrete(name = "", labels = c("Actual", "Predicted"))
dev.off()



# ARIMA 
bitcoin_time_series_forecast <- forecast(arima(train_bitcoin[,5], order = c(9,1,1)), h=10)
arima(train_bitcoin[,5], order = c(9,1,1))

bitcoin_time_series_forecast 
plot(bitcoin_time_series_forecast)

bts_f_df <- as.data.frame(bitcoin_time_series_forecast)

testdata <- test_bitcoin[1:10,5]
accuracy(bts_f_df[,1],testdata)


gegefct <- cbind(test_bitcoin[1:10,], bts_f_df[,1])

plot(bitcoin_time_series_forecast,ylab = 'Closing Price', xlab = 'Number_of_Days', main='Forecast ARIMA (1,1,8)')
pdf('arima_bitcoin.pdf')
ggplot() + geom_line(data = gegefct, aes(Date, gegefct[,5], color = "blue")) + ylab('Closing Price') +
  geom_line(data = gegefct, aes(Date, gegefct[,8], color = "Dark Red")) +  
  ggtitle('Predicted vs Actual for ARIMA') +
  scale_color_discrete(name = "", labels = c("Actual", "Predicted"))
dev.off()


ethereum_time_series_forecast <- forecast(arima(train_ethereum[,5], order = c(8,1,1)), h=10)
arima(train_ethereum[,5], order = c(8,1,1))

ethereum_time_series_forecast 
plot(ethereum_time_series_forecast)

bts_f_df <- as.data.frame(ethereum_time_series_forecast)

testdata <- test_ethereum[1:10,5]
accuracy(bts_f_df[,1],testdata)


gegefct <- cbind(test_ethereum[1:10,], bts_f_df[,1])
plot(ethereum_time_series_forecast,ylab = 'Closing Price', xlab = 'Number_of_Days', main='Forecast ARIMA (1,1,8)')
pdf('arima_ethereum.pdf')
ggplot() + geom_line(data = gegefct, aes(Date, gegefct[,5], color = "blue")) + ylab('Closing Price') +
  geom_line(data = gegefct, aes(Date, gegefct[,8], color = "Dark Red")) +  
  ggtitle('Predicted vs Actual for ARIMA') +
  scale_color_discrete(name = "", labels = c("Actual", "Predicted"))
dev.off()


all <- cbind(test_bitcoin[1:10,],bts_f_df[,1],ETS_df[,1],holt_df[,1])
head(all)

pdf('all_bitcoin.pdf')
ggplot() + geom_line(data = all, aes(Date, all[,5], color = "blue")) + ylab('Closing Price') +
  geom_line(data = all, aes(Date, all[,8], color = "green")) +  
  geom_line(data = all, aes(Date, all[,9], color = "red")) +  
  geom_line(data = all, aes(Date, all[,10], color = "orange")) +  
  ggtitle('Predicted vs Actual for all methods') +
  scale_color_discrete(name = "", labels = c("True", "ARIMA", "ETS", "Holt"))
dev.off()

all <- cbind(test_ethereum[1:10,],bts_f_df[,1],ETS_df[,1],holt_df[,1])#, bts_f_df_auto[,1])
head(all)
colors <- c("Actual" = "blue", "ARIMA" = "green", "ETS" = "red", "Holt's" = "orange")

pdf('all_ethereum.pdf')
ggplot() + geom_line(data = all, aes(Date, all[,5], color = "blue")) + ylab('Closing Price') +
  geom_line(data = all, aes(Date, all[,8], color = "green")) +  
  geom_line(data = all, aes(Date, all[,9], color = "red")) +  
  geom_line(data = all, aes(Date, all[,10], color = "orange")) +  
  ggtitle('Predicted vs Actual for all methods') +
  scale_color_discrete(name = "", labels = c("True", "ARIMA", "ETS", "Holt"))
dev.off()