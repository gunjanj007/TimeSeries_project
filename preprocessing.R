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

#reading data
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

#Difference between high and low on each day
a <- matrix(c(0), nrow = 0, ncol = 1)
for(i in 1:nrow(bitcoin_data)){
  a <- rbind(a, bitcoin_data[i,3] - bitcoin_data[i,4])
  i <- i + 1
}
bitcoin_data <- cbind(bitcoin_data,a)

fifty_avg <- round(mean(bitcoin_data$Volume[bitcoin_data$a < 50], na.rm = TRUE), digits = 2)
hun_avg <- round(mean(bitcoin_data$Volume[bitcoin_data$a > 50 & bitcoin_data$a < 100], na.rm = TRUE), digits = 2)
hf_avg <- round(mean(bitcoin_data$Volume[bitcoin_data$a > 100 & bitcoin_data$a < 150], na.rm = TRUE), digits = 2)
th_avg <- round(mean(bitcoin_data$Volume[bitcoin_data$a > 150 & bitcoin_data$a < 350], na.rm = TRUE), digits = 2)

for(i in 1:nrow(bitcoin_data)){
  if(is.na(bitcoin_data[i,6])){
    if(bitcoin_data$a[i] < 50){
      bitcoin_data$Volume[i] <- fifty_avg
    } else if(bitcoin_data$a[i] < 100){
      bitcoin_data$Volume[i] <- hun_avg
    } else if(bitcoin_data$a[i] < 150){
      bitcoin_data$Volume[i] <- hf_avg
    } else if(bitcoin_data$a[i] < 350){
      bitcoin_data$Volume[i] <- th_avg
    }else
      print("Uncaught Title")
  }
}

bitcoin_data <- bitcoin_data[,-8] #removing column 'a'

head(bitcoin_data)
summary(bitcoin_data$a)
boxplot(bitcoin_data$a)
