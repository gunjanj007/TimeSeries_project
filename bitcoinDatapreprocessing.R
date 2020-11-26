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


##########################################################################################################
#Data Collection

# url <- "https://coinmarketcap.com/currencies/bitcoin/historical-data/?
# start=20130428&end=20190205"
# webpage <- read_html(url)
# bitcoin_data <- webpage %>%
#   html_nodes("table") %>%
#   .[[3]] %>%
#   html_table(fill = TRUE)
# html_table()
# bitcoin_data <- bitcoin_data[[1]]
# 
# head(bitcoin_data)
# summary(bitcoin_data)
# 
# 
# #scraping ethereum data
# url_2 <- "https://coinmarketcap.com/currencies/ethereum/historical-data/?start=20130428&end=20190205"
# ethereum_data <- url_2 %>%
#   html() %>%
#   html_nodes(xpath='//*[@id="historical-data"]/div/div[2]/table') %>%
#   html_table()
# ethereum_data <- ethereum_data[[1]]
# 

bitcoin_data = read.csv('bitcoin.csv')
ethereum_data = read.csv('ethereum.csv')


head(ethereum_data)
summary(ethereum_data)

colnames(bitcoin_data) = c("Date","Open","High","Low","Close","Volume","Market Cap")
colnames(ethereum_data) = c("Date","Open","High","Low","Close","Volume","Market Cap")

#about data
#The Open, High, Low and Close columns in the data set signify the Opening, highest, lowest and 
#Closing price of Bitcoin against the USD on that particular day. 
#The volume refers to the volume of bitcoin transferred in the market on a particular day and 
#Market Cap refers to the total amount of bitcoin on circulation which is to be capped at 21 million.


##########################################################################################################
#Data Cleaning for bitcoin

#formating date in standard format from character
bitcoin_data$Date <- as.Date(anytime(bitcoin_data$Date))
head(bitcoin_data)

#formating market cap as numeric from character by replacing ',' from the data
bitcoin_data$'Market Cap' <- gsub(",","",bitcoin_data$'Market Cap')
bitcoin_data$`Market Cap` <- as.numeric(bitcoin_data$`Market Cap`)

#formating volume as numeric from character by replacing ',' from the data
bitcoin_data$Volume <- gsub(",","",bitcoin_data$Volume)
bitcoin_data$Volume <- as.numeric(bitcoin_data$Volume)

bitcoin_data$Open <- gsub(",","",bitcoin_data$Open)
bitcoin_data$Open <- as.numeric(bitcoin_data$Open)

bitcoin_data$High <- gsub(",","",bitcoin_data$High)
bitcoin_data$High <- as.numeric(bitcoin_data$High)

bitcoin_data$Low <- gsub(",","",bitcoin_data$Low)
bitcoin_data$Low <- as.numeric(bitcoin_data$Low)

bitcoin_data$Close <- gsub(",","",bitcoin_data$Close)
bitcoin_data$Close <- as.numeric(bitcoin_data$Close)

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