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
bitcoin_data = read.csv('bitcoin.csv')
ethereum_data = read.csv('ethereum.csv')

head(ethereum_data)
summary(ethereum_data)