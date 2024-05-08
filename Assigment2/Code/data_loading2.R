#Data loading for Assigment 3 
#
#
#Task: Model log measures of returns of stock index using ARMA-GARCH
#Identify, Estimate parameters, Verify, Plot and Discuss the estimated volatility.
#Predict the vývoj of returns and volatility 5 days ahead



##Cleaning----------------------------------------------------------------------
rm(list = ls())
graphics.off()
#-------------------------------------------------------------------------------
##Libraries----------------------------------------------------------------------

install.packages("PerformanceAnalytics")
library(quantmod)
library(tseries) #arima
library(lmtest)
library(car)
library(forecast) #auto.arima
library(rugarch)
library(PerformanceAnalytics)
#------------------------------------------------------------------------------
##Data loading-------------------------------------------------------------------

# Define the ticker symbol
ticker <- "NTDOY"

# Load data from Google Finance
getSymbols(ticker, src = "yahoo", from="2017-01-03", to = "2024-04-01") #in sample
nintendo.in <-NTDOY$NTDOY.Adjusted
nintendo_ts.in <- ts(nintendo.in)

getSymbols(ticker, from="2024-04-01", to = "2024-04-06") #out sample
nintendo.out <-NTDOY$NTDOY.Adjusted
nintendo_ts.out <- ts(nintendo.out)

#Note: this way of data loading is inefficient, but functional.

#-----------------------------------------------------------------------
#Explanation of each column
# NTDOY.Open: This column represents the opening price of Nintendo's stock on a 
#particular trading day. The opening price is the price at which the first trade of the day occurred.

# NTDOY.High: This column represents the highest price at which Nintendo's stock traded during the trading day.
#It indicates the peak price reached by the stock at any point during the day's trading session.

# NTDOY.Low: This column represents the lowest price at which Nintendo's stock traded during the trading day.
#It indicates the lowest price reached by the stock at any point during the day's trading session.

# NTDOY.Close: This column represents the closing price of Nintendo's stock on a particular trading day. 
#As explained earlier, the closing price is the final price at which the stock traded before the market closed for the day.

# NTDOY.Volume: This column represents the trading volume of Nintendo's stock on a particular trading day. 
#Trading volume refers to the total number of shares that were traded during the day. It indicates the level of investor interest and activity in the stock.

#NTDOY.Adjusted: This column likely represents the adjusted closing price of Nintendo's stock.
#The adjusted closing price accounts for any corporate actions such as stock splits, dividends, or other adjustments that may affect the stock's price.
#It provides a more accurate representation of the stock's performance over time, taking into account these factors
#------------------------------------------------------------------------------

