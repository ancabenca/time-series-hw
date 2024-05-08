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

#install.packages("quantmod")
library(quantmod)
library(tseries) #arima
library(lmtest)
library(car)
library(forecast) #auto.arima
library(rugarch)

#------------------------------------------------------------------------------
##Data loading-------------------------------------------------------------------

# Define the ticker symbol
ticker <- "NTDOY"

# Load data from Google Finance
getSymbols(ticker, src = "yahoo")

# View loaded data
View(NTDOY)

#------------------------------------------------------------------------------