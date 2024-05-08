### ------------------------------------------------------------------------ ###
### NMST414 Time Series (seminar): 07 [24th April 2024] -------------------- ###
### Section 1: ARMA-GARCH processes ---------------------------------------- ###
### ------------------------------------------------------------------------ ###


# CLEANING WS ------------------------------------------------------------------
rm(list = ls())
graphics.off()


# LOADING LIBRARIES ------------------------------------------------------------
library(tseries) #arima
library(lmtest)
library(car)
library(forecast) #auto.arima
library(rugarch)


### ------------------------------------------------------------------------ ###
# SECTION 1: ARMA-GARCH processes ----------------------------------------------

#* EMPIRICAL DATA --------------------------------------------------------------
data("sp500ret") # SP500 index closing value log return from 1987-03-10 to 2009-01-30 from Yahoo Finance
# data("dji30ret") # Dow Jones 30 Constituents closing value log returns from 1987-03-16 to 2009-02-03 from Yahoo Finance

y = sp500ret$SP500RET
# y = dji30ret$...

#* EXPLORING DATA --------------------------------------------------------------
summary(y) 
plot(y, type = "l")

#* MODEL IDENTIFICATION: GRAPHICS ----------------------------------------------
acf(y, type = "correlation") # Auto-correlation function
pacf(y) # Partial Auto-correlation function

#* MODEL IDENTIFICATION & ESTIMATION: AUTO.ARIMA -------------------------------
model.arima = auto.arima(y, d = 0, D = 0, seasonal = FALSE, ic = "aic", test = "adf", stepwise = FALSE, allowmean = TRUE, approximation=FALSE)
summary(model.arima)

checkresiduals(model.arima)
Box.test(model.arima$residuals^2, lag = floor(log(length(model.arima$residuals^2))), type = c("Ljung-Box")) # Ljung - Box test RES^2

#* MODEL IDENTIFICATION & ESTIMATION: ARMA-GARCH -------------------------------
model.arma.garch.spec = ugarchspec(variance.model = list(model = "sGARCH", 
                                                         garchOrder = c(1, 1), 
                                                         submodel = NULL, 
                                                         external.regressors = NULL, 
                                                         variance.targeting = FALSE), 
                                   
                                   mean.model = list(armaOrder = c(4, 0),
                                                     include.mean = FALSE,
                                                     external.regressors = NULL),
                                   
                                   distribution.model = "norm", 
                                   start.pars = list(), 
                                   fixed.pars = list())

print(model.arma.garch.spec)

model.arma.garch = ugarchfit(spec = model.arma.garch.spec, data = y, solver.control = list(trace=0))

print(model.arma.garch)

# ---------------------------------------------------------------------------- #
# Sign Bias Test is used to test leverage effect in the standardized residuals. (Null: no significiant negative and positive reaction shocks (if exist apARCH type models))
# 
# The Nyblom stability test provides a means of testing for structural change within a time series. A structural change implies that the relationship between variables changes overtime e.g. for the regression y=�x
# beta changes over time. (Null: the parameter values are constant i.e. zero variance, the alternative hypothesis is that their variance > 0.) (Reject H0, if Test Stat > CV.)
# 
# Adjusted Pearson Goodness-of-Fit Test calculates the chi-squared goodness of fit test, which compares the empirical distribution of the standardized residuals with the theoretical ones from the chosen density.
# ---------------------------------------------------------------------------- #

model.arma.garch@fit$coef

plot(model.arma.garch, which = 3)
plot(model.arma.garch, which = 8)
plot(model.arma.garch, which = 12)

model.arma.garch.res = model.arma.garch@fit$z

#* MODEL DIAGNOSTICS: RESIDUALS ------------------------------------------------
acf(model.arma.garch.res, type = "correlation")

Box.test(model.arma.garch.res, lag = floor(log(length(model.arima$residuals^2))), type = c("Ljung-Box")) # Ljung - Box test RES

acf(model.arma.garch.res^2, type = "correlation")

Box.test(model.arma.garch.res^2, lag = floor(log(length(model.arma.garch.res^2))), type = c("Ljung-Box")) # Ljung - Box test RES^2

jarque.bera.test(model.arma.garch.res)

#* MODEL PREDICTION ------------------------------------------------------------
model.arma.garch.pred = ugarchboot(model.arma.garch, method = c("Partial", "Full")[1], n.ahead = 5, n.bootpred = 1000, n.bootfit=1000)
print(model.arma.garch.pred)

plot(model.arma.garch.pred, which = 2) # expected returns
plot(model.arma.garch.pred, which = 3) # expected sigma

# REFERENCES -------------------------------------------------------------------
# https://www.quantstart.com/articles/ARIMA-GARCH-Trading-Strategy-on-the-SP500-Stock-Market-Index-Using-R/