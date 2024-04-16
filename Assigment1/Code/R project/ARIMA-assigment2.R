###Assigment 2: ARIMA processes###

##TASK
# Analyze the time series provided in the first homework using the class of (seasonal) ARIMA models: 
# (i) identify the most suitable model, 
# (ii) state its formal mathematical representation, 
# (iii) present the estimation of its parameters, 
# (iv) perform its verification, and 
# (v) predict in accordance with the previous homework assignment 
# (point and interval prediction). Compare with the results of the previous homework. 
# Which of the compared models (from the first and second homework) would you prefer 
# (and why)?

# LOADING LIBRARIES ------------------------------------------------------------
library(tseries) #arima
library(lmtest)
library(car)
library(forecast) #auto.arima

#Additional exploration-----------------------------------------------------------------------------
acf(hungary_data.in, type = "correlation") # Auto-correlation function
pacf(hungary_data.in) # Partial Auto-correlation function


#* MODEL IDENTIFICATION & ESTIMATION: AUTO.ARIMA -------------------------------
model.arma = auto.arima(hungary_data.in, seasonal = FALSE, ic = "aic", stepwise = FALSE, allowmean = FALSE, approximation=FALSE)
summary(model.arma)
#Možná zahrnout odhad pro seasonalitu? -> resp jak napasovat naši předešlou analyzu
?auto.arima

#* MODEL DIAGNOSTICS: ARMA STRUCTURE -------------------------------------------
autoplot(model.arma) # It is easier to plot the inverse roots instead, as they should all lie within the unit circle.

lag.arma = 20
acf(hungary_data.in, lag.max = lag.arma, type = "correlation", ylim = c(-1,1)) # Auto-correlation function
lines(0:(lag.arma + 1), ARMAacf(ar = model.arma$coef[1], ma = model.arma$coef[2:3], lag = lag.arma + 1), col = "red") # ar / ma to updated according to the identified model

lag.arma = 20
acf(hungary_data.in, lag.max = lag.arma, type = "partial", ylim = c(-1,1)) # Partial auto-correlation function
lines(1:lag.arma, ARMAacf(ar = model.arma$coef[1], ma = model.arma$coef[2:3], lag = lag.arma, pacf = TRUE), col = "red") # ar / ma to updated according to the identified model

#* MODEL DIAGNOSTICS: RESIDUALS ------------------------------------------------
checkresiduals(model.arma)

Box.test(model.arma$residuals^2, lag = floor(log(length(model.arma$residuals^2))), type = c("Ljung-Box")) # Ljung - Box test RES^2

jarque.bera.test(model.arma$residuals)

#* MODEL DIAGNOSTICS: PREDICTIONS ----------------------------------------------
autoplot(forecast(model.arma, h = 20))


#SARIMA PROCESSES
#* MODEL IDENTIFICATION & ESTIMATION: AUTO.ARIMA -------------------------------
model.sarima = auto.arima(hungary_data.in, seasonal = TRUE, ic = "aic", test = "adf", stepwise = FALSE, allowmean = FALSE, approximation=FALSE)
summary(model.sarima)


#* MODEL DIAGNOSTICS: ARMA STRUCTURE -------------------------------------------
autoplot(model.sarima) # It is easier to plot the inverse roots instead, as they should all lie within the unit circle.

#* MODEL DIAGNOSTICS: RESIDUALS ------------------------------------------------
checkresiduals(model.sarima)

Box.test(model.sarima$residuals^2, lag = floor(log(length(model.sarima$residuals^2))), type = c("Ljung-Box")) # Ljung - Box test RES^2

jarque.bera.test(model.sarima$residuals)

#* MODEL DIAGNOSTICS: PREDICTIONS ----------------------------------------------
autoplot(forecast(model.sarima, h = 20))