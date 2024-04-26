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

#ACF: Pomaly pokles -> potřeba diferencování
#

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
summary(for_sarima)
for_sarima <- forecast(model.sarima,h=12)
modelARIMA_forecast <- forecast(model.arma,h=12)
modelBATS_forecast <- forecast(modelBATS, h = 12)

mod_data.out

data <- data.frame(
  Month = factor(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
  Point_Forecast =for_sarima$mean,
  Lo_95 = for_sarima$lower[,2],
  Hi_95 = for_sarima$upper[,2],
  Real_Value = mod_data.out$hun,
  BATS_forecast = modelBATS_forecast$mean,
  ARIMA_forecast = modelARIMA_forecast$mean 
  )


# Your ggplot code
gg <- ggplot(data) +
  geom_line(aes(x = as.numeric(Month), y = Point_Forecast, color = "SARIMA Forecast"), size = 1) +
  geom_line(aes(x = as.numeric(Month), y = BATS_forecast, color = "BATS Forecast"), size = 1) +
  geom_line(aes(x = as.numeric(Month), y = ARIMA_forecast, color = "ARIMA Forecast"), size = 1) +
  geom_line(aes(x = as.numeric(Month), y = Real_Value, color = "Real Value"), linetype = "solid", size = 1) +
  labs(x = "Month (2023)", y = "Unemployment (thousands)", title = "Comparison in Prediction between SARIMA, ARIMA  and BATS Models") +
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_color_manual(values = c("SARIMA Forecast" = "blue", "BATS Forecast" = "red","ARIMA Forecast" = "green", "Real Value" = "black")) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "Lines", nrow = 2),
         fill = guide_legend(title = "Intervals"))

# Save the plot
ggsave("comparison_plot.png", gg, width = 10, height = 6, dpi = 300)
#---------------------------------------------------------------------------------
library(gridExtra)

# Step 2: Modify the plots
# Assuming modelARIMA_forecast and for_sarima are the objects containing the forecasts
library(forecast)  # Assuming you used forecast package for ARIMA forecasting

# Function to modify y-axis labels
modify_y_labels <- function(p) {
  p + ylab("Unemployment (thousands)")
}

# Assuming modelARIMA_forecast and for_sarima are already generated
# Modify y-axis labels for both plots
plot1 <- modify_y_labels(autoplot(modelARIMA_forecast))
plot2 <- modify_y_labels(autoplot(for_sarima))

# Step 3: Arrange plots side by side using gridExtra
combined_plot <- grid.arrange(plot1, plot2, ncol = 2)

# Step 4: Save the combined plot
ggsave("combined_forecasts.png", combined_plot, width = 12, height = 6)

autoplot(modelARIMA_forecast)
autoplot(for_sarima)


#--------------------------------------------------------------------
# Load necessary libraries
library(forecast)
library(dplyr)

# Extract actual values from the "hun" column of mod_data.out
actual_values <- mod_data.out$hun

# Assuming you have forecasts from your models stored in vectors:
# sarima_forecast, arima_forecast, bats_forecast
sarima_forecast <- forecast(model.sarima,h=12)$mean
# SARIMA model error metrics
sarima_errors <- actual_values - sarima_forecast
sarima_mae <- mean(abs(sarima_errors))
sarima_mse <- mean(sarima_errors^2)
sarima_rmse <- sqrt(mean(sarima_errors^2))
sarima_mape <- mean(abs(sarima_errors/actual_values)) * 100
sarima_mpe <- mean(sarima_errors/actual_values) * 100
sarima_bias <- mean(sarima_errors)
sarima_mase <- mean(abs(sarima_errors) / mean(abs(actual_values - lag(actual_values, 1))))


arima_forecast <- forecast(model.arma,h=12)$mean
# ARIMA model error metrics
arima_errors <- actual_values - arima_forecast
arima_mae <- mean(abs(arima_errors))
arima_mse <- mean(arima_errors^2)
arima_rmse <- sqrt(mean(arima_errors^2))
arima_mape <- mean(abs(arima_errors/actual_values)) * 100
arima_mpe <- mean(arima_errors/actual_values) * 100
arima_bias <- mean(arima_errors)
arima_mase <- mean(abs(arima_errors) / mean(abs(actual_values - lag(actual_values, 1))))


# BATS model error metrics
bats_errors <- actual_values - bats_forecast
bats_mae <- mean(abs(bats_errors))
bats_mse <- mean(bats_errors^2)
bats_rmse <- sqrt(mean(bats_errors^2))
bats_mape <- mean(abs(bats_errors/actual_values)) * 100
bats_mpe <- mean(bats_errors/actual_values) * 100
bats_bias <- mean(bats_errors)
bats_mase <- mean(abs(bats_errors) / mean(abs(actual_values - lag(actual_values, 1))))


# Combine error metrics into a data frame
error_metrics <- data.frame(Model = c("SARIMA", "ARIMA", "BATS"),
                            MAE = c(sarima_mae, arima_mae, bats_mae),
                            MSE = c(sarima_mse, arima_mse, bats_mse),
                            RMSE = c(sarima_rmse, arima_rmse, bats_rmse),
                            MAPE = c(sarima_mape, arima_mape, bats_mape),
                            MPE = c(sarima_mpe, arima_mpe, bats_mpe),
                            Bias = c(sarima_bias, arima_bias, bats_bias),
                            MASE = c(sarima_mase, arima_mase, bats_mase))

# Print error metrics
print(error_metrics)

