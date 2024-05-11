#Data exploration part
#3.1.2017 - 28.3.2024

#Basic characterists----------------------------------------------------------
nrow(nintendo.in_log)
nrow(nintendo.in)
head(nintendo_ts.in)
head(nintendo.in)

tail(nintendo_ts.in)
tail(nintendo.in)

stat.desc(nintendo.in)
summary(nintendo.in)
str(nintendo.in)

#něco o tom indexu, z jaké OTC to je, jaká měna
#Visualization------------------------------------------------------------
plot(nintendo.in)
plot(nintendo_ts.in)
hist(nintendo_ts.in)


timeSeries <- autoplot(nintendo.in) +
  ylab("NTDOY (in $)") +
  xlab("Time") +  # Set a minimalistic theme
  theme(axis.text = element_text(color = "black"),  # Set axis text color
        axis.title = element_text(color = "black"), # Set axis title color
        plot.title = element_text(hjust = 0.5))  
ggsave("TimeSeries.png",timeSeries, width = 12, height = 6)




?autoplot
#chartSeries(NTDOY)
chartSeries(NTDOY, type = "bars", theme="white",main="Nintendo Stock")
plot(nintendo_ts.in, type='l', col=4, main="Time series Plot of price Google", xlab='Date: from February 7, 2005 to July 23, 2005', ylab='Stock')  
chart_Series()



#---------------------------------------------------------------------------
#Stationarity test
adf.test(nintendo.in$NTDOY.Adjusted)
acfOriginal <- acf(nintendo_ts.in)
acfLog <- acf(nintendo.in_log)



acf(diff(nintendo_ts.in))
pacf(diff(nintendo_ts.in))

#Log return ->stationarize series
nintendo.in_log <- CalculateReturns(nintendo.in, method = "log")
nintendo.in_log <- na.omit(nintendo.in_log)
plot(nintendo.in_log,main='Nintendo return', xlab='Date', ylab='Log(Return)')

nintendo.out_log <- CalculateReturns(nintendo.out, method = "log")
nintendo.out_log <- na.omit(nintendo.out_log)

#classical signs of financial series ->leptokurtické, shluky of volatility + levereage
head(nintendo.in_log)
tail(nintendo.in_log)
hist(nintendo.in_log, breaks  = 50)
?hist
acf(nintendo.in_log)
pacf(nintendo.in_log)
adf.test(nintendo.in_log$NTDOY.Adjusted)

timeSeriesLog <- autoplot(nintendo.in_log) +
  ylab("NTDOY (in $)") +
  xlab("Time") +  # Set a minimalistic theme
  theme(axis.text = element_text(color = "black"),  # Set axis text color
        axis.title = element_text(color = "black"), # Set axis title color
        plot.title = element_text(hjust = 0.5))  
ggsave("TimeSeriesLog.png",timeSeriesLog, width = 12, height = 6)




acfOriginal <- acf(nintendo_ts.in, main = "ACF of Original Series Y_t")
acfLog <- acf(nintendo.in_log, main = "ACF of Log Returns P_t")

TimeSeriesACF <- arrangeGrob(acfOriginal, acfLog, nrow=1, ncol=2)

# Save the plot to a file
ggsave("TimeSeriesACF.png", plot = TimeSeriesACF, width = 12, height = 6)



# Load necessary libraries
library(rugarch)
library(ggplot2)

# Assuming you have stored your original data in nintendo.out_log[,1]
original_data <- nintendo.out_log[,1]

# Extract predicted values
predicted_values <- garch.pred@forc@forecast$seriesFor
predicted_sigma <- garch.pred@forc@forecast$sigmaFor

predicted_values2 <- garch.pred2@forc@forecast$seriesFor
predicted_sigma2 <- garch.pred2@forc@forecast$sigmaFor

# Create a dataframe for plotting
plot_data <- data.frame(
  Date = index(original_data),
  Original_Data = original_data,
  Predicted_Data = predicted_values,
  Predicted_Sigma = predicted_sigma,
  Predicted_Data2 = predicted_values2,
  Predicted_sigma2 = predicted_sigma2
)

colnames(plot_data)[2] <- "Original_Data"
colnames(plot_data)[3] <- "Predicted_Data"
colnames(plot_data)[4] <- "Predicted_Sigma"
colnames(plot_data)[5] <- "Predicted_Data2"
colnames(plot_data)[6] <- "Predicted_Sigma2"

# Plot
ggplot(data = plot_data, aes(x = Date)) +
  geom_line(aes(y = Original_Data, color = "Original Data")) +
  geom_line(aes(y = Predicted_Data, color = "Predicted Data")) +
  geom_line(aes(y = Predicted_Sigma, color = "Predicted Sigma")) +
  geom_line(aes(y = Predicted_Data2, color = "Predicted Data2")) + 
  geom_line(aes(y = Predicted_Sigma2, color = "Predicted Sigma2")) + 
labs(title = "(GJR)GARCH Prediction vs Log Returns P_t",
       y = "Log Returns",
       color = "Data Type") +
  theme_minimal()

plot(garch.pred, which =2)
