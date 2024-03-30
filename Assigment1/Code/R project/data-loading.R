#This part is for data loading and basic data exploration
#ABE: Please do not delete or change mindlessly lines or we get nasty conflicts in GIT

# CleaningWS ------------------------------------------------------------------
rm(list = ls())[]
graphics.off()
#ABE: do not forgo to set directory to the correct path!

#Libraries--------------------------------------------------------------
library(tseries)
library(forecast)
library(quantmod)
library(lmtest)
library(sandwich)
library(car)
library(openxlsx)
library(fpp3)
#install.packages('fpp3')
#Data loading-----------------------------------------------------------
hungary_data <- t(read.xlsx("Data/une_rt_m.xlsx", sheet = "Hungary",na.strings = ":"))#transpose, so we can use ts fun properly

colnames(hungary_data) <- c("hun")
hungary_data <- apply(hungary_data, 2, as.numeric)
hungary_data <- ts(hungary_data, frequency = 12, start = c(1983, 0))


#in-sample
hungary_data.in <- window(hungary_data, start = c(2010, 1), end = c(2022, 12))
mod_data.in <-  data.frame(hun = hungary_data.in, t = seq_along(hungary_data.in))
#out-sample
hungary_data.out <- window(hungary_data, start = c(2023, 1), end = c(2023,12))
mod_data.out <- data.frame(hun = hungary_data.out, t =seq_along(hungary_data.out))
#Check
start(hungary_data)
end(hungary_data)
frequency(hungary_data)

# Check for NA values in the dataset
na_values <- is.na(hungary_data)
na_values <- is.na(hungary_data.in) #no NA in our in-sample
print(which(na_values, arr.ind = TRUE))

#Data exploration-------------------------------------------------------
View(hungary_data)
View(hungary_data.in)
View(hungary_data.out)
View(mod_data.in)
View(mod_data.out)
summary(hungary_data)
summary(hungary_data.in)
hist(hungary_data)
hist(hungary_data.in)
plot(hungary_data)
plot(hungary_data.in)
#Data visualization-------------------------
autoplot(hungary_data)
autoplot(hungary_data.in)
ggseasonplot(hungary_data.in) #seasonal months
ggsubseriesplot(hungary_data)

Acf(hungary_data)
plot(decompose(hungary_data.in))


#Additional data visualization-----------------------------------------------

gg_season(as_tsibble(hungary_data.in), period = "year", labels = "both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Hungarian Data")

gg_subseries(as_tsibble(hungary_data.in))

gg_lag(as_tsibble(hungary_data.in), geom = "point") +
  labs(x = "lag(Beer, k)")

#Trend identification --------------------------------------------------------

#Linear -> NOP
plot(diff(hungary_data.in)) #not constant-> not linear trend

#Quadratic -> NOP
plot(diff(diff(hungary_data.in))) #not constant
#Exponential -> NOP
plot(diff(log(hungary_data.in))) #Not constant 

#Logistic -> Maybe
hist(diff(log(hungary_data.in))) #should be approx normal






#Modified exponential TODO - MAYBE
mod_exp =ts(diff(hungary_data.in)[1:154]/diff(hungary_data.in)[-1])
plot(ts(diff(hungary_data.in)[1:154]/diff(hungary_data.in)[-1]))

# Calculate first differential
first_diff <- diff(hungary_data.in)

# Calculate second differential
second_diff <- diff(first_diff)
first_diff_shifted <- first_diff[-1]


# Calculate the ratio of two differences
ratio_diff <- second_diff / first_diff_shifted

# Plot the ratio of two differences
plot(ratio_diff, type='l', main='Ratio of Differences Plot', xlab='Time', ylab='Ratio of Differences')


#Gomperz -MAYBE

# Calculate first differential
first_diff_log <- diff(log(hungary_data.in))

# Calculate second differential
second_diff_log <- diff(first_diff_log)
first_diff_shifted_log <- first_diff_log[-1]


# Calculate the ratio of two differences
ratio_diff_log <- second_diff_log / first_diff_shifted_log

# Plot the ratio of two differences
plot(ratio_diff_log, type='l', main='Ratio of Log Differences Plot', xlab='Time', ylab='Ratio of Differences')

nrow(hungary_data)
