#This part is for data loading and basic data exploration
#ABE: Please do not delete or change mindlessly lines or we get nasty conflicts in GIT

# CleaningWS ------------------------------------------------------------------
rm(list = ls())
graphics.off()
#ABE: do not forgo to set directory to the correct path!

#Libraries--------------------------------------------------------------
library(tseries)
library(forecast)
library(quantmod)
library(lmtest)
library(car)
library(openxlsx)
#install.packages('openxlsx')

#Data loading-----------------------------------------------------------
hungary_data <- t(read.xlsx("Data/une_rt_m.xlsx", sheet = "Hungary",na.strings = ":"))#transpose, so we can use ts fun properly
colnames(hungary_data) <- c("hungary")
hungary_data <- apply(hungary_data, 2, as.numeric)
hungary_data <- ts(hungary_data, frequency = 12, start = c(1983, 1))


#in-sample
hungary_data.in <- window(hungary_data, start = c(2010, 1), end = c(2022, 12))

#out-sample
hungary_data.out <- window(hungary_data, start = c(2023, 1), end = c(2023, 12))

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

summary(hungary_data)
hist(hungary_data)
plot(hungary_data)

#Data visualization-------------------------
autoplot(hungary_data)
ggseasonplot(hungary_data) #seasonal months
ggsubseriesplot(hungary_data)

Acf(hungary_data)
plot(decompose(hungary_data.in))
plot(diff(hungary_data.in)) #fluctuation of unemployemnt
