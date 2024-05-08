#Data exploration part
#3.1.2017 - 28.3.2024

#Basic characterists----------------------------------------------------------
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
autoplot(nintendo.in)
#chartSeries(NTDOY)
chartSeries(NTDOY, type = "bars", theme="white",main="Nintendo Stock")
plot(nintendo_ts.in, type='l', col=4, main="Time series Plot of price Google", xlab='Date: from February 7, 2005 to July 23, 2005', ylab='Stock')  




#---------------------------------------------------------------------------
#Stationarity test
adf.test(nintendo.in$NTDOY.Adjusted)
acf(nintendo_ts.in)
pacf(nintendo_ts.in)
acf(diff(nintendo_ts.in))
pacf(diff(nintendo_ts.in))

#Log return ->stationarize series
nintendo.in_log <- CalculateReturns(nintendo.in, method = "log")
nintendo.in_log <- na.omit(nintendo.in_log)
plot(nintendo.in_log,main='Nintendo return', xlab='Date', ylab='Log(Return)')
#classical signs of financial series ->leptokurtické, shluky of volatility + levereage
head(nintendo.in_log)
tail(nintendo.in_log)
hist(nintendo.in_log)

acf(nintendo.in_log)
pacf(nintendo.in_log)
adf.test(nintendo.in_log$NTDOY.Adjusted)
