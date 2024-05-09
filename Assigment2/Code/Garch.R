library(rugarch)
library(tseries) #arima
library(lmtest)
library(car)
library(forecast) #auto.arima
library(PerformanceAnalytics)
library(quantmod)

# Define the ticker symbol
ticker <- "NTDOY"

# Load data from Google Finance
getSymbols(ticker, src = "yahoo", from="2017-01-03", to = "2024-04-01") #in sample
nintendo.in <-NTDOY$NTDOY.Adjusted
nintendo_ts.in <- ts(nintendo.in)


nintendo.in_log <- CalculateReturns(nintendo.in, method = "log")
nintendo.in_log <- na.omit(nintendo.in_log)
plot(nintendo.in_log,main='Nintendo return', xlab='Date', ylab='Log(Return)')


ar_range = 0:3
ma_range = 0:3
p_range = 0:4
q_range = 0:4
distributions <- c("norm", "std", "ged")
best_aic = Inf


for (dist in distributions) {
  for (p in p_range) {
    for (q in q_range) {
      for (ar in ar_range) {
        for (ma in ma_range) {
          spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
                             mean.model = list(armaOrder = c(ar, ma), include.mean = FALSE),
                             distribution.model = dist)
          
          # Using tryCatch to handle errors and warnings
          fit_result <- tryCatch({
            ugarchfit(spec = spec, data = nintendo.in_log, solver.control = list(trace = 0))
          }, error = function(e) {
            NULL  # Returns NULL if an error occurs
          }, warning = function(w) {
            NULL  # Optionally handle warnings, here just ignore and return NULL
          })
          
          # Check if fit_result is not NULL (i.e., no error occurred)
          if (!is.null(fit_result)) {
            current_aic <- infocriteria(fit_result)[1]
            
            if (!is.na(current_aic) && current_aic < best_aic) {
              best_aic <- current_aic
              best_model <- list(ar = ar, ma = ma, p = p, q = q, dist = dist)
              best_spec <- spec
              best_fit <- fit_result
            }
          }
        }
      }
    }
  }
}

# Coefficients, podle me omega je u cipry alfa_0
best_fit@fit$coef

# some plots, muzes prohledat a vybrat nejaky fajn? 
plot(best_fit, which = 3)
plot(best_fit, which = 8)
plot(best_fit, which = 12)

#residuals definition
res = best_fit@fit$z

checkresiduals(res)

# ACF +  Test na autokorel 

acf(res, type = "correlation",main = "Residuals ARMA-GARCH")

Box.test(res, lag = floor(log(length(res^2))), type = c("Ljung-Box")) # Ljung - Box test RES

# ACF +  Test na konstantni rozptyl

acf(res^2, type = "correlation")

Box.test(res^2, lag = floor(log(length(res^2))), type = c("Ljung-Box")) # Ljung - Box test RES^2

# test na normalitu

jarque.bera.test(res)


# Predikce

garch.pred = ugarchboot(best_fit, method = c("Partial", "Full")[1], n.ahead = 5, n.bootpred = 1000, n.bootfit=1000)
print(garch.pred)

plot(garch.pred, which = 2) # expected returns
plot(garch.pred, which = 3) # expected sigma


# gjr garch 

ar_range2 = 0:3
ma_range2 = 0:3
p_range2 = 0:4
q_range2 = 0:4
distributions2 <- c("norm","std","ged")
best_aic = Inf

for (dist in distributions2) {
  for (p in p_range2) {
    for (q in q_range2) {
      for (ar in ar_range2) {
        for (ma in ma_range2) {
          spec <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(p, q)),
                             mean.model = list(armaOrder = c(ar, ma), include.mean = FALSE),
                             distribution.model = dist)
          
          # Using tryCatch to handle errors and warnings for both fitting and AIC calculation
          fit_result <- tryCatch({
            fitted_model <- ugarchfit(spec = spec, data = nintendo.in_log, solver.control = list(trace = 0))
            aic_value <- infocriteria(fitted_model)[1]  # AIC calculation here
            list(fitted_model = fitted_model, aic = aic_value)
          }, error = function(e) {
            NULL  # Returns NULL if an error occurs
          }, warning = function(w) {
            NULL  # Optionally handle warnings, here just ignore and return NULL
          })
          
          # Check if fit_result is not NULL (i.e., no error occurred) and AIC was successfully retrieved
          if (!is.null(fit_result) && !is.na(fit_result$aic) && fit_result$aic < best_aic) {
            best_aic2 <- fit_result$aic
            best_model2 <- list(ar = ar, ma = ma, p = p, q = q, dist = dist)
            best_spec2 <- spec
            best_fit2 <- fit_result$fitted_model
          }
        }
      }
    }
  }
}



# Coefficients, podle me omega je u cipry alfa_0
best_fit2@fit$coef

# some plots, muzes prohledat a vybrat nejaky fajn? 
plot(best_fit2, which = 3)
plot(best_fit2, which = 8)
plot(best_fit2, which = 12)

#residuals definition
res2 = best_fit2@fit$z

checkresiduals(res2)
# ACF +  Test na autokorel 

acf(res2, type = "correlation")

Box.test(res2, lag = floor(log(length(res2^2))), type = c("Ljung-Box")) # Ljung - Box test RES

# ACF +  Test na konstantni rozptyl

acf(res2^2, type = "correlation")

Box.test(res2^2, lag = floor(log(length(res2^2))), type = c("Ljung-Box")) # Ljung - Box test RES^2

# test na normalitu

jarque.bera.test(res2)


# Predikce

garch.pred2 = ugarchboot(best_fit2, method = c("Partial", "Full")[1], n.ahead = 5, n.bootpred = 1000, n.bootfit=1000)
print(garch.pred2)

plot(garch.pred2, which = 2) # expected returns
plot(garch.pred2, which = 3) # expected sigma
