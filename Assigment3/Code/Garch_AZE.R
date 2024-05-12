#GARCH algorithm-------------------------------------------------------------
ar_range = 0:3
ma_range = 0:3
p_range = 0:4
q_range = 0:4
distributions <- c("norm", "std", "ged")
best_aic = Inf

#Beware it takes a long till finish
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
best_model <-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                       mean.model = list(armaOrder = c(3, 3), include.mean = FALSE),
                       distribution.model = "std")

best_fit <- ugarchfit(spec = best_model, data = nintendo.in_log, solver.control = list(trace = 0))

best_fit@fit$coef
#AIC:-5.1934

#Assumptions--------------------------------------------------------------------
# AR ROOTS
ar_coefs <- coef(best_fit)[grepl("ar", names(coef(best_fit)))]
ar_roots <- polyroot(c(1, -ar_coefs))
inverse_roots <- 1/Mod(ar_roots)
all(inverse_roots < 1)

# MA ROOTS
ma_coefs <- coef(best_fit)[grepl("ma", names(coef(best_fit)))]
ma_roots <- polyroot(c(1,+ma_coefs))
inverse_rootsma <- 1/Mod(ma_roots)
all(inverse_rootsma < 1)

#Visualization------------------------------------------------------------------
# some plots, muzes prohledat a vybrat nejaky fajn? 
plot(best_fit, which = 3)
plot(best_fit, which = 8)
plot(best_fit, which = 12)
plot(best_fit, which = 10)
plot(best_fit)

#residuals definition-----------------------------------------------------------
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

# Predikce-prep----------------------------------------------------------------

garch.pred = ugarchboot(best_fit, method = c("Partial", "Full")[1], n.ahead = 5, n.bootpred = 1000, n.bootfit=1000)
garch.pred@forc@forecast
print(garch.pred)
nintendo.out_log
plot(garch.pred, which = 2) # expected returns
plot(garch.pred, which = 3) # expected sigma

################################################################################
#GJR GARCH---------------------------------------------------------------------

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
best_model2 <-ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(4, 2)),
                        mean.model = list(armaOrder = c(3, 1), include.mean = FALSE),
                        distribution.model = "std")
best_fit2 <- ugarchfit(spec = best_model2, data = nintendo.in_log, solver.control = list(trace = 0))
best_fit2@fit$coef


#Assumptions--------------------------------------------------------------------
# AR ROOTS
ar_coefs2 <- coef(best_fit2)[grepl("ar", names(coef(best_fit2)))]
ar_roots2 <- polyroot(c(1, -ar_coefs2))
inverse_roots2 <- 1/Mod(ar_roots2)
all(inverse_roots2 < 1)
# MA ROOTS
ma_coefs2 <- best_fit2@fit$coef[4]
ma_roots2 <- polyroot(c(1,+ma_coefs2))
inverse_rootsma2 <- 1/Mod(ma_roots2)
all(inverse_rootsma2 < 1)

#Visualization------------------------------------------------------------------
# some plots, muzes prohledat a vybrat nejaky fajn? 
plot(best_fit2, which = 3)
plot(best_fit2, which = 2)
plot(best_fit2, which = 8)
plot(best_fit2, which = 12)


#Assumptions--------------------------------------------------------------------
#residuals definition
res2 = best_fit2@fit$z
#AIC: -5.1917

checkresiduals(res2)
# ACF +  Test na autokorel 

acf(res2, type = "correlation")

Box.test(res2, lag = floor(log(length(res2))), type = c("Ljung-Box")) # Ljung - Box test RES

# ACF +  Test na konstantni rozptyl

acf(res2^2, type = "correlation")

Box.test(res2^2, lag = floor(log(length(res2^2))), type = c("Ljung-Box")) # Ljung - Box test RES^2

# test na normalitu

jarque.bera.test(res2)


# Predikce-prep-------------------------------------------------------------------

garch.pred2 = ugarchboot(best_fit2, method = c("Partial", "Full")[1], n.ahead = 5, n.bootpred = 1000, n.bootfit=1000)
print(garch.pred2)

plot(garch.pred2, which = 2) # expected returns
plot(garch.pred2, which = 3) # expected sigma

#----------------------------------------------------------------------------------
#Additional visualization--------------------------------------------------------

# Define the specification of the ARMA+GARCH model with student's t-distributed errors
best_model <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(3, 3), include.mean = FALSE),
  distribution.model = "std"
)

# Fit the model to your data
best_fit <- ugarchfit(spec = best_model, data = nintendo.in_log, solver.control = list(trace = 0))

# Extract the standardized residuals
std_resid <- residuals(best_fit, standardize = TRUE)

# Plot the standardized residuals
ggplot(data = as.data.frame(std_resid), aes(x = index(std_resid), y = std_resid)) +
  geom_line(color = "blue") +
  labs(title = "Standardized Residuals", x = "Date", y = "Residuals") +
  theme_minimal()

plot(nintendo.in_log)

# Extract the fitted values
fitted_values <- fitted(best_fit)

# Plot the original data along with the fitted values
ggplot() +
  geom_line(data = as.data.frame(nintendo.in_log), aes(x = index(nintendo.in_log), y = nintendo.in_log), color = "blue") +
  geom_line(data = as.data.frame(fitted_values), aes(x = index(fitted_values), y = fitted_values), color = "red") +
  labs(title = "Original Data vs. Fitted Values", x = "Date", y = "Log Returns") +
  theme_minimal()

