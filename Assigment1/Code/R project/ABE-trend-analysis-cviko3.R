#Cviko 3 - trend identification
#Pomocný skript k identifikaci správného trendu



#* MODEL: Y = Tr + E with Tr = a0 + a1*t + a2*t^2 ------------------------------

#*** Model declaration ---------------------------------------------------------

model = lm(hun ~ t + I(t^2), data = mod_data.in)

summary(model) 

par(mfrow = c(1,1))
plot(mod_data.in$hun, lwd = 2)
lines(mod_data.in$t, predict(model), col = "red") 

par(mfrow = c(1,1))
plot(residuals(model), ylab = "Residuals") 
par(mfrow = c(2,2))
plot(model) 
acf(residuals(model)) 
#*** Model characteristics -----------------------------------------------------
model.chars = matrix(NA, 1, 7)
colnames(model.chars) = c("R2", "R2.adj", "AIC", "RMSE(in)", "MAE(in)", "RMSE(out)", "MAE(out)")
rownames(model.chars) = c("model")

model.chars[1,1] = summary(model)$r.squared # R2
model.chars[1,2] = summary(model)$adj.r.squared # Adjusted R2
model.chars[1,3] = AIC(model) # AIC
model.chars[1,4] = sqrt(mean(residuals(model)^2)) # RMSE in
model.chars[1,5] = mean(abs(residuals(model))) # MAPE in
model.chars[1,6] = sqrt(mean(((mod_data.out$hun - predict(model, mod_data.out)))^2)) # RMSE out
model.chars[1,7] = mean(abs(((mod_data.out$hun - predict(model, mod_data.out))))) # MAPE out

print(model.chars)
#------------------------------------------------------------------
#More efficient variant
# Assuming 'model' is already fitted and 'mod_data.out' is available

# Initialize the model.chars matrix
model.chars <- matrix(NA, 1, 7, dimnames = list("model", c("R2", "R2.adj", "AIC", "RMSE(in)", "MAE(in)", "RMSE(out)", "MAE(out)")))

# Populate the matrix with model performance metrics
model.chars[1, 1:3] <- c(
  summary(model)$r.squared,         # R2
  summary(model)$adj.r.squared,     # Adjusted R2
  AIC(model)                         # AIC
)

# Calculate and add RMSE and MAE for in-sample
in_sample_residuals <- residuals(model)
model.chars[1, 4:5] <- c(
  sqrt(mean(in_sample_residuals^2)), # RMSE in
  mean(abs(in_sample_residuals))    # MAE in
)

# Calculate and add RMSE and MAE for out-of-sample
out_sample_residuals <- mod_data.out$hun - predict(model, mod_data.out)
model.chars[1, 6:7] <- c(
  sqrt(mean(out_sample_residuals^2)), # RMSE out
  mean(abs(out_sample_residuals))    # MAE out
)

print(model.chars)
#*** Model diagnostics 1: Basic graphs -----------------------------------------
par(mfrow = c(2,2))
plot(model)

# Graph 1: The residuals vs fitted plot is mainly useful for investigating:
# --- the linearity assumption: This is indicated by the mean residual value for every fitted value region being close to 0. This is shown by the red line is approximate to the dashed line in the graph.
# --- the homoskedasticity assumptions: If spread of residuals is approximately the same across the x-axis.
# --- the presence of outliers: This indicated by some �extreme� residuals that are far from the other residuals points.

# Graph 2: The Q-Q plot, or quantile-quantile plot, is a graphical tool to help us assess if a set of data plausibly came from some theoretical distribution (e.g., Normal).

# Graph 3: The scale-location plot is very similar to residuals vs fitted, but plot the square root Standardized residuals vs fitted values to verify homoskedasticity assumption. We want to look at:
# --- the red line: The red line represent the average the standardized residuals and must be approximately horizontal. If the line approximately horizontal and magnitude of the line hasn�t much fluctuations in the line, it means the average of the standardized residuals approximately same.
# --- variance around the red line: The spread of standardized residuals around the red line doesn't vary with respect to the fitted values, means the variance of standardized residuals due to each fitted value is approximately the same not much fluctuations in the variance.

# Graph 4: The Residuals vs. Leverage plots helps to identify influential data points on the model. Outliers can be influential, though they don�t necessarily have to it and some points within a normal range in your model could be very influential.
# --- Outliers: They are defined as an observation that has a large residual. In other words, the observed value for the point is very different from that predicted by the regression model.
# --- Leverage observations: They are defined as an observation that has a value of x that is far away from the mean of x.
#                            [leverage score (i.e., the particular diagonal element of the hat matrix) can be viewed as the 'weighted' distance between x_{i} to the mean of x_{i}'s]        
# --- Influential observations: They are defined as an observation that changes the slope of the line. Thus, influential points have a large influence on the fit of the model. One method to find influential points is to compare the fit of the model with and without each observation.
#                               [DFFITS, DFBETAS, Cook's distance] 

#*** Model diagnostics 2: Heteroskedasticity tests -----------------------------
bptest(model, data = mod_data.in) # Breusch - Pagan test: E^2 = a0 + a1 * t + a2 * t^2

bptest(model, ~ t + I(t^2) + I(t^3), data = mod_data.in) # White test: E^2 = a0 + a1 * t + a2 * t^2 + a3 * t * t^2

help("bptest")

Box.test(residuals(model)^2, lag = 4, type = c("Ljung-Box")) # Ljung - Box test on squared residuals ##test nekorelovanosti: zamitami hypotezu
help("Box.test") # lag = log(T) or in accordance with freq of observations

par(mfrow = c(1,1))
acf(residuals(model)^2)

#*** Model diagnostics 3: Autocorrelation --------------------------------------
par(mfrow = c(1,1))
plot(residuals(model)[1:(length(residuals(model))-1)], residuals(model)[2:length(residuals(model))], xlab = "resid(-1)", ylab = "resid")
#závislost 
acf(residuals(model)) # Autocorrelation function

dwt(model) # Durbin - Watson test
#0 neavdi -> technická implementace
#zamitame nulovou hypotezu¨
?dwt()
# sum((residuals(model)[2:length(residuals(model))] - residuals(model)[1:(length(residuals(model))-1)])^2) / sum(residuals(model)^2)



Box.test(residuals(model), lag = 4, type = c("Ljung-Box")) # Ljung - Box test

bgtest(model, order = 4) # Breusch - Godfrey test E = a0 + a1 * t + a2 * t^2 + beta1 * E(-1) + ... + beta4 * E(-4), H0: beta1 = ... = beta4 = 0

#*** Model diagnostics 4: Normality --------------------------------------------
shapiro.test(residuals(model)) # Shapiro - Wilk test

jarque.bera.test(residuals(model)) # Jarque - Bera test

#* model2: Y = Tr + E with Tr = a0 + a1*log(t) ------------------------------

#*** Model declaration ---------------------------------------------------------
model2 = lm(hun ~ log(t), data = mod_data.in) #quadratic curve?

summary(model2) #highly significant

par(mfrow = c(1,1))
plot(mod_data.in$hun, lwd = 2)
lines(mod_data.in$t, predict(model2), col = "red") #terrible fit

par(mfrow = c(1,1))
plot(residuals(model2), ylab = "Residuals") #naprosto nevhodné
par(mfrow = c(2,2))
plot(model2) 
acf(residuals(model2)) # highly correlated -> not the best model2
#*** model2 characteristics -----------------------------------------------------
model2.chars = matrix(NA, 1, 7)
colnames(model2.chars) = c("R2", "R2.adj", "AIC", "RMSE(in)", "MAE(in)", "RMSE(out)", "MAE(out)")
rownames(model2.chars) = c("model2")

model2.chars[1,1] = summary(model2)$r.squared # R2
model2.chars[1,2] = summary(model2)$adj.r.squared # Adjusted R2
model2.chars[1,3] = AIC(model2) # AIC
model2.chars[1,4] = sqrt(mean(residuals(model2)^2)) # RMSE in
model2.chars[1,5] = mean(abs(residuals(model2))) # MAPE in
model2.chars[1,6] = sqrt(mean(((mod_data.out$hun - predict(model2, mod_data.out)))^2)) # RMSE out
model2.chars[1,7] = mean(abs(((mod_data.out$hun - predict(model2, mod_data.out))))) # MAPE out

print(model2.chars)
#------------------------------------------------------------------
#More efficient variant
# Assuming 'model2' is already fitted and 'mod_data.out' is available

# Initialize the model2.chars matrix
model2.chars <- matrix(NA, 1, 7, dimnames = list("model2", c("R2", "R2.adj", "AIC", "RMSE(in)", "MAE(in)", "RMSE(out)", "MAE(out)")))

# Populate the matrix with model2 performance metrics
model2.chars[1, 1:3] <- c(
  summary(model2)$r.squared,         # R2
  summary(model2)$adj.r.squared,     # Adjusted R2
  AIC(model2)                         # AIC
)

# Calculate and add RMSE and MAE for in-sample
in_sample_residuals <- residuals(model2)
model2.chars[1, 4:5] <- c(
  sqrt(mean(in_sample_residuals^2)), # RMSE in
  mean(abs(in_sample_residuals))    # MAE in
)

# Calculate and add RMSE and MAE for out-of-sample
out_sample_residuals <- mod_data.out$hun - predict(model2, mod_data.out)
model2.chars[1, 6:7] <- c(
  sqrt(mean(out_sample_residuals^2)), # RMSE out
  mean(abs(out_sample_residuals))    # MAE out
)

print(model2.chars)
#*** model2 diagnostics 1: Basic graphs -----------------------------------------
par(mfrow = c(2,2))
plot(model2)

#*** model2 diagnostics 2: Heteroskedasticity tests -----------------------------
bptest(model2, data = mod_data.in) # Breusch - Pagan test: E^2 = a0 + a1 * t + a2 * t^2
#zamitame hypotezu -> + graph: data nejsu homo
bptest(model2, ~ t + I(t^2) + I(t^3), data = mod_data.in) # White test: E^2 = a0 + a1 * t + a2 * t^2 + a3 * t * t^2

help("bptest")

Box.test(residuals(model2)^2, lag = 4, type = c("Ljung-Box")) # Ljung - Box test on squared residuals ##test nekorelovanosti: zamitami hypotezu
help("Box.test") # lag = log(T) or in accordance with freq of observations

par(mfrow = c(1,1))
acf(residuals(model2)^2) #první tři vysoce korelované
#??? why squared, we never used it like this before

#*** model2 diagnostics 3: Autocorrelation --------------------------------------
par(mfrow = c(1,1))
plot(residuals(model2)[1:(length(residuals(model2))-1)], residuals(model2)[2:length(residuals(model2))], xlab = "resid(-1)", ylab = "resid")
#závislost 
acf(residuals(model2)) # Autocorrelation function

dwt(model2) # Durbin - Watson test
#0 neavdi -> technická implementace
#zamitame nulovou hypotezu¨
?dwt()
# sum((residuals(model2)[2:length(residuals(model2))] - residuals(model2)[1:(length(residuals(model2))-1)])^2) / sum(residuals(model2)^2)



Box.test(residuals(model2), lag = 4, type = c("Ljung-Box")) # Ljung - Box test

bgtest(model2, order = 4) # Breusch - Godfrey test E = a0 + a1 * t + a2 * t^2 + beta1 * E(-1) + ... + beta4 * E(-4), H0: beta1 = ... = beta4 = 0

#*** model2 diagnostics 4: Normality --------------------------------------------
shapiro.test(residuals(model2)) # Shapiro - Wilk test

jarque.bera.test(residuals(model2)) # Jarque - Bera test


#* MODEL3: Y = Tr + E with Tr = a0 + a1*t ------------------------------

#*** Model declaration ---------------------------------------------------------
model3 = lm(hun ~ t, data = mod_data.in) 

summary(model3) 

par(mfrow = c(1,1))
plot(mod_data.in$hun, lwd = 2)
lines(mod_data.in$t, predict(model3), col = "red") 

par(mfrow = c(1,1))
plot(residuals(model3), ylab = "Residuals") 
plot(model3) 
acf(residuals(model3)) # 
#*** model3 characteristics -----------------------------------------------------
model3.chars = matrix(NA, 1, 7)
colnames(model3.chars) = c("R2", "R2.adj", "AIC", "RMSE(in)", "MAE(in)", "RMSE(out)", "MAE(out)")
rownames(model3.chars) = c("model3")

model3.chars[1,1] = summary(model3)$r.squared # R2
model3.chars[1,2] = summary(model3)$adj.r.squared # Adjusted R2
model3.chars[1,3] = AIC(model3) # AIC
model3.chars[1,4] = sqrt(mean(residuals(model3)^2)) # RMSE in
model3.chars[1,5] = mean(abs(residuals(model3))) # MAPE in
model3.chars[1,6] = sqrt(mean(((data.hun.out$hun - predict(model3, data.hun.out))[129:136])^2)) # RMSE out
model3.chars[1,7] = mean(abs(((data.hun.out$hun - predict(model3, data.hun.out))[129:136]))) # MAPE out

print(model3.chars)

#Warning I used nls which I have no experience wiht, I would rather avoid Gomperz or Logistic trend
#Other models
#* MODEL4: Y = Tr + E with ln(Tr) = a0 + a1*beta^t ------------------------------
#* # Define the Gompertz function
gompertz <- function(t, gamma, alpha, beta) {
  gamma + alpha * beta^t
}

# Take the natural logarithm of 'hun'
mod_data.in$ln_hun <- log(mod_data.in$hun)
initial_values <- list(gamma = 0, alpha = 1, beta = 0.95)
# Fit the Gompertz model
model4 <- nls(ln_hun ~ gompertz(t, gamma, alpha, beta), 
           data = mod_data.in,
           start = initial_values)

# Print the summary of the model
summary(model4)
plot(model4)
par(mfrow = c(1,1))
plot(mod_data.in$hun, lwd = 2)
lines(mod_data.in$t, exp(predict(model4)), col = "red") 



#* MODEL5: Y = Tr + E with Tr = a0/(1-a1*beta^t) ------------------------------


# Define the logistic function
logistic <- function(t, a0, a1, beta) {
  a0 / (1 - a1 * beta^t)
}

# Assuming 'mod_data.in' is your data frame with the 'hun' column

# Providing initial parameter values
initial_values2 <- list(a0 = 1000, a1 = 0.5, beta = 0.95)  # Adjust these values as needed

# Fit the logistic model
model5 <- nls(hun ~ logistic(t, a0, a1, beta), 
           data = mod_data.in,
           start = initial_values2)

# Print the summary of the model
summary(model5)
plot(model5)
par(mfrow = c(1,1))
plot(mod_data.in$hun, lwd = 2)
lines(mod_data.in$t, predict(model5), col = "red") 

