#ABE: this is your playground, I won't change anything, but may take a peak, how did you dealt with the problem, so pls at least keep some struct


# TASK 1 

data.hungary.in = data.frame(hun = hungary_data.in, t = seq_along(hungary_data.in), seas.dummy = as.factor(cycle(hungary_data.in)))
data.hungary.out = data.frame(hun = hungary_data.out, t = seq_along(hungary_data.out), seas.dummy = as.factor(cycle(hungary_data.out)))


plot(hungary_data.in)
# We see that data have nice "inverse S shape", that is not typical for our general trend choices, that is why we will try
# to catch the trend using polynomial of the third order.

model = lm(hun ~  + I(t^2) + I(t^3),data=data.hungary.in)


plot(mod_data.in$hun, lwd = 2)
lines(mod_data.in$t, predict(model), col = "red")

# maybe diagnostics?  plot(model), ale asi bych udelal az s hotovym modelem
# summary(model)

# We see that our model estimated the trend very nicely. We want to capture seasonality aswell. So we will try the dummy
# variables approach


model_s = lm(hun ~  + I(t^2) + I(t^3)+ seas.dummy, data= data.hungary.in)


par(mfrow = c(1,1))
plot(hungary_data.in, lwd = 2)
lines(ts(predict(model_s), start = start(hungary_data.in), frequency = frequency(hungary_data.in)), col = "red")


plot(model_s)
checkresiduals(model_s)
# Model1 assumption E[eps] = 0 and var[eps] = sigma ^2 doesnt seem to be violated. 

summary(model_s)

#Lets check model performance 
model.s.chars = matrix(NA, 1, 5)#7
colnames(model.s.chars) = colnames(model.s.chars) = c("R2", "R2.adj", "AIC", "RMSE(in)", "MAE(in)") #"RMSE(out)", "MAE(out)"
rownames(model.s.chars) = c("model_s")

model.s.chars[1,1] = summary(model_s)$r.squared # R2
model.s.chars[1,2] = summary(model_s)$adj.r.squared # Adjusted R2
model.s.chars[1,3] = AIC(model_s) # AIC
model.s.chars[1,4] = sqrt(mean(residuals(model_s)^2)) # RMSE in
model.s.chars[1,5] = mean(abs(residuals(model_s))) # MAPE in
#model.s.chars[1,6] = sqrt(mean(((hungary_data.out - predict(model_s, data.hungary.out)))^2)) # RMSE out
#model.s.chars[1,7] = mean(abs(((hungary_data.out - predict(model_s, data.hungary.out)))))
print(model.s.chars)

#Task 3

model2 = ets(hungary_data.in, model = "ZZZ", opt.crit = "lik", ic = "aic")
summary(model2)
par(mfrow = c(1,1))
plot(model2)

checkresiduals(model2)

acf(residuals(model2)) # Autocorrelation function
Box.test(residuals(model2), lag = floor(log(length(residuals(model2)))), type = c("Ljung-Box")) # Ljung - Box test

acf(residuals(model2)^2) # Autocorrelation function RES^2
Box.test(residuals(model2)^2, lag = floor(log(length(residuals(model2)))), type = c("Ljung-Box")) # Ljung - Box test RES^2

jarque.bera.test(residuals(model2)) # Jarque - Bera test

#Task 5 

model3 = tbats(hungary_data.in)

model3

par(mfrow = c(1,1))
plot(model3)


checkresiduals(model3)

acf(residuals(model3))
Box.test(residuals(model03), lag = floor(log(length(residuals(model3)))), type = c("Ljung-Box"))

acf(residuals(model3)^2) # Autocorrelation function RES^2
Box.test(residuals(model3)^2, lag = floor(log(length(residuals(model3)))), type = c("Ljung-Box"))

jarque.bera.test(residuals(model3))
