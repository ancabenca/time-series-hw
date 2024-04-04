#TASK 1, TASK 3, TASK 5

# TASK 1 
library(tseries)
library(forecast)

data.hungary.in = data.frame(hun = hungary_data.in, t = seq_along(hungary_data.in), seas.dummy = as.factor(cycle(hungary_data.in)))
data.hungary.out = data.frame(hun = hungary_data.out, t = seq_along(hungary_data.out), seas.dummy = as.factor(cycle(hungary_data.out)))


plot(hungary_data.in)
seasonplot(hungary_data.in)
# We see that data have nice "inverse S shape", that is not typical for our general trend choices, that is why we will try
# to catch the trend using polynomial of the fourth order.

model = lm(hun ~ t+  I(t^2) + I(t^3) + + I(t^4),data=data.hungary.in)

plot(mod_data.in$hun, lwd = 2)

lines(mod_data.in$t, predict(model1), col = "red")

data = data.frame(c=(12))

predict(model,data=data)


summary(model)

# maybe diagnostics?  plot(model), ale asi bych udelal az s hotovym modelem

# We see that our model estimated the trend very nicely. We want to capture seasonality aswell. So we will try the dummy
# variables approach


model1 = lm(hun ~ t + I(t^2) + I(t^3)+I(t^4)+ seas.dummy, data= data.hungary.in)


par(mfrow = c(1,1))
plot(hungary_data.in, lwd = 2)
lines(ts(predict(model1), start = start(hungary_data.in), frequency = frequency(hungary_data.in)), col = "red")

par(mfrow=c(2,2))
plot(model1)
checkresiduals(model1)

#We see that our model violates the assumption of noncorellated errors,(E[eps] = 0 and var[eps] = sigma ^2 is good)


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
library(forecast)
model3 = ets(hungary_data.in, model = "ZZZ", opt.crit = "lik", ic = "aic")
summary(model3)

par(mfrow = c(1,1))
plot(model3)

checkresiduals(model3)

acf(residuals(model3)) # Autocorrelation function
Box.test(residuals(model3), lag = floor(log(length(residuals(model3)))), type = c("Ljung-Box")) # Ljung - Box test

acf(residuals(model3)^2) # Autocorrelation function RES^2
Box.test(residuals(model3)^2, lag = floor(log(length(residuals(model3)))), type = c("Ljung-Box")) # Ljung - Box test RES^2

jarque.bera.test(residuals(model3)) # Jarque - Bera test

#Task 5 

model5 = tbats(hungary_data.in)

model5

par(mfrow = c(1,1))
plot(model5)


checkresiduals(model5)

acf(residuals(model5))
Box.test(residuals(model5), lag = floor(log(length(residuals(model3)))), type = c("Ljung-Box"))

acf(residuals(model5)^2) # Autocorrelation function RES^2
Box.test(residuals(model5)^2, lag = floor(log(length(residuals(model3)))), type = c("Ljung-Box"))

jarque.bera.test(residuals(model5))
