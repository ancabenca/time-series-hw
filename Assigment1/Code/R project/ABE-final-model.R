#final model analysis
data.hungary.out = data.frame(hun = hungary_data.out, t = seq_along(hungary_data.out), seas.dummy = as.factor(cycle(hungary_data.out)))


model1 = lm(hun ~ t + I(t^2) + I(t^3)+I(t^4)+ seas.dummy, data= data.hungary.in)
model.1.gon3 = lm(hun ~ t + I(t^2)+I(t^3)+ I(t^4)  + I(sin(2*pi*t/12)) + I(sin(4*pi*t/12)), data = mod_data.in)
model3 = ets(hungary_data.in, model = "ZZZ", opt.crit = "lik", ic = "aic")
modelBATS = bats(hungary_data.in)
model5 = tbats(hungary_data.in)

c(AIC(model1),AIC(model.1.gon3),model3$aic, modelBATS$AIC, model5$AIC)



model.chars = matrix(NA, 5, 7)
colnames(model.chars) = c("R2", "R2.adj", "AIC", "RMSE(in)", "MAE(in)", "RMSE(out)", "MAE(out)")
rownames(model.chars) = c("model1", "model2", "model3","model4","model5")

model.chars[1,1] = summary(model1)$r.squared # R2
model.chars[1,2] = summary(model1)$adj.r.squared # Adjusted R2
model.chars[1,3] = AIC(model1) # AIC
model.chars[1,4] = sqrt(mean(residuals(model1)^2)) # RMSE in
model.chars[1,5] = mean(abs(residuals(model1))) # MAPE in
model.chars[1,6] = sqrt(mean(((mod_data.out$hun - predict(model1, data.hungary.out)))^2)) # RMSE out
model.chars[1,7] = mean(abs(((mod_data.out$hun - predict(model1, data.hungary.out))))) # MAPE out

model.chars[2,1] = summary(model.1.gon3)$r.squared # R2
model.chars[2,2] = summary(model.1.gon3)$adj.r.squared # Adjusted R2
model.chars[2,3] = AIC(model.1.gon3) # AIC
model.chars[2,4] = sqrt(mean(residuals(model.1.gon3)^2)) # RMSE in
model.chars[2,5] = mean(abs(residuals(model.1.gon3))) # MAPE in
model.chars[2,6] = sqrt(mean(((mod_data.out$hun - predict(model.1.gon3, mod_data.out)))^2)) # RMSE out
model.chars[2,7] = mean(abs(((mod_data.out$hun - predict(model.1.gon3, mod_data.out))))) # MAPE out


model.chars[3,3] = model3$aic # AIC
model.chars[3,4] = sqrt(mean(residuals(model3)^2)) # RMSE in
model.chars[3,5] = mean(abs(residuals(model3))) # MAP-E in
model.chars[3,6] = sqrt(mean(((mod_data.out$hun - predict(model3, mod_data.out)))^2)) # RMSE out
model.chars[3,7] = mean(abs(((mod_data.out$hun - predict(model3, mod_data.out))))) # MAPE out


model.chars[4,3] = modelBATS$AIC # AIC
model.chars[4,4] = sqrt(mean(residuals(modelBATS)^2)) # RMSE in
model.chars[4,5] = mean(abs(residuals(modelBATS))) # MAPE in
model.chars[4,6] = sqrt(mean(((mod_data.out$hun - predict(modelBATS, mod_data.out)))^2)) # RMSE out
model.chars[4,7] = mean(abs(((mod_data.out$hun - predict(modelBATS, mod_data.out))))) # MAPE out


model.chars[5,3] = model5$AIC # AIC
model.chars[5,4] = sqrt(mean(residuals(model5)^2)) # RMSE in
model.chars[5,5] = mean(abs(residuals(model5))) # MAPE in
model.chars[5,6] = sqrt(mean(((mod_data.out$hun - predict(model5, mod_data.out)))^2)) # RMSE out
model.chars[5,7] = mean(abs(((mod_data.out$hun - predict(model5, mod_data.out))))) # MAPE out

modelBATS_forecast = forecast(modelBATS, h = 12)
acModel4 <- accuracy(modelBATS_forecast, mod_data.out$hun)
model3_forecast = forecast(model3, h = 12)
acModel3 <- accuracy(model3_forecast, mod_data.out$hun)
model5_forecast = forecast(model5, h = 12)
acModel5 <- accuracy(model5_forecast, mod_data.out$hun)

acModel3
acModel4
acModel5
?accuracy
