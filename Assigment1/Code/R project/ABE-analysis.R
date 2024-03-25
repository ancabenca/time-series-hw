##ABE playground-----------------------------------------------------------------

#General instructions
#A. Fit a model
#B. Provide mathematical formula
#C. Plot outputs + describe and interpret
#D. Verify model

#2. Modeling seasonality using gon funs-----------------------------------------

#estimate of the trend in the data -> plot the things out
#2.2 -> s=12, p4-14

#Trend identification
#* modelpol3: Y = Tr + E with Tr = a0 + a1*t + a2*t^2 + a3*t^3 ------------------------------

#*** Model declaration ---------------------------------------------------------
modelpol3 = lm(hun ~ t+I(t^2)+I(t^3), data = mod_data.in) #quadratic curve?

summary(modelpol3) #highly significant

par(mfrow = c(1,1))
plot(mod_data.in$hun, lwd = 2)
lines(mod_data.in$t, predict(modelpol3), col = "red") #terrible fit

par(mfrow = c(1,1))
plot(residuals(modelpol3), ylab = "Residuals") #naprosto nevhodné
par(mfrow = c(2,2))
plot(modelpol3) 
acf(residuals(modelpol3)) # highly correlated -> not the best model2
acf(residuals(modelpol3)^2)

#* modelpol4: Y = Tr + E with Tr = a0 + a1*t + a2*t^2 + a3*t^3 ------------------------------

#*** Model declaration ---------------------------------------------------------
modelpol4 = lm(hun ~ t+I(t^2)+I(t^3) + I(t^4), data = mod_data.in) #quadratic curve?

summary(modelpol4) #highly significant

par(mfrow = c(1,1))
plot(mod_data.in$hun, lwd = 2)
lines(mod_data.in$t, predict(modelpol4), col = "red") #terrible fit

par(mfrow = c(1,1))
plot(residuals(modelpol4), ylab = "Residuals") #naprosto nevhodné
par(mfrow = c(2,2))
plot(modelpol4) 
acf(residuals(modelpol4)) # highly correlated -> not the best model2
acf(residuals(modelpol4)^2)

#* modelpol4: Y = Tr + E with Tr = a0 + a1*t + a2*t^2 + a3*t^3 ------------------------------

#*** Model declaration ---------------------------------------------------------
modelpol5 = lm(hun ~ t+I(t^2)+I(t^3) + I(t^4) +I(t^5), data = mod_data.in) #quadratic curve?

summary(modelpol5) #highly significant

par(mfrow = c(1,1))
plot(mod_data.in$hun, lwd = 2)
lines(mod_data.in$t, predict(modelpol5), col = "red") #terrible fit

par(mfrow = c(1,1))
plot(residuals(modelpol5), ylab = "Residuals") #naprosto nevhodné
par(mfrow = c(2,2))
plot(modelpol4) 
acf(residuals(modelpol5)) # highly correlated -> not the best model2
acf(residuals(modelpol5)^2)

model.chars = matrix(NA, 3, 7)
colnames(model.chars) = c("R2", "R2.adj", "AIC", "RMSE(in)", "MAE(in)", "RMSE(out)", "MAE(out)")
rownames(model.chars) = c("modelpol3", "modelpol4", "modelpol5")

model.chars[1,1] = summary(modelpol3)$r.squared # R2
model.chars[1,2] = summary(modelpol3)$adj.r.squared # Adjusted R2
model.chars[1,3] = AIC(modelpol3) # AIC
model.chars[1,4] = sqrt(mean(residuals(modelpol3)^2)) # RMSE in
model.chars[1,5] = mean(abs(residuals(modelpol3))) # MAPE in
model.chars[1,6] = sqrt(mean(((mod_data.out$hun - predict(modelpol3, mod_data.out)))^2)) # RMSE out
model.chars[1,7] = mean(abs(((mod_data.out$hun - predict(modelpol3, mod_data.out))))) # MAPE out

model.chars[2,1] = summary(modelpol4)$r.squared # R2
model.chars[2,2] = summary(modelpol4)$adj.r.squared # Adjusted R2
model.chars[2,3] = AIC(modelpol4) # AIC
model.chars[2,4] = sqrt(mean(residuals(modelpol4)^2)) # RMSE in
model.chars[2,5] = mean(abs(residuals(modelpol4))) # MAPE in
model.chars[2,6] = sqrt(mean(((mod_data.out$hun - predict(modelpol4, mod_data.out)))^2)) # RMSE out
model.chars[2,7] = mean(abs(((mod_data.out$hun - predict(modelpol4, mod_data.out))))) # MAPE out

model.chars[3,1] = summary(modelpol5)$r.squared # R2
model.chars[3,2] = summary(modelpol5)$adj.r.squared # Adjusted R2
model.chars[3,3] = AIC(modelpol5) # AIC
model.chars[3,4] = sqrt(mean(residuals(modelpol5)^2)) # RMSE in
model.chars[3,5] = mean(abs(residuals(modelpol5))) # MAPE in
model.chars[3,6] = sqrt(mean(((mod_data.out$hun - predict(modelpol5, mod_data.out)))^2)) # RMSE out
model.chars[3,7] = mean(abs(((mod_data.out$hun - predict(modelpol5, mod_data.out))))) # MAPE out


print(model.chars)
#4 BATS------------------------------------------------------------------------





#6 Predictions------------------------------------------------------------------



#Measuremnts: MSE and spol
#některé jsou bezrozměrné (MAPE, AMAPE)