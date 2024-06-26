##ABE playground-----------------------------------------------------------------
#TASK 2, TASK 4, TASK 6

#General instructions
#A. Fit a model
#B. Provide mathematical formula
#C. Plot outputs + describe and interpret
#D. Verify model



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

#* modelpol4: Y = Tr + E with Tr = a0 + a1*t + a2*t^2 + a3*t^3 + a4*t^4 ------------------------------

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

#* modelpol4: Y = Tr + E with Tr = a0 + a1*t + a2*t^2 + a3*t^3 + a4*t^4 + a5*t^5 ------------------------------

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
#2. Modeling seasonality using gon funs-----------------------------------------
#pol4 gon 1 
model.1.gon = lm(hun ~ t + I(t^2)+I(t^3)+ I(t^4)  +
                  I(sin(2*pi*t/12)) + I(cos(2*pi*t/12)) +
                  I(sin(4*pi*t/12)) + I(cos(4*pi*t/12)) +
                  I(sin(6*pi*t/12)) + I(cos(6*pi*t/12)) +
                  I(sin(8*pi*t/12)) + I(cos(8*pi*t/12)) +
                  I(sin(10*pi*t/12)) + I(cos(10*pi*t/12)) +
                  I(sin(12*pi*t/12)) + I(cos(12*pi*t/12)), data = mod_data.in)

summary(model.1.gon)
drop1(model.1.gon, test = 'F')

par(mfrow = c(1,1))
plot(mod_data.in$hun, lwd = 2)
lines(mod_data.in$t, predict(model.1.gon), col = "red") 

par(mfrow = c(2,2))
plot(model.1.gon)
par(mfrow = c(1,1))
acf(model.1.gon$residuals)
acf(model.1.gon$residuals^2)

#pol4 gon WINNER
model.1.gon3 = lm(hun ~ t + I(t^2)+I(t^3)+ I(t^4)  +
                    I(sin(2*pi*t/12)) +
                    I(sin(4*pi*t/12)), data = mod_data.in)
summary(model.1.gon3)
decomposed <- decompose(hungary_data.in)
par(mfrow = c(1,1))
plot(mod_data.in$hun, lwd = 2)
lines(mod_data.in$t, decomposed$trend, col = "blue") #pro srovnani s fci decompose
lines(mod_data.in$t, predict(model.1.gon3), col = "red") 

par(mfrow = c(2,2))
plot(model.1.gon3)
par(mfrow = c(1,1))
acf(model.1.gon3$residuals)
acf(model.1.gon3$residuals^2)

anova(model.1.gon2,model.1.gon3) 
checkresiduals(model.1.gon3)
#pol4 gon2 
model.1.gon2 = lm(hun ~ t + I(t^2)+I(t^3)+ I(t^4)  +
                   I(sin(2*pi*t/12)) + I(cos(2*pi*t/12)) +
                   I(sin(4*pi*t/12)), data = mod_data.in)
summary(model.1.gon2)
decomposed <- decompose(hungary_data.in)
par(mfrow = c(1,1))
plot(mod_data.in$hun, lwd = 2)
lines(mod_data.in$t, decomposed$trend, col = "blue") #pro srovnani s fci decompose
lines(mod_data.in$t, predict(model.1.gon2), col = "red") 


par(mfrow = c(2,2))
plot(model.1.gon2)
par(mfrow = c(1,1))
acf(model.1.gon2$residuals)
acf(model.1.gon2$residuals^2)

anova(model.1.gon,model.1.gon2) #nezamitame -> lze zjednodušit?

acf(na.omit(decomposed$random)) #pro srovnani
#pol3 gon 1
model.2.gon = lm(hun ~ t + I(t^2)+I(t^3)  +
                   I(sin(2*pi*t/12)) + I(cos(2*pi*t/12)) +
                   I(sin(4*pi*t/12)) + I(cos(4*pi*t/12)) +
                   I(sin(6*pi*t/12)) + I(cos(6*pi*t/12)) +
                   I(sin(8*pi*t/12)) + I(cos(8*pi*t/12)) +
                   I(sin(10*pi*t/12)) + I(cos(10*pi*t/12)) +
                   I(sin(12*pi*t/12)) + I(cos(12*pi*t/12)), data = mod_data.in)

summary(model.2.gon)
drop(model.2.gon, method = 'F')

par(mfrow = c(1,1))
plot(mod_data.in$hun, lwd = 2)
lines(mod_data.in$t, predict(model.2.gon), col = "red") 

par(mfrow = c(2,2))
plot(model.2.gon)
par(mfrow = c(1,1))
acf(model.2.gon$residuals)
acf(model.2.gon$residuals^2)
#horsi resids I would not pick that

#pol3 gon 2

model.2.gon2 = lm(hun ~ t + I(t^2)+I(t^3)  +
                   I(sin(2*pi*t/12)) + I(cos(2*pi*t/12)) +
                   I(sin(4*pi*t/12)), data = mod_data.in)

summary(model.2.gon2)
#drop(model.2.gon2, method = 'F')

par(mfrow = c(1,1))
plot(mod_data.in$hun, lwd = 2)

lines(mod_data.in$t, predict(model.2.gon2), col = "red") 

par(mfrow = c(2,2))
plot(model.2.gon2)
par(mfrow = c(1,1))
acf(model.2.gon2$residuals)
acf(model.2.gon2$residuals^2)
#NOP

#Diagnostics
model.chars = matrix(NA, 3, 7)
colnames(model.chars) = c("R2", "R2.adj", "AIC", "RMSE(in)", "MAE(in)", "RMSE(out)", "MAE(out)")
rownames(model.chars) = c("model.1.gon3", "modelpol4-gonfull", "modelpol4-gon2")

model.chars[1,1] = summary(model.1.gon3)$r.squared # R2
model.chars[1,2] = summary(model.1.gon3)$adj.r.squared # Adjusted R2
model.chars[1,3] = AIC(model.1.gon3) # AIC
model.chars[1,4] = sqrt(mean(residuals(model.1.gon3)^2)) # RMSE in
model.chars[1,5] = mean(abs(residuals(model.1.gon3))) # MAPE in
model.chars[1,6] = sqrt(mean(((mod_data.out$hun - predict(model.1.gon3, mod_data.out)))^2)) # RMSE out
model.chars[1,7] = mean(abs(((mod_data.out$hun - predict(model.1.gon3, mod_data.out))))) # MAPE out

model.chars[2,1] = summary(model.1.gon)$r.squared # R2
model.chars[2,2] = summary(model.1.gon)$adj.r.squared # Adjusted R2
model.chars[2,3] = AIC(model.1.gon) # AIC
model.chars[2,4] = sqrt(mean(residuals(model.1.gon)^2)) # RMSE in
model.chars[2,5] = mean(abs(residuals(model.1.gon))) # MAPE in
model.chars[2,6] = sqrt(mean(((mod_data.out$hun - predict(model.1.gon, mod_data.out)))^2)) # RMSE out
model.chars[2,7] = mean(abs(((mod_data.out$hun - predict(model.1.gon, mod_data.out))))) # MAPE out

model.chars[3,1] = summary(model.1.gon2)$r.squared # R2
model.chars[3,2] = summary(model.1.gon2)$adj.r.squared # Adjusted R2
model.chars[3,3] = AIC(model.1.gon2) # AIC
model.chars[3,4] = sqrt(mean(residuals(model.1.gon2)^2)) # RMSE in
model.chars[3,5] = mean(abs(residuals(model.1.gon2))) # MAPE in
model.chars[3,6] = sqrt(mean(((mod_data.out$hun - predict(model.1.gon2, mod_data.out)))^2)) # RMSE out
model.chars[3,7] = mean(abs(((mod_data.out$hun - predict(model.1.gon2, mod_data.out))))) # MAPE out

anova(model.1.gon,model.1.gon2)
print(model.chars)



coeftest(model.1.gon2,vcov=vcovHC(model.1.gon2,type="HC0")) #nezměnily se p hodnoty
#-> hetero není dostatečně potento?

#??? zbyla mi tam nějaká autokorelace, co s ní? -> možná param model?
#??? jsou podmínky lineárního modelu splněny?

#pol3 gon 1
model.1.gon2.log = lm(log(hun) ~ t + I(t^2)+I(t^3)+ I(t^4)  +
                    I(sin(2*pi*t/12)) + I(cos(2*pi*t/12)) +
                    I(sin(4*pi*t/12)), data = mod_data.in)
summary(model.1.gon2.log)
par(mfrow = c(1,1))
plot(mod_data.in$hun, lwd = 2)
lines(mod_data.in$t, exp(predict(model.1.gon2.log)), col = "red") 

par(mfrow = c(2,2))
plot(model.1.gon2.log)
par(mfrow = c(1,1))
acf(model.1.gon2.log$residuals)
acf(model.1.gon2.log$residuals^2)

model.1.gon3.log = lm(log(hun) ~ t + I(t^2)+I(t^3)+ I(t^4)+ I(cos(2*pi*t/12)), data = mod_data.in)
summary(model.1.gon3.log)
par(mfrow = c(1,1))
plot(mod_data.in$hun, lwd = 2)
lines(mod_data.in$t, exp(predict(model.1.gon3.log)), col = "red") 

par(mfrow = c(2,2))
plot(model.1.gon3.log)
par(mfrow = c(1,1))
acf(model.1.gon3.log$residuals)
acf(model.1.gon3.log$residuals^2)
#ZHORSENI

#Finalni model => model.1.gon2 (polynomialni trend 4 stupne)


#4 BATS------------------------------------------------------------------------
seasonplot(hungary_data.in, col=rainbow(12), year.labels=TRUE)
modelBATS = bats(hungary_data.in)
modelBATS #lambda =0 -> log transformation
#(omega,p,q,phi,m1,..,mJ) ->omega: Box Cox param, phi - damping param, ARMA(p,q), m1.., mJ seasonal periods
par(mfrow = c(1,1))
plot(modelBATS)
plot(model5)
?bats
modelBATS_forecast = forecast(modelBATS, h = 12)
plot(hungary_data)
plot(modelBATS_forecast)
lines(hungary_data.out)
lines(modelBATS$fitted, col = "red")

checkresiduals(modelBATS)
?checkresiduals
?jarque.bera.test
acf(residuals(modelBATS))
Box.test(residuals(modelBATS), lag = floor(log(length(residuals(modelBATS)))), type = c("Ljung-Box")) # Ljung - Box test
jarque.bera.test(residuals(modelBATS)) # Jarque - Bera test

#1404, 1392, 1565, 1539, 1538
#6 Predictions------------------------------------------------------------------

#A. Model selection


#B.Prediction
autoplot(modelBATS_forecast) +
  autolayer(hungary_data.in, series = "Observed", size = 0.9) +
  autolayer(hungary_data.out, series = "Observed",  size = 1) +
  autolayer(modelBATS$fitted, series = "Fitted", linetype = "solid", size = 0.9) +
  labs(x = "Time", y = "Unemployment (thousands)") +
  theme(legend.position = "bottom")
ggsave()
plot_components(modelBATS)
#Measuremnts: MSE and spol
#některé jsou bezrozměrné (MAPE, AMAPE)