#Pomocný skript pro exportované obrázky

#Knihovny

library(ggplot2)
  library(gridExtra)

#-------------------------------------------------------------------------------
#Data exploration
plotDataExpl2 <- gg_season(as_tsibble(hungary_data.in), period = "year", labels = "both") +
  labs(y = "Unemployment (thousands)",
       x = "Time")
  #theme_bw()

plotDataExpl1 <-ggplot(hungary_data.in, aes(x = x, y = y)) +
  labs(y = "Unemployment (thousands)",
       x = "Time")+
  geom_line(size = 0.8) #+
  #theme_bw()


plotDataExpl <- grid.arrange(plotDataExpl1, plotDataExpl2, nrow = 1)


# Save the combined plots
ggsave("plotDataExpl.pdf", plotDataExpl, width = 12, height = 6)

#-------------------------------------------------------------------------------
#Task 2
plot_data <- data.frame(
  hun = mod_data.in$hun,
  t = mod_data.in$t,
  fit = predict(model.1.gon3)
)

# Plot using ggplot2
plotT2Fit <- ggplot(plot_data, aes(x = t)) +
  geom_line(aes(y = hun, color = "Observed"), size = 0.5) +
  geom_line(aes(y = fit, color = "Fitted"), size = 0.6) +
  labs(x = "Time", y = "Unemployment (thousands)") +
  scale_color_manual(values = c("Observed" = "black", "Fitted" = "red")) +
  theme(legend.position = "right")

# Save the plot
ggsave("plotT2Fit.png",plotT2Fit, width = 8, height = 4, dpi = 300)
checkresiduals(model.1.gon3)


#-------------------------------------------------------------------------------
#Task 1
data.hungary.in = data.frame(hun = hungary_data.in, t = seq_along(hungary_data.in), seas.dummy = as.factor(cycle(hungary_data.in)))

model1 = lm(hun ~ t + I(t^2) + I(t^3)+I(t^4)+ seas.dummy, data= data.hungary.in)


par(mfrow = c(1,1))
plot(hungary_data.in, lwd = 2)
lines(ts(predict(model1), start = start(hungary_data.in), frequency = frequency(hungary_data.in)), col = "red")

checkresiduals(model1)
#---------------------------
plot_data2 <- data.frame(
  hun = hungary_data.in,
  t = seq_along(hungary_data.in),
  fit = predict(model1),
  seas_dummy = as.factor(cycle(hungary_data.in))
)
plotT1Fit <- ggplot(plot_data2, aes(x = t, y = hun)) +
  geom_line(aes(color = "Observed"), size = 0.5) +
  geom_line(aes(y = fit, color = "Fitted"), size = 0.6) +
  labs(x = "Time", y = "Unemployment (thousands)", color = "Line") +
  theme(legend.position = "right") +
  scale_color_manual(values = c("Observed" = "black", "Fitted" = "green"))

# Save the plot
ggsave("plotT1Fit.png",plotT1Fit, width = 8, height = 4, dpi = 300)

#-------------------------------------------------------------------------------
#Task 3



#-------------------------------------------------------------------------------
#Task 4



#-------------------------------------------------------------------------------
#Task 5



#-------------------------------------------------------------------------------
#Task 6

plotfore <- autoplot(modelBATS_forecast) +
  autolayer(hungary_data.in, series = "Observed", size = 0.9) +
  autolayer(hungary_data.out, series = "Observed",  size = 1) +
  autolayer(modelBATS$fitted, series = "Fitted", linetype = "solid", size = 0.9) +
  labs(x = "Time", y = "Unemployment (thousands)") +
  theme(legend.position = "bottom")
ggsave("forecast.png",plotfore, width = 8, height = 4, dpi = 300)
