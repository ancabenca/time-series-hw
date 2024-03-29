#Pomocný skript pro exportované obrázky

#Knihovny

library(ggplot2)
  library(gridExtra)

#-------------------------------------------------------------------------------
#Data exploration
plotDataExpl2 <- gg_season(as_tsibble(hungary_data.in), period = "year", labels = "both") +
  labs(y = "Unemployment (thousands)",
       x = "Time")+
  theme_minimal()

plotDataExpl1 <-ggplot(hungary_data.in, aes(x = x, y = y)) +
  labs(y = "Unemployment (thousands)",
       x = "Time")+
  geom_line(size = 0.8) +
  theme_minimal()


plotDataExpl <- grid.arrange(plotDataExpl1, plotDataExpl2, nrow = 1)


# Save the combined plots
ggsave("plotDataExpl.pdf", plotDataExpl, width = 12, height = 6)

#-------------------------------------------------------------------------------
#Task 1



#-------------------------------------------------------------------------------
#Task 2
data.hungary.in = data.frame(hun = hungary_data.in, t = seq_along(hungary_data.in), seas.dummy = as.factor(cycle(hungary_data.in)))

model1 = lm(hun ~ t + I(t^2) + I(t^3)+I(t^4)+ seas.dummy, data= data.hungary.in)


par(mfrow = c(1,1))
plot(hungary_data.in, lwd = 2)
lines(ts(predict(model1), start = start(hungary_data.in), frequency = frequency(hungary_data.in)), col = "red")

checkresiduals(model1)

#-------------------------------------------------------------------------------
#Task 3



#-------------------------------------------------------------------------------
#Task 4



#-------------------------------------------------------------------------------
#Task 5



#-------------------------------------------------------------------------------
#Task 6


