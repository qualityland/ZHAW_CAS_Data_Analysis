library(MASS)
data(whiteside)

# Zuerst stellen wir die Daten in einem geeigneten Streudiagramm dar.
# Tragen Sie Gas auf der y-Achse und Temp auf der x-Achse auf. Zusätzlich
# färben Sie die Datenpunkte bezüglich der Variable Insul ein. Beschreiben
# Sie den Zusammenhang.

plot(Gas ~ Temp, data = whiteside, col = Insul, pch = 16)
b.fit <- lm(Gas ~ Temp, data = whiteside[whiteside$Insul == "Before",])
abline(b.fit, col = 'black')
a.fit <- lm(Gas ~ Temp, data = whiteside[whiteside$Insul == "After",])
abline(a.fit, col = 'red')
legend("topright", legend=c("before", "after"), col=c("black", "red"), lty=c(1, 1), cex=0.8)

