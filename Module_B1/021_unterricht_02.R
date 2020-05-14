################# Einfache Lineare Regression #################################
###                 Inferenz und Vorhersage

# ---------------------------
# Beispiel: Getränkeautomaten
# ---------------------------


# Daten laden
data.path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B1/data/"
ga <- read.table(paste0(data.path, "Softdrink.dat"), header = TRUE)


# Modell, Koeffizienten, Regressionsgerade
fit.ga <- lm(Zeit ~ Menge, data = ga)

coef(fit.ga)

plot(Zeit ~ Menge, data = ga, pch = 16)
abline(fit.ga, col = 'red')


# Schaetzen von alpha und beta (Simulation)
# Wir simulieren mehrere Datensets von folgendem Modell:
#       Yi = 4 − 2xi + Ei mit σ = 0.5
# und schätzen dann mittels Regression jeweils α und β.
set.seed(134)
nsim <- 100 # Anzahl Simulationen
result <- matrix(NA, nrow = nsim, ncol = 2) # result matrix
x <- seq(0, 1, 0.025)

for(i in 1:nsim){
  y <- 4 - 2 * x + rnorm(length(x), 0, 0.5)
  fit <- lm(y ~ x)
  result[i, ] <- coef(fit)
}

#par(mfrow=c(1,2))
hist(result[, 1], main = "Schaetzung fuer alpha")
hist(result[, 2], main = "Schaetzung fuer beta")

### Beziehung zwischen x und y
# 1. Welcher Werte sind für die unbekannten Parameter α und β am plausibelsten?
# 2. Welchen Erklärungsgehalt hat die Gerade?
# 3. Hat die erklärende Variable einen signifikanten Einfluss auf die Zielgrösse?
# 4. Welche Werte sind insgesamt plausibel?

# 1. Welcher Werte sind für die unbekannten Parameter α und β am plausibelsten?
#    Die Punktschätzungen aus dem KQ-Verfahren:
fit.ga <- lm(Zeit ~ Menge, data = ga)
coef(fit.ga)

# 2. Welchen Erklärungsgehalt hat die Gerade?
# Bestimmheitsmass R2
# gute Werte: knapp unter 1 (z.B. 0.85)

# 3. Hat die erklärende Variable einen signifikanten Einfluss auf die Zielgrösse?
summary(fit.ga)
# Coefficients:
#                 Estimate  Std. Error  t value   Pr(>|t|)    
# (Intercept)     3.321     1.371       2.422     0.0237 *  
#   Menge         2.176     0.124       17.546    8.22e-15 ***
#
# Nullhypothese alpha=alpha0 muss verworfen werden (p-value < 0.05)
# Nullhypothese beta=beta0 muss verworfen werden (p-value < 0.05)

# 4. Welche Werte sind insgesamt plausibel?
confint(fit.ga, level = 0.95)
#               2.5 %       97.5 %
# (Intercept)   0.4844979   6.157062
# Menge         1.9195920   2.432741



### Vorhersage
# 1. Welcher y-Wert ist für ein gegebenes x am plausibelsten?
# 2. Wie genau ist dieser vorhergesagte Wert für y?
# 3. In welchem Bereich wird der Datenpunkt y liegen?

fit.ga <- lm(Zeit ~ Menge, data = ga)
x0 <- data.frame(Menge = 20)
# Vorhersage:
predict(fit.ga, newdata = x0)   # Data Frame uebergeben!
#        1 
# 46.84411 

# Vertrauens Intervall fuer die erwartete Service-Zeit
predict(fit.ga, newdata = x0, interval = "confidence", level = 0.95)
#        fit      lwr     upr
#1  46.84411 43.48112 50.2071
#
# Die erwartete Service-Zeit liegt zwischen 43.48 und 50.20 min.

# Prognose Intervall fuer y von x0
predict(fit.ga, newdata = x0, interval = "prediction", level = 0.95)
#        fit      lwr      upr
#1  46.84411 37.56348 56.12474


# Darstellung von Vertrauens- und Prognose-Band
datanew <- data.frame(Menge = seq(0, 35,by = 1))
pred1 <- predict(fit.ga, newdata = datanew, interval = "confidence")
pred2 <- predict(fit.ga, newdata = datanew, interval = "prediction")
plot(ga$Menge, ga$Zeit); abline(fit.ga)
lines(datanew$Menge, pred1[,2], col = "blue")
lines(datanew$Menge, pred1[,3], col = "blue")
lines(datanew$Menge, pred2[,2], col = "red")
lines(datanew$Menge, pred2[,3], col = "red")
legend("topleft", legend=c("Vertrauensband", "Prognoseband"), col=c("blue", "red"), lty=c(1, 1), cex=0.8)

# Zusammenfassum zum Modell
summary(fit.ga)
# Call:
#   lm(formula = Zeit ~ Menge, data = ga)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -7.5811 -1.8739 -0.3493  2.1807 10.6342 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      3.321      1.371   2.422   0.0237 *  
#   Menge          2.176      0.124  17.546 8.22e-15 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.181 on 23 degrees of freedom
# Multiple R-squared:  0.9305,	Adjusted R-squared:  0.9275 
# F-statistic: 307.8 on 1 and 23 DF,  p-value: 8.22e-15

