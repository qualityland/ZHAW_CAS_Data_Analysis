#data.path <- "/Users/sschmidt/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B1/data/"
data.path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B1/data/"
ga <- read.table(paste0(data.path, "Softdrink.dat"), header = TRUE)
fit.ga <- lm(Zeit ~ Menge, data = ga)


# Residuenanalyse
# ---------------

# Tukey-Ascombe-, Normal- und Scale-Location-Plot
# (mit und ohne Simulationen)
par(mfrow = c(1,3))
# ohne Simulationen
plot(fit.ga, which = 1:3) 
load(paste0(data.path, "resplot.rda"))
# mit Simulationen
resplot(fit.ga, plots = 1:3)


# Modellerweiterungen
# -------------------
bw <- read.csv(paste0(data.path, "bremsweg.csv"))
fit.bw <- lm(brdist ~ speed, data = bw)
# Scatterplot & Regressionsgerade
par(mfrow = c(1,1))
plot(brdist ~ speed, data = bw, pch = 16, main = "Bremsweg vs. Geschwindigkeit",
     ylab = "Bremsweg [m]", xlab = "Speed [km/h]")
abline(fit.bw, col = 'blue')

# Tukey-Anscombe-, Normal- und Scale-Location-Plot
par(mfrow = c(1,3))
plot(fit.bw, which = 1:3)
load(paste0(data.path, "resplot.rda"))
# mit Simulationen
resplot(fit.bw, plots = 1:3)
# ==> kein linearer Zusammenhang!
# Physik: E = 1/2 m v2
# Zusammenhang quadratisch!

### Kurvlineare Regression ###
# Anpassen des Modells
# (neue Variable speed2)
bw$speed2 <- bw$speed^2                    # quadrierte Speed
fit.bw2 <- lm(brdist ~ speed2, data = bw)  # neues Modell
# Visualisierung
par(mfrow = c(1,1))
plot(brdist ~ speed, data = bw)
x <- seq(10, 130, 1)
# Regressionslinie
# mit Koeffizienten des neuen Modells und quadrierter Geschw.
lines(x, coef(fit.bw2)[1] + coef(fit.bw2)[2] * x^2, col = "red")


### Log-Response Regression (Transformation der Zielvariablen) ###
# Autopreis
ap <- read.csv(paste0(data.path, "autopreis.csv"))
ap$LogValue <- log(ap$value)
fit.ap <- lm(LogValue ~ age, data = ap)
# Geschaetzte Koeffizienten
coef(fit.ap)
# Log(Preis) = 2.95 - 0.19 * Alter
# => Preis   = exp(2.95) * exp(-0.19 * Alter)
#            = 19.11     * exp(-0.19 * Alter)

# Einzeichnen
plot(value ~ age, data = ap, xlab = "Alter [Jahre]", ylab = "Preis [$]", main = "Autopreis und -alter")
x <- seq(0, 25, by = 0.01)
lines(x, exp(coef(fit.ap)[1] + coef(fit.ap)[2] * x), col = 'red')



### Log-Log Regression (Ziel- und Prädiktor-Variable log-transformiert) ###
# Säuglingssterblichkeit (infant mortality)
im <- read.csv(paste0(data.path, 'leinhardt.csv'))
# un-transformiert
plot(infant ~ income, data = im,
     main = "Säuglingssterblichkeit (NICHT transformiert)",
     xlab = "Pro-Kopf-Einkommen",
     ylab = "Säuglingssterblichkeit")
# X und Y logarithmiert
plot(LogInfant ~ LogIncome, data = im,
     main = "Säuglingssterblichkeit (X und Y logarithmiert)")
fit.im <- lm(LogInfant ~ LogIncome, data = im)
# Einzeichnen auf Log-Skala
abline(fit.im, col = 'green')

# Einzeichnen auf Original-Skala
plot(infant ~ income, data = im,
     main = "Säuglingssterblichkeit (Original-Skala)",
     xlab = "Pro-Kopf-Einkommen",
     ylab = "Säuglingssterblichkeit")
x <- 0:6000
lines(x, exp(coef(fit.im)[1] + coef(fit.im)[2] * log(x)), col = 'green')
      