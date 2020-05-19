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
plot(brdist ~ speed, data = bw, main = "Bremsweg vs. Geschwindigkeit")
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



