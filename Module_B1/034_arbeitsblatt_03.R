################### Aufgabe 1. ################### 

# Pfad zu den Daten
dpath <- "./Module_B1/data/"

# Daten laden
windmill <- read.table(paste0(dpath, "Windmuehle.dat"), header=TRUE)

# 
fit.wm <- lm(Strom ~ Windgeschwindigkeit, data = windmill)

# Achsenabschnitt und Steigung
summary(fit.wm)

plot(Strom ~ Windgeschwindigkeit, data = windmill)
abline(fit.wm, col = "blue")

# Vertrauensintervall fuer die Steigung
confint(fit.wm, level = 0.95)

# graphische Residuenanalyse
par(mfrow = c(1, 3))
plot(fit.wm, which = 1:3)

# mit Simulationen
load(paste0(dpath, "resplot.rda"))
resplot(fit.wm, plots = 1:3)
