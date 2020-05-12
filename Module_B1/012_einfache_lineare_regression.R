
# Beispiel: Getränkeautomaten
# ---------------------------

# Frage: Inwiefern haengt die Servicezeit (Zeit) von der Anzahl gelieferter
#        Produkteinheiten (Menge) ab.

# Daten laden
ga <- read.table("./Module_B1/data/Softdrink.dat", header = TRUE)

# Daten Struktur
str(ga)

# Lineares Modell
fit.ga <- lm(Zeit ~ Menge, data = ga)

# alpha (Achsenabschnitt) und beta (Steigung) des Modells
coef(fit.ga)

# Scatterplot mit der Regressionsgeraden
plot(ga$Menge, ga$Zeit)
abline(fit.ga, col = "red")

# die angepassten Werte (auf der Geraden)
fitted(fit.ga)

# Residuen
resid(fit.ga)

# Standardfehler
summary(fit.ga)$sigma


# Beispiel: Haus-Isolierung
# -------------------------

# Frage: Wie haengt der woechentliche Gasverbrauch (Gas) eines Hauses von der
#        Aussentemperatur (Temp) ab.

library(MASS)
data("whiteside")
str(whiteside)

# nur die Daten VOR der Isolierung des Hauses
before <- whiteside[whiteside$Insul == "Before", ]

# Model VOR Isolierung
fit.before <- lm(Gas ~ Temp, data = before)

# geschaetzte Koeffizienten
coef(fit.before)

# Standardfehler
summary(fit.before)$sigma

# Plot
plot(before$Temp, before$Gas)
abline(fit.before, col = "red")

# Vergleich mit NACH der Isolierung
after <- whiteside[whiteside$Insul == "After", ]
fit.after <- lm(Gas ~ Temp, data = after)

plot(after$Temp, after$Gas)
abline(fit.after, col = "red")
