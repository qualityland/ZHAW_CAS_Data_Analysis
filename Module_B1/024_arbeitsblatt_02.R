data.path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B1/data/"


### Aufgabe 1: Antike Uhren-------------------------------------------
au <- read.table(paste0(data.path, "AntikeUhren.dat"), header = TRUE)

# (a)
# Scatterplot von Preis ~ Alter
plot(Preis ~ Alter, data = au)

# angepasste Gerade:
fit.au <- lm(Preis ~ Alter, data = au)
abline(fit.au, col = "red")

# Koeffizienten:
coef(fit.au)

# geschätzte Geradengleichung:
# Preis = -191.66 + 10.48 * Alter


# (b)

## (1) mit Hilfe von summary():
summary(fit.au)
# Ja, H0: beta=0 muss mit p-value < 0.05 verworfen werden.
# p < 2.1e-06 ***


## (2) manuell:
hat_beta <- coef(fit.au)[2]          # geschaetztes beta
ssx <- sum((au$Alter - mean(au$Alter))^2)
se_beta <- sqrt(summary(fit.au)$sigma^2 / ssx)
T <- hat_beta / se_beta
T
# to be continued...


# (c)
confint(fit.au, parm = "Alter", level = 0.99)
# Ja, Preiserhoehungen zwischen $ 5.55 und $ 15.40 sind plausibel (1% Niveau)


# (d)
# Geschaetztes beta 'Alter'
coef(fit.au)[2]
# Innerhalb eines Jahres ist mit einer Preiszunahme von $10.48 zu rechnen
confint(fit.au, parm = "Alter", level = 0.95)
# Plausible Preiszunahmen liegen zwischen $6.82 und $14.13.

# (e)
x0 <- data.frame(Alter=160)
# Vertrauens-Intervall (Variabilitaet angepasster Werte)
predict(fit.au, newdata = x0, interval = 'confidence', level = 0.95)
#        fit     lwr      upr
# 1 1484.998 1372.09 1597.905


# Prognose-Intervall (Bereich zukuenftiger Datenpunkte)
predict(fit.au, newdata = x0, interval = 'prediction', level = 0.95)
#        fit      lwr      upr
# 1 1484.998 916.0829 2053.912

# Prognose-Intervall nuetzlicher, es steckt Bereich zukuenftiger Preise ab.
# Das Prognose-Intervall ist für den unerfahrenen Käufer wichtiger, weil es
# sinnvoller ist, auch die stochastische Variabilität des Beobachtungsfehlers
# Ei zu berücksichtigen und nicht nur die Ungenauigkeit, die sich aus der 
# Schätzung des Modells ergibt.



### Aufgabe 2: Conconi Test -------------------------------------------
load(paste0(data.path, "conconi.rda"))
str(conconi)

# (a)
# Stellen sie die Daten in einem Scatterplot dar, passen sie mit dem Befehl
# lm() die Regressionsgerade an und zeichnen sie diese ein.

plot(puls ~ speed, data = conconi)
fit.co <- lm(puls ~ speed, data = conconi)
abline(fit.co, col = 'red')

# (b)
# Zu welchem Prozentanteil lassen sich die Schwankungen in den Pulswerten durch
# die Zunahme der Geschwindigkeit erklären?
summary(fit.co)
# Multiple R-squared:  0.9861
#
# Zu 98.61% werden die Schwankungen im Puls durch die Geschwindigkeitszunahme erklaert.


# (c)
# Mit welcher Pulsfrequenz muss der Läufer rechnen, wenn er mit 10km/h
# unterwegs ist? Geben Sie ein 95% Prognoseintervall an.
x0 <- data.frame(speed=10)
predict(fit.co, newdata = x0, interval = 'prediction', level = 0.95)
#        fit      lwr      upr
# 1 150.6807 145.7308 155.6306
# Er muss mit einem Puls von 150 rechnen.
# Plausible Werte liegen zwischen 145 und 155 (95% Niveau)

# (d)
# Geben Sie an, wie hoch der Ruhepuls (d.h. keine Vorwärtsbewegung)
# geschätzt wird. In welchem 95% Intervall würden Sie den entsprechenden
# Messpunkt erwarten.
coef(fit.co)[1]
# (Intercept) 
# 86.61053 
confint(fit.co)
#                 2.5 %    97.5 %
# (Intercept) 81.257578 91.963475
# speed        6.018418  6.795617
# oder:
predict(fit.co, newdata = data.frame(speed=0), interval = 'prediction', level = 0.95)
#        fit      lwr      upr
# 1 86.61053 79.52724 93.69381

# (e)
# Um wie viel nimmt der Puls im Schnitt zu, wenn die Geschwindigkeit um 1 km/h
# erhöht wird? Welche anderen Werte sind für die Pulszunahme ebenfalls plausibel?
coef(fit.co)[2]
#    speed 
# 6.407018 
# Im Schnitt nimmt der Puls um 6.4 Schlaege zu
confint(fit.co)
#                 2.5 %    97.5 %
# (Intercept) 81.257578 91.963475
# speed        6.018418  6.795617
# Plausibel sind Zunahmen zwischen 6.0 und 6.8 Schlaegen / min

# (f)
# Im File conconi2.rda stehen Ihnen die Daten eines zweiten Läufers zur
# Verfügung. Wessen Puls steigt bei Geschwindigkeitserhöhung langsamer an?
# Können sie eine Aussage treffen, ob zwischen den beiden ein auf 5% 
# signifikanter Unterschied besteht? Lässt sich ableiten, wer der besser
# trainierte Läufer ist?
load(paste0(data.path, "conconi2.rda"))
str(conconi2)
plot(puls ~ speed, data = conconi2)
fit.co2 <- lm(puls ~ speed, data = conconi2)
abline(fit.co2, col = 'blue')
coef(fit.co2)
coef(fit.co)
# beim 2. Laeufer steigt der Puls nur um 4.09,
# beim 1. Laeufer um 6.40 pro Geschwindigkeitszunahme um 1 km/h.
confint(fit.co2, parm = 'speed', level = 0.95)
#            2.5 %   97.5 %
#   speed 3.883739 4.302727
confint(fit.co, parm = 'speed', level = 0.95)
#            2.5 %   97.5 %
#   speed 6.018418 6.795617
# Der langsamere Pulsanstieg deutet darauf hin, dass Laeufer 2 der
# besser trainierte ist.