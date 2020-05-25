################### Aufgabe 1. ################### 

# Pfad zu den Daten
data.path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B1/data/"
# Daten laden
windmill <- read.table(paste0(data.path, "Windmuehle.dat"), header=TRUE)


# (a)
# Wir betrachten zuerst das Modell
#                  Stromi = α + β · Windgeschwindigkeiti + Ei
# Passen Sie ein entsprechendes Regressionsmodell an und prüfen Sie mittels
# grafischer Residuenana- lyse, ob die Voraussetzungen erfüllt sind.

# Modell
fit.wm <- lm(Strom ~ Windgeschwindigkeit, data = windmill)
plot(Strom ~ Windgeschwindigkeit, data = windmill)
abline(fit.wm, col = "red")

# graphische Residuenanalyse
par(mfrow = c(1, 3))
plot(fit.wm, which = 1:3)

# mit Simulationen
load(paste0(data.path, "resplot.rda"))
resplot(fit.wm, plots = 1:3)

# Ergebnis

# Tukey-Anscombe-Plot:
# Der Glätter zeigt eine systematische Abweichung von der Horizontalen,
# d.h. dass der Erwartungswert ist nicht konstant 0 ist.

# Normal Plot:
# Datenpunkte streuen recht gut um eine Gerade. Es gibt keine Hinweise,
# dass die Normalverteilungsannahme verletzt ist.

# Scale-Location-Plot:
# Der Glätter ist einigermassen horizontal. Die Varianz der kann als konstant
# angenommen werden.

# Unabhängigkeit:
# Da die zeitliche Reihenfolge der Messungen unbekannt ist, ist es sinnlos,
# sich über zeitliche Korrelationen (d.h. Verletzung der Unabhängigkeit der Residuen)
# den Kopf zu zerbrechen.

# Fazit:
# Die Anpassung ist ungenügend, da systematische Abweichungen im Erwartungswert vorkommen.


# (b)
# Wenden Sie die First-Aid Transformationen auf die erklärende Variable und die
# Zielvariable an, passen Sie das Regressionsmodell mit den transformierten
# Grössen an und beurteilen Sie grafisch, ob die Vorraussetzungen nun erfüllt
# sind.

# Ziel- und Prädiktor-Variable logarithmieren
windmill$LogStrom <- log(windmill$Strom)
windmill$LogWindgeschwindigkeit <- log(windmill$Windgeschwindigkeit)
fit.wm2 <- lm(LogStrom ~ LogWindgeschwindigkeit, data = windmill)
par(mfrow = c(1, 1))
plot(LogStrom ~ LogWindgeschwindigkeit, data = windmill)
abline(fit.wm2, col = 'red')
resplot(fit.wm2, 1:3)

# Ergebnis

# Tukey-Anscombe-Plot:
# Der Glätter zeigt immer noch eine Bananenform, d.h. der Erwartungswert
# ist nicht konstant 0. Des Weiteren gibt es nun einen Ausreisser (Beobachtung 25).

# Normal Plot:
# Datenpunkte liegen, abgesehen vom Ausreisser, recht gut auf einer Gerade.

# Scale-Location-Plot:
# Der Glätter verläuft wellenförmig. Es gibt Varianzschwankungen.

# Fazit:
# Anpassung ist ungenügend, da systematische Abweichungen im Erwartungswert
# vorkommen, die Varianz nicht konstant ist und ein Ausreisser sichtbar ist.
# Die First-Aid-Transformationen führen meist, aber eben nicht immer zu einem
# guten Modell.


# (c)
# Aus der Theorie ist folgender funktionaler Zusammenhang bekannt:
#    Strom ≈ α + β * 1 / Windgeschwindigkeit
# Passen Sie dieses Modell aus der Fachtheorie mittels einer Regression an
# (d.h. Strom als Zielgrösse und x = 1/Windgeschwindigkeit als erklärende Variable)
# und stellen Sie das Regressionsmodell in einem Streudiagramm dar.

# Einzeichnen in transformierte Graphik (x=1/Windgeschwindigkeit)
windmill$x <- 1 / windmill$Windgeschwindigkeit
fit.wm3 <- lm(Strom ~ x, data = windmill)
par(mfrow = c(1, 1))
plot(Strom ~ x, data = windmill, xlab = "1 / Windgeschwindigkeit")
abline(fit.wm3, col = 'red')

# geschätzte Koeffizienten
coef(fit.wm3)
# (Intercept)           x 
#     2.97886   -15.51546 

# Einzeichnen in Original-Daten
plot(Strom ~ Windgeschwindigkeit, data = windmill,
     ylim = c(0, coef(fit.wm3)[1]),
     xlim = c(0, max(windmill$Windgeschwindigkeit)))

# plot(Strom ~ Windgeschwindigkeit, data = windmill,
#      ylim = c(0, max(windmill$Strom)),
#      xlim = c(0, max(windmill$Windgeschwindigkeit)))
# graue x- und y-Achse
abline(v = 0, h = 0, col = "grey")
# x-Range für den Messungen vorliegen
x <- seq(min(windmill$Windgeschwindigkeit),
         max(windmill$Windgeschwindigkeit),
         length = 50)
# x- und y-Werte aus dem Modell
lines(x, coef(fit.wm3)[1] + coef(fit.wm3)[2] * (1/x))


# (d)
# Was bedeuten die beiden Parameter α und β im Modell aus der Fachtheorie?
# alpha - die max. Stromausbeute
# - beta / alpha : minimale Windgeschwindigkeit bei der ueberhaupt Strom
# produziert wird.
(-coef(fit.wm3)[2]/coef(fit.wm3)[1])



# (e)
# Prüfen Sie mittels Residuenanalyse, ob die Voraussetzung für das Modell aus
# der Fachtheorie erfüllt sind.
par(mfrow = c(1, 3))
plot(fit.wm3, 1:3)
load(paste0(data.path, "resplot.rda"))
resplot(fit.wm3, plots = 1:3)
# Tukey-Anscombe-Plot:
# Die Punkte sind viel gleichmässiger um die horizontale Nulllinie gestreut.
# Glätter zeigt zwar noch eine ganz leichte Bananenform. Diese ist aber vernachlässigbar.
# Normalplot:
# Datenpunkte streuen bis auf das rechte Ende gut um eine Gerade. Das rechte Ende ist ein
# Hinweis auf Kurzschwänzigkeit, welche aber für die Regressionsergebnisse ungefährlich ist.
# Scale-Location-Diagramm: Der Glätter zeigt eine kleine Delle. Da die Punkte aber sehr
# weit verstreut liegen, befindet sich diese Delle innerhalb der stochastischen Fluktuation.
# Fazit:
# Die Anpassung ist ok.

# (f)
# Gibt es einen signifikanten Zusammenhang zwischen Windgeschwindigkeit und der
# Stromgewinnung aufgrund des Modell aus der Fachtheorie? Führen Sie einen geeigneten
# Test auf dem 5% Niveau durch.

# t-Test zur Nullhypothese: H0 : β = 0, HA : β ̸= 0
summary(fit.wm3)$coefficients
# Der t-Test verwirft die Nullhypothese, da der p-Wert < 0.05. Es gibt somit einen auf 5% 
# signifikanten Einfluss von Windgeschwindigkeit auf die Stromgewinnung

# (g)
# Was sind plausible Werte für die maximale Stromgewinnung der Windmühle?
# Geben Sie ein 95% Vertrauensintervall an.
confint(fit.wm3, level = 0.95, parm = 1)
#                  2.5 %   97.5 %
#   (Intercept) 2.885973 3.071748
# also mit 95% Wahrscheinlichkeit zwischen 2.89 und 3.07 Ampere

# parm ist hier der 1. parameter α (Achsenabschnitt)
# (β waere 2)

# (h)
# Machen Sie eine Vorhersage, wie gross die erwartete Stromproduktion bei einer
# Windgeschwindigkeit von 15 m/s ist. Geben Sie ein 95%-Prognoseintervall an.
x0 <- data.frame(x=1/15)
predict(fit.wm3, newdata = x0, interval = 'prediction')
#        fit      lwr     upr
# 1 1.944496 1.744763 2.14423
# erwartete Stromgewinnung: 1.94 Ampere
# mit 95% Wahrscheinlichkeit zwischen 1.74 und 2.14 Ampere



################### Aufgabe 2. ################### 

highway <- read.csv(paste0(data.path, 'highway.csv'))
str(highway)

# (a)
# Passen Sie eine Regressionsgerade (runoff als Zielgrösse und rain als
# erklärende Grösse). Visualisieren Sie die geschätzte Gerade.
par(mfrow = c(1, 1))
plot(runoff ~ rain, data = highway,
     main = "Abflussmenge vs. Regenfallvolumen",
     xlab = "Regenfallvolumen",
     ylab = "Abflussmenge")
fit.hw <- lm(runoff ~ rain, data = highway,)
abline(fit.hw, col = 'red')

# (b)
# Welcher Anteil der beobachteten Variation in der Abflussmenge kann mit dem
# einfachen linearen Regressionsmodell erklärt werden?
summary(fit.hw)$r.squared
# [1] 0.9752689
# 97.53% der Variation der Abflussmenge kann mit dem einfachen linearen
# Regressionsmodell erklaert werden.

#summary(fit.hw)$adj.r.squared


# (c)
# Besteht ein auf 5% signifikanter linearer Zusammenhang zwischen Abflussmenge
# und Regenfallvolumen? Geben Sie auch die anschauliche Interpretation des
# Regressionskoeffizienten an.

summary(fit.hw)$coefficients
#               Estimate Std. Error    t value     Pr(>|t|)
# (Intercept) -1.1283048 2.36778251 -0.4765238 6.416111e-01
# rain         0.8269731 0.03652408 22.6418585 7.896130e-12

# Ja, der Zusammenhang ist hochsignifikant