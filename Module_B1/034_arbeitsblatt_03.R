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
