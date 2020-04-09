# Arbeitsblatt 01

# Aufgabe 1 (Flughafen Zuerich)
# -----------------------------

# b)
# Anzahl Passagiere (Pax) in Abhaengigkeit von den Flugbewegungen (ATM).

# alpha_hat = −1197682.1, beta_hat = 138.8

# y fuer x = 19923
-1197682.1 + 138.8 * 19923  # y = 1567630

# y fuer x = 23004
-1197682.1 + 138.8 * 23004  # y = 1995273


# c)
# Laden Sie die Daten in R, passen Sie die Regressionsgerade an und vergleichen
# Sie, ob Sie dieselben Koeffizieten wie in (b) erhalten.

load("./Module_B1/data/flughafen.rda")
str(zrh)

fit.zh <- lm(Pax ~ ATM, data = zrh)

coef(fit.zh)
# (Intercept)           ATM 
# -1197682.0740      138.7617

plot(zrh$ATM, zrh$Pax)
abline(fit.zh, col = "red")

# d)
# Welche Anzahl Passagiere gibt es laut Modell, falls keine Flugbewegungen
# durchgeführt werden (ATM = 0)? Halten Sie diese Anzahl für plausibel? Können
# Sie sich die Anzahl erklären?

# Antwort: -1197682.0740, macht keinen Sinn


# Aufgabe 2 (Antike Uhren)
# ------------------------

# a)
# Stellen Sie die Daten in einem Streudiagramm Preis (y-Achse) gegen Alter
# (x-Achse) dar und beschreiben Sie den funktionalen Zusammenhang in Worten.

uhren <- read.table("./Module_B1/data/AntikeUhren.dat", header = TRUE)

plot(uhren$Alter, uhren$Preis)
# Antwort: positive lineare Regression

# b)
# Passen Sie eine Gerade an die Datenpunkte an. Geben Sie die geschätzten
# Koeffizientenwerte an. Wie lautet die angepasste Geradengleichung?
fit.uhren <- lm(Preis ~ Alter, data = uhren)

coef(fit.uhren)
# (Intercept)       Alter 
# -191.65757    10.47909 

# Geradengleichung: y = -191.65757 + 10.47909 * x

# c)
# Welche Auswirkung hat eine um ein Jahr ältere Uhr auf den erwarteten
# Auktionspreis?

# Antwort: Er wird um $10.47 hoeher sein.

# d)
summary(fit.uhren)$sigma
# 273.0284

# Antwort: Im mittel weicht der Preis um $273 von der Regressionsgeraden ab.

# f)
# Zeichnen Sie die Gerade in das Streudiagramm von Teilaufgabe (a) ein.
# Kommentieren Sie die Lösung.
abline(fit.uhren, col = "red")

# Antwort: Die realen Preise weichen z.T. betraechtlich von der Regressions-
#          geraden ab. Andere Faktoren scheinen ebenfalls eine Rolle bei der
#          Preisgestaltung zu spielen (Zustand, Materialien, Hersteller,
#          Seltenheit).


# Aufgabe 3 (Gotthard Strassentunnel)

# Wir betrachten in dieser Aufgabe einen Datensatz, welcher über die Jahre
# 2004-2016 die Anzahl Tage mit Stau vor dem Gotthard Strassentunnel Nordportal
# beschreibt. Lesen sie diese Daten in R (gotthard.rda).
load("./Module_B1/data/gotthard.rda")

str(gotthard)

# a)
# Stellen Sie die Daten in einem Streudiagramm dar. Gibt es einen Trend?
plot(gotthard$jahr, gotthard$stautage)
plot(stautage ~ jahr, data = gotthard)

# Antwort: Zunahme (pos Regression) ueber die Jahre.

# b)
# Passen Sie eine Gerade an die Datenpunkte an. Geben Sie die geschätzten
# Werte der beiden Koeffizienten an.

fit.gh <- lm(stautage ~ jahr, data = gotthard)

abline(fit.gh, col = "red")

coef(fit.gh)
# (Intercept)         jahr 
# -11815.13187      5.93956 


# c)
# Wie viele Stautage werden vom Modell für 2016 geschätzt? Was ist das Residuum
# für diesen Datenpunkt?

# Stautage 2016
fitted(fit.gh)[gotthard$jahr == 2016]
# 13 
# 159.022 

# Residuum 2016
resid(fit.gh)[gotthard$jahr == 2016]
# 13 
# -19.02198 


# d)
# Zeichnen Sie die Gerade in das Streudiagramm von Teilaufgabe (a) ein.
# Halten Sie das lineare Regressionsmodell für plausibel?
abline(fit.gh, col = "red")

# Antwort: Ja, bis 2014, dann scheinen die Stautage abzunehmen.
