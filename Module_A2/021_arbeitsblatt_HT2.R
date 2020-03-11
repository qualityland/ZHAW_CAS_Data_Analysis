library(readr)

# 1. Erdbeben
# Arbeitsblatt HT2: Poisson-und Binomial-Modell
# Die Anzahl der Erdbeben in einer bestimmten Zeiteinheit wird oft mit einer
# Poisson-Verteilung modelliert.

# 1a.
# Angenommen, der langfristige Mittelwert fuer Erdbeben der Staerke von
# mindestens 2.0 sei λ0 = 64.
# Waere die Beobachtung von 80 Erdbeben mit dieser Hypothese vereinbar?
# Beantworten Sie diese Frage mit Faustregel P1 und mit dem P-Wert zum 5% Niveau.

# 95% Streubereich (plausible fuer x)
# berechnet mit Faustregel (x + 2 * +/-sqrt(x + 1)):
# x = 64
64 + c(-1, 1) * 2 * sqrt(64)
# ja, 80 ist gerade noch im plausiblen Bereich

# 1b.
# Vom 21. bis 25. Oktober 1995 wurden 8 Erdbeben der Staerke groesser als 5
# beobachtet. Falls diese Ereignisse einer Poisson-Verteilung mit Parameter λ
# folgen, fuer welche “wahren” Intensitaeten (Parameterwerte λ) waere der
# beobachtete Wert noch plausibel?


# 2.
# Konfindezintervalle fuer λ
# Bestimmen Sie jeweils das 95%-Vertrauensintervall fuer λ mit den Datensaetzen
# aus Aufgabe 1 und 2 des HT1. Verwenden Sie dazu poisson.test(...).
# Freiwillig: Benutzen Sie die Faustregel P2’ und P2, um das jeweilige
# 95%-Vertrauensintervall zu bestimmen.

# alpha
alpha <- read.table("./Module_A2/data/alpha.dat", header = TRUE)
# Rekonstruktion der Original-Messdaten
h.alpha <- rep(alpha$k, alpha$freq)
poisson.test(sum(h.alpha), T=length(h.alpha), conf.level = 0.95)
#        Exact Poisson test
# 
# data:  sum(h.alpha) time base: length(h.alpha)
# number of events = 10129, time base = 2612, p-value < 2.2e-16
# alternative hypothesis: true event rate is not equal to 1
# 95 percent confidence interval:
#  3.802715 3.954139
# sample estimates:
# event rate 
#   3.877871 


# verkehr
verkehr <- read.table("./Module_A2/data/verkehr.dat", header = TRUE)
# Rekonstruktion der Original-Messdaten
h.verkehr <- rep(verkehr$k, verkehr$freq)
poisson.test(sum(h.verkehr), T=length(h.verkehr), conf.level = 0.95)
#        Exact Poisson test
# 
# data:  sum(h.verkehr) time base: length(h.verkehr)
# number of events = 1168, time base = 300, p-value < 2.2e-16
# alternative hypothesis: true event rate is not equal to 1
# 95 percent confidence interval:
#  3.673228 4.123182
# sample estimates:
# event rate 
#   3.893333 
