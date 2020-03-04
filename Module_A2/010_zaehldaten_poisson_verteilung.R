### Wednesday, 04.03.2020
# Statistische Grundlagen der Datenanalyse
# Lektor:
#   Thoralf Mildenberger

# Slides:
#   CAS-DA_ModulA2-HT1_Folien.pdf


############## Analyse von Zähldaten mit der Poisson-Verteilung ###############
library(readr)
hefezellen <- read_csv("./Module_A2/data/Hefezellen.dat", col_names = FALSE)
hefe <- hefezellen$X1

# data set also available in package HistData
library(HistData)
YeastD.mat

# explore data set
summary(hefe)
table(hefe)

# bar chart
barplot(table(hefe))

# 
tab <- table(hefe)
names(tab)
plot(as.integer(names(tab)), as.integer(tab), type = "h")   # h: high density lines
lines(as.integer(names(tab)), as.integer(tab), type = "b", col = "red") # add line

# tab <- table(hefe)
tab
prop.table(tab)
sum(tab)     
tab / sum(tab)

k <- as.integer(names(tab))     # Auspraegungen
k

hk <- as.integer(tab)           # abs. Haeufigkeiten
hk

fk <- hk / sum(hk)              # rel. Haeufigkeiten
fk

mean(hefe)
sum()



##################### Arbeitsblatt: Radioaktiver Zerfall ######################
# read in data
#alpha <- read.csv("./Module_A2/data/alpha.dat", header = TRUE, sep = " " )
alpha <- read.table("./Module_A2/data/alpha.dat", header = TRUE)
alpha

# plot der Daten mit dickeren blauen Balken
plot(
  alpha$k,
  alpha$freq,
  type = "h",
  lwd = 6,
  col = "blue",
  lend = 2,
  xlab = "k",
  ylab = "Haeufigkeit"
)


# Anzahl Messungen
n <- sum(alpha$freq)
n


# Anzahl Zerfaelle
x <- sum(alpha$freq * alpha$k)


# Schaetzen von lambda ()
mu <- sum(alpha$freq * alpha$k) / n
mu


# ideale Poisson Verteilung
yModell <- dpois(alpha$k, lambda = mu) * n


# ideale Verteilungs-Kurve in Plot einzeichnen (rot)
lines(alpha$k, yModell, type="b", lwd=2, col="red")


# Original Datensatz rekonstruieren
x <- rep(alpha$k, alpha$freq)
x


######################## Arbeitsblatt: Verkehrsdaten ##########################
alpha <- read.table("./Module_A2/data/verkehr.dat", header = TRUE)
alpha

# plot der Daten mit dickeren blauen Balken
plot(
  alpha$k,
  alpha$freq,
  type = "h",
  lwd = 6,
  col = "blue",
  lend = 2,
  xlab = "k",
  ylab = "Haeufigkeit"
)


# Anzahl Messungen
n <- sum(alpha$freq)
n

# Anzahl Rechtsabbieger
x <- sum(alpha$freq * alpha$k)

# Schaetzen von lambda ()
mu <- sum(alpha$freq * alpha$k) / n
mu

# ideale Poisson Verteilung
yModell <- dpois(alpha$k, lambda = mu) * n

# ideale Verteilungs-Kurve in Plot einzeichnen (rot)
lines(alpha$k, yModell, type="b", lwd=2, col="red")


# Original Datensatz rekonstruieren
x <- rep(alpha$k, alpha$freq)
x

######################## Arbeitsblatt: Wuerfeln ##########################
# Aufgabe 2 - Wuerfel mit 6 Seiten

k <- 1:6
k

pk <- rep(1/6, 6)
pk

plot(k,
     pk,
     type = "h"
     )

mu <- sum(k * pk)
mu

sigma2 <- sum(pk * (k - mu) ^ 2)
sigma2

sigma <- sqrt(sigma2)
sigma


# Aufgabe 2 - Wuerfel mit 12 Seiten

k <- 1:12
k

pk <- rep(1/12, 12)
pk

plot(k,
     pk,
     type = "h"
)

mu <- sum(k * pk)
mu

sigma2 <- sum(pk * (k - mu) ^ 2)
sigma2

sigma <- sqrt(sigma2)
sigma


# Aufgabe:
# Ziehen Sie mit rpois() 10 Zufallszahlen aus einer Poisson-Verteilung mit λ=55
# und berechnen Sie den Mittelwert.

# – Entspricht dieser Mittelwert genau λ?
# – Was passiert, wenn Sie das Ganze wiederholen?
# – Was passiert, wenn Sie eine grössere Stichprobe verwenden?

x <- rpois(n = 10, lambda = 55)
mean(x)

x <- rpois(n = 100, lambda = 55)
mean(x)

x <- rpois(n = 1000, lambda = 55)
mean(x)

x <- rpois(n = 100000, lambda = 55)
mean(x)

x <- rpois(n = 10000000, lambda = 55)
mean(x)



# Bestimmung der Grenzen 2.5% und 97.5%

k <- 0:10
plot(k, dpois(k, lambda = 4.68), type = "h")

data.frame(
  k = k,
  ppois = ppois(k, lambda = 4.68),
  rechts = 1 - ppois(k, lambda = 4.68)
)

# testen ob 1:9 Zellen pro Quadrant wahrscheinlicher als 95% ist
sum(dpois(1:9, lambda = 4.68))    # yeah! fast 97%!
