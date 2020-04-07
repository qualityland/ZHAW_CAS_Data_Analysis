### Wednesday, 04.03.2020
# Statistische Grundlagen der Datenanalyse
# Lektor:
#   Thoralf Mildenberger

# Slides:
#   CAS-DA_ModulA2-HT1_Folien.pdf


############## Analyse von Zähldaten mit der Poisson-Verteilung ###############
#library(readr)
#hefezellen <- read_csv("./Module_A2/data/Hefezellen.dat", col_names = FALSE)
hefezellen <- read.table("./Module_A2/data/Hefezellen.dat")
hefe <- hefezellen$V1

# Datensatz ist auch im Package HistData enthalten:
#library(HistData)
#YeastD.mat

# Anzahl Hefezellen pro Quadrat
# Wertebereich: 1 - 12
# mean:         4.68
summary(hefe)

# Wertetabelle
# Wert und abs. Häufigkeit
table(hefe)

# Anzahl ausgezählter Quadrate
length(hefe)

# Summe gezählter Hefezellen
sum(hefe)

# Absolute Häufigket HZ pro Quadrat
barplot(table(hefe))

# Rel. Häufigket HZ pro Quadrat
barplot(table(hefe) / length(hefe))

# Kontingenz- / Häufigkeitstabelle
crosstab <- table(hefe)

# Hefezellen (HZ) / Quadrat
k <- as.integer(names(crosstab))

# Häufigkeit des HZ-Werts
hk <- as.integer(crosstab)

# type = "h": high density lines
plot(k, hk, type = "h", xlab = "k (Hefezellen / Quadrat", ylab = "hk (Häufigkeit von k)")
lines(k, hk, type = "b", col = "red") # add line

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


HZ <- data.frame(k=as.integer(names(table(hefezellen))),
                 freq=as.integer(table(hefezellen)))
HZ
# bar chart der Haeufigkeit von Hefezellen pro Quadrat
plot(
  HZ$k,
  HZ$freq,
  type = "h",
  lwd = 6,
  lend = 2,
  col = "blue",
  xlab = "k",
  ylab = "Haeufigkeit"
)

# Totale Anzahl Hefezellen
n <- sum(HZ$freq)
n

# λ schaetzen
mu <- sum(HZ$freq*HZ$k)/n 
mu

# Modellanpassung
# n * pk(Hut) bestimmen
yModell <- n * dpois(HZ$k, lambda=mu)
# n * pk(Hut) einzeichnen
lines(HZ$k, yModell, type="b", lwd=2, col="red")



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
