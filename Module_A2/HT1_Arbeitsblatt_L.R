##
## CAS Datenanalyse:
## Modul 'Statistische Grundlagen der Datenanalyse'
##
## Lösung HT1
## ************************************************

## Aufgabe 1 (i)
## (a)
alpha <- read.table("CAS-DA_ModulA2-HT1_Daten/alpha.dat", header=TRUE)
str(alpha)

plot(alpha$k, alpha$freq, type="h", lwd=6, col="blue", lend=2, xlab="k",
     ylab="Häufigkeit", las=1)


## (b)
n <- sum(alpha$freq)
mu <- sum(alpha$freq*alpha$k)/n
mu   ##  = 3.877871
## Alternative Berechnung:
mu <- mean(rep(alpha$k, alpha$freq))

## (c)
## Wahrscheinlichkeiten mit geschätztem lambda berechnen:
yModell <- dpois(alpha$k, lambda=mu) * n
## In Grafik einzeichnen:
lines(alpha$k, yModell, type="b", lwd=2, col="red")



## Aufgabe 1 (ii)
## (a)
vk <- read.table("CAS-DA_ModulA2-HT1_Daten/verkehr.dat", header=TRUE)
plot(vk$k, vk$freq, type="h", lwd=6, col="blue", lend=2, xlab="k",
            ylab="Häufigkeit", las=1)

## (b)
n <- sum(vk$freq)
mu <- sum(vk$freq*vk$k)/n
mu ## 3.893333
## Alternative Berechnung:
mean(rep(vk$k, vk$freq))

## (c)
yModell <- dpois(vk$k, lambda=mu) * n
lines(vk$k, yModell, type="b", lwd=2, col="red")


## ------------------------------------------------------------------------
##
## Aufgabe 2
## *********

## (a)
plot(c(1,6), c(0,0.2), type="n", xlab="k", ylab="Wahrscheinlichkeit",
     las=1, main="Würfel")
lines(1:6, rep(1/6,6), type="h", lwd=6, col="blue", lend=2)
axis(side=2, at=1/6, labels="1/6", cex=0.8, las=1)
abline(h=0, col="gray", lwd=2)


## (b)
## E[X] = 1*1/6 + 2*1/6 + 3*1/6 + 4*1/6 + 5*1/6 + 6*1/6
##      = (1 + 2 + 3 + 4 + 5 + 6) / 6 = 21/6 = 3.5

## var[X] = (1-3.5)^2*1/6 + (2-3.5)^2*1/6 + (3-3.5)^2*1/6 + (4-3.5)^2*1/6
##           + (5-3.6)^2*1/6 + (6-3.5)^2*1/6 =
## mit R:
((1-3.5)^2*1/6 + (2-3.5)^2*1/6 + (3-3.5)^2*1/6 + (4-3.5)^2*1/6
           + (5-3.5)^2*1/6 + (6-3.5)^2*1/6)
## = 2.916667

## sd[X] = sqrt(var[X]) = sqrt(2.868333)
sqrt(2.916667)  ## = 1.707825

##
## Eleganter:
## E[X] =
(xq <- sum((1:6)/6))  ## = 3.5
## var[X] =
(s2 <- sum(((1:6)-xq)^2/6)) ## = 2.916667
## sd[X]
sqrt(s2)  ## = 1.707825


## (c) Augenzahl 1 bis 12
## E[X] =
(xq <- sum((1:12)/12))  ## = 6.5
## var[X] =
(s2 <- sum(((1:12)-xq)^2/12)) ## = 11.91667
## sd[X]
sqrt(s2)  ## = 3.452053

## (c') Augenzahl 0 bis 12
## E[X] =
(xq <- sum((0:12)/13))  ## = 6
## var[X] =
(s2 <- sum(((0:12)-xq)^2/13)) ## = 14
## sd[X]
sqrt(s2)  ## = 3.741657

## ------------------------------------------------------------------------

## Aufgabe 3
## *********

## (a)
A1 <- read.table("CAS-DA_ModulA2-HT1_Daten/asbest1.dat", header=F)[,1]
range(A1)  ## 16 34 (min=16, max=34)
           ## Spannweite = 34 - 16 = 18
diff(range(A1))
mean(A1)   ## = 24.91304

## (b)
A2 <- read.table("CAS-DA_ModulA2-HT1_Daten/asbest2.dat", header=F)[,1]
range(A2)  ## 13 38
           ## Spannweite = 38 - 13 = 25
mean(A2)   ## = 24.64

## Spannweite hat zugenommen; der Mittelwert ist in etwa gleich


## (c)
par(mfrow=c(1,2)) ## Zwei Grafiken nebeneinander
A1T <- table(A1)
plot(A1T, lwd=6, col="blue", lend=2, xlab="k", ylab="Häufigkeit", las=1)
xq1 <- mean(A1)
k1 <- min(A1):max(A1) ## an diesen Werten von k sollen die
                      ## Wahrscheinlichkeiten berechnet werden
yModell1 <- dpois(k1, lambda=xq1) * sum(A1T)
lines(k1, yModell1, type="b", lwd=2, col="red")##
A2T <- table(A2)
plot(A2T, lwd=6, col="blue", lend=2, xlab="k", ylab="Häufigkeit", las=1)
xq2 <- mean(A2)
k2 <- min(A2):max(A2) ## an diesen Werten von k sollen die
                      ## Wahrscheinlichkeiten berechnet werden
yModell2 <- dpois(k2, lambda=xq2) * sum(A2T)
lines(k2, yModell2, type="b", lwd=2, col="red")

## In der ersten Grafik zeigt sich, dass die einzelnen Balken nur die Werte
## 0, 1, 2 und 3 annehmen. Gemäss dem angepassten Poisson-Modell erwarten
## wir in den einzelnen Balken auch nicht höhere Werte. Wir haben also viel
## zu wenig Beobachtungen, um zu beurteilen, ob das Poisson-Modell die Daten
## gut beschreibt.
## Bei der zweiten Grafik ist die Übereinstimmung schon besser beurteilbar.
## Aber eigentlich hat es da immer noch zu wenig Beobachtungen. Die
## Erwarteten Häufigkeiten in den Balken (=die roten Modellpunkte) sind
## immer noch tief.


## ------------------------------------------------------------------------
## Aufgabe 4
## *********

## (a)
## Die Zufallsvariable ist die Anzahl tödlicher Segelunfälle pro Jahr
## Da es sich hier um Zähldaten handelt, die sich auf eine Zeiteinheit
## beziehen, und da man annehmen kann, dass die Ereignisse unabhängig
## voneinander und mit konstanter Rate eintreten, bietet sich zur
## Modellierung von X eine Poisson-Verteilung an:
## X ~ Pois(lambda) mit lambda = 4.21

## (b)
## Plausibler Bereich mit Faustregel P1: 4.21 +/- 2*sqrt(4.21)
4.21 + c(-1,1)*2*sqrt(4.21)   ## [0.1063431, 8.3136569]



## FREIWILLIG: Exakt zum Niveau von 5%:
## (ppois gibt die kumulierte Verteilungsfunktion F(k))
## (die Wahl von k ist aufgrund der Fausregel P1 erfolgt)
k <- 0:10
cbind(k, "F(k)"=ppois(k, 4.21), "1-F(k-1)"=1-ppois(k-1, 4.21))
##        k       F(k)   1-F(k-1)
##  [1,]  0 0.01484637 1.00000000
##  [2,]  1 0.07734958 0.98515363
##  [3,]  2 0.20891884 0.92265042
##  [4,]  3 0.39355436 0.79108116
##  [5,]  4 0.58788325 0.60644564
##  [6,]  5 0.75150818 0.41211675
##  [7,]  6 0.86631834 0.24849182
##  [8,]  7 0.93536845 0.13368166
##  [9,]  8 0.97170607 0.06463155
## [10,]  9 0.98870400 0.02829393
## [11,] 10 0.99586013 0.01129600

## plausibler Bereich: untere Grenze: 1;   obere Grenze: 9
## d.h. der plausible Bereich ist 1 bis 9

## Gemäss beiden Bestimmungen ist ein Wert von 7 Unfällen auf dem
## 5% Niveau durchaus plausible.


## ------------------------------------------------------------------------

