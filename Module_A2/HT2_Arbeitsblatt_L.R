##
## CAS Datenanalyse:
## Modul 'Statistische Grundlagen der Datenanalyse'
##
## Lösung HT2
## **********

## Aufgabe 1
## *********

## (a)
## 95%-Streubereich mit Faustregel P1:
64 + c(-1,1)*2*sqrt(64)   ## [48, 80]
## Die Beobachtung von x=80 Erdbeben ist im 95%-Streubereich der Poisson-
## Verteilung mit Parameter lambda=64 und deshalb ist der Wert x=80 mit dem
## Modell vereinbar. Oder anders ausgedrückt: Wir haben mit der Beobachtung
## x=80 keine Evidenz gegen die Null-Hypothese "Poisson-Verteilung mit
## Parameter lambda=64" auf dem 5% Niveau.

poisson.test(x=80, r=64)
## number of events = 80, time base = 1, p-value = 0.05221
## alternative hypothesis: true event rate is not equal to 64
## 95 percent confidence interval:
##  63.43502 99.56692
## sample estimates:
## event rate
##         80

## Da der P-Wert mit 0.05221 grösser als das Niveau von 5% ist, kann die
## Null-Hypothese "Poisson-Verteilung mit Parameter lambda=64" auf dem
## 5% Niveau nicht verworfen werden.


## (b)
poisson.test(x=8)
## 95 percent confidence interval:
##   3.453832 15.763189

## Bemerkung:
## Da hier kein Wert für lambda (d.h. für r in poisson.test(...)) unter der
## Null-Hypothese bekannt ist, belassen wir ihn auf dem Default-Wert -
## somit interessiert uns der P-Wert-Teil im Output von poisson.test(...)
## nicht weiter


## Anmerkung: Wir können die Vertrauensintervalle auch für andere
##            Vertrauensniveaus berechnen:
poisson.test(x=8, conf.level=0.99)
## 99 percent confidence interval:
##   2.571103 18.578226



## (c)
## Faustregel P2:
8 + c(-1,1)*2*sqrt(8) ##  [2.3, 13.7]

## Faustregel P2':
8 + 2 + c(-1,1)*2*sqrt(8+1) ## [4.0, 16.0]



## (d)
## Faustregel P2:
50 + c(-1,1)*2*sqrt(50) ##  [35.9, 64.1]

## Faustregel P2':
50 + 2 + c(-1,1)*2*sqrt(50+1) ## [37.7, 66.3]

## exakt
poisson.test(x=50)
## 95 percent confidence interval:
##  37.11096 65.91877


## (e)
## Wie erwartet stimmen die Faustregeln bei der grösseren Zahl (x=50)
## besser mit dem exakten Vertrauensintervall überein. Vor allem die
## Faustregel P2' stimmt bei x=50 mit dem exakten Intervall sehr gut überein.




## ------------------------------------------------------------------------
## Aufgabe 2
## *********

## (i) alpha-Daten
##     """""""""""
alpha <- read.table("Daten4ModulA2/alpha.dat", header=TRUE)
h.alpha <- rep(alpha$k, alpha$freq) ## sortierte Beobachtungen
poisson.test(sum(h.alpha), T=length(h.alpha))
## 	Exact Poisson test
## data:  sum(h.alpha) time base: length(h.alpha)
## number of events = 10129, time base = 2612, p-value < 2.2e-16
## alternative hypothesis: true event rate is not equal to 1
## 95 percent confidence interval:
##  3.802715 3.954139
## sample estimates:
## event rate
##   3.877871

## Lösung:
## ## 95 percent confidence interval:  3.802715 3.954139


## (ii) Verkehrs-Daten
##      """"""""""""""
vk <- read.table("Daten4ModulA2/verkehr.dat", header=TRUE)
h.vk <- rep(vk$k, vk$freq) ## sortierte Beobachtungen
poisson.test(sum(h.vk), T=length(h.vk))
## 	Exact Poisson test
## data:  sum(h.vk) time base: length(h.vk)
## number of events = 1168, time base = 300, p-value < 2.2e-16
## alternative hypothesis: true event rate is not equal to 1
## 95 percent confidence interval:
##  3.673228 4.123182
## sample estimates:
## event rate
##   3.893333

## Lösung:
## ## 95 percent confidence interval:  3.673228 4.123182


## (iii) Asbestfasern-Daten
##       """"""""""""""""""
A1 <- read.table("Daten4ModulA2/asbest1.dat", header=F)[,1]
poisson.test(sum(A1), T=length(A1))
## 	Exact Poisson test
## data:  sum(A1) time base: length(A1)
## number of events = 573, time base = 23, p-value < 2.2e-16
## alternative hypothesis: true event rate is not equal to 1
## 95 percent confidence interval:
##  22.91468 27.03901
## sample estimates:
## event rate
##   24.91304

## Lösung:
## ## 95 percent confidence interval:  22.91468 27.03901


## Zweiter Datensatz
A2 <- read.table("Daten4ModulA2/asbest2.dat", header=F)[,1]
poisson.test(sum(A2), T=length(A2))
## 	Exact Poisson test
## data:  sum(A2) time base: length(A2)
## number of events = 1848, time base = 75, p-value < 2.2e-16
## alternative hypothesis: true event rate is not equal to 1
## 95 percent confidence interval:
##  23.52927 25.78962
## sample estimates:
## event rate
##      24.64


## Lösung:
## ## 95 percent confidence interval:  23.52927 25.78962



##
## (Freiwillig) mit Faustregel P2'
## (i) alpha-Daten
##     """""""""""

## Schätzwert für lambda:
sum()/sum(alpha$freq) ##  = 3.877871

(alpha.tot <- sum(alpha$freq*alpha$k)) ##  = 10129

## 95%-Vertrauensintervall für totale Beobachtungszeit(=2612*7.5s)
alpha.tot + 2 + c(-1,1)*2*sqrt(alpha.tot+1) ## 9929.704 10332.296

## 95%-Vertrauensintervall für eine Zeiteinheit (=7.5s):
(alpha.tot + 2 + c(-1,1)*2*sqrt(alpha.tot+1))/sum(alpha$freq)
## 3.801571 3.955703



## (ii) Verkehrs-Daten
##      """"""""""""""
vk <- read.table("Daten4ModulA2/verkehr.dat", header=TRUE)
## Schätzwert für lambda:
sum(vk$freq*vk$k)/sum(vk$freq) ## = 3.893333

(vk.tot <- sum(vk$freq*vk$k))         ## = 1168

## 95%-Vertrauensintervall für die totale Beobachtungszeit (=300*3min):
vk.tot + 2 + c(-1,1)*2*sqrt(vk.tot+1) ## 1101.619 1238.381

## 95%-Vertrauensintervall für eine Beobachtungsperiode (= 3 Minuten):
(vk.tot + 2 + c(-1,1)*2*sqrt(vk.tot+1))/sum(vk$freq)
## 3.672062 4.127938



## (iii) Asbestfasern-Daten
##       """"""""""""""""""
A1 <- read.table("Daten4ModulA2/asbest1.dat", header=F)[,1]
## Schätzwert für lambda:
mean(A1) ## = 24.91304

(A1.tot <- sum(A1))   ## = 573

## 95%-Vertrauensintervall für die totale Beobachtungsfläche (=23*3mmFilter):
A1.tot + 2 + c(-1,1)*2*sqrt(A1.tot+1) ##  527.0834 622.9166

## 95%-Vertrauensintervall für ein Filterstück (3mm Durchmesser):
(A1.tot + 2 + c(-1,1)*2*sqrt(A1.tot+1))/length(A1)
## 22.91667 27.08333



## Zweiter Datensatz
A2 <- read.table("Daten4ModulA2/asbest2.dat", header=F)[,1]
## Schätzwert für lambda:
mean(A2)  ## = 24.64  ## Schätzwert fast gleich wie in (iii)-a)


(A2.tot <- sum(A2))   ## = 1848

## 95%-Vertrauensintervall für die totale Beobachtungsfläche (=75*3mmFilter):
A2.tot + 2 + c(-1,1)*2*sqrt(A2.tot+1) ## 1764 1936

## 95%-Vertrauensintervall für ein Filterstück (3mm Durchmesser):
(A2.tot + 2 + c(-1,1)*2*sqrt(A2.tot+1))/length(A2)
## 23.52000 25.81333
## 95%-Konfidenzintervall ist schmaler mit A2 als mit A1, weil
## es in A2 mehr Daten hat.




## ------------------------------------------------------------------------
## Aufgabe 3
## *********

## (i) alpha-Daten
##     """""""""""
alpha <- read.table("Daten4ModulA2/alpha.dat", header=TRUE)
## aus der Tabelle (wieder) ein Beobachtungsvektor machen
h.alpha <- rep(alpha$k,alpha$freq)  ## Beobachtungen sind sortiert

## (i-1)
## Bootstrap-Vertrauensintervall für die Dispersion
f.disp <- function(x, ind){
    ## x   = ursprünglicher Beobachtungsvektor
    ## ind = Beobachtungsnummer für die Bootstrap-Stichprobe
    xx <-x[ind]       # erzeugen der Bootstrap-Stichprobe
    var(xx)/ mean(xx) # Berechnet die Dispersion für die Bootstrap-Stichprobe
}

library(boot)
set.seed(seed=117)  ## "Startwert" für den Pseudo-Zufallszahlengenerator
alpha.boot2 <- boot(h.alpha, f.disp, R=4999, stype="i")
boot.ci(alpha.boot2, conf=0.95, type="perc")
## Level     Percentile
## 95%   ( 0.8990,  1.0132 )

## Da die Nullhypothese "s^2/xq = 1" im
##      95%-Vertrauensintervall [0.8990, 1.0132]
## liegt, kann die Nullhypothese auf dem 2.5% Niveau nicht verworfen werden.
## Folglich haben wir keine Evidenz gegen die Poisson-Verteilung gefunden.


## (i-2) Chi-quadrat-Test
chisq.test(h.alpha)
## 	Chi-squared test for given probabilities

## data:  h.alpha
## X-squared = 2502.414, df = 2611, p-value = 0.9351

## Da der P-Wert von 0.9351 grösser als das Niveau von 2.5% ist,
## kann die Null-Hypothese "Daten können durch eine Poisson-Verteilung
## beschrieben werden" nicht verworfen werden.


## (i-3) (Freiwillig) Chi-quadrat-Test und Faustregel Chi2
mean(h.alpha)  ## = 3.877871
var(h.alpha)   ## = 3.716599
(h.n <- length(h.alpha)) ## = 2612
(h.n-1)*var(h.alpha)/ mean(h.alpha) ## = 2502.414

(h.n-1) + 2*sqrt(2*(h.n-1)) ## 2755.527
## Faustregel Chi2: Da 2502.414 < 2755.527, ist das Poissonmodell für
##                  diesen Datensatz plausibel


##
## (ii) Verkehrs-Daten
##      """"""""""""""
vk <- read.table("CAS-DA_ModulB_W6_Daten/verkehr.dat", header=TRUE)
h.vk <- rep(vk$k,vk$freq)  ## Beobachtungsvektor


## (ii-1)
## Bootstrap-Vertrauensintervall für die Dispersion
set.seed(seed=141204)
vk.boot2 <- boot(h.vk, f.disp, R=999, stype="i")
boot.ci(vk.boot2, conf=0.95, type="perc")
## Level     Percentile
## 95%   ( 1.130,  1.554 )

## Da die Nullhypothese "s^2/xq = 1" NICHT im
##      95%-Vertrauensintervall [1.1, 1.6]
## liegt, kann die Nullhypothese auf dem 2.5% Niveau verworfen werden.
## Folglich spricht sich dieser Test gegen die Poisson-Verteilung als
## geeignetes Modell für die Daten aus.


## (ii-2) Chi-quadrat-Test
chisq.test(h.vk)
## 	Chi-squared test for given probabilities

## data:  h.vk
## X-squared = 400.3219, df = 299, p-value = 7.935e-05

## Da der P-Wert von 0.00007935 kleiner als das Niveau von 2.5% ist,
## wird die Null-Hypothese "Daten können durch eine Poisson-Verteilung
## beschrieben werden" verworfen.


## (ii-3) (Freiwillig) Chi-quadrat-Test und Faustregel Chi2
mean(h.vk)  ## = 3.893333
var(h.vk)   ## = 5.212664
(h.n <- length(h.vk)) ## = 300
(h.n-1)*var(h.vk)/ mean(h.vk) ## = 400.3219

(h.n-1) + 2*sqrt(2*(h.n-1)) ## 347.9081

## Faustregel Chi2: Da 400.3219 > 347.9081, ist das Poissonmodell für
##                  diesen Datensatz auf dem 2.5% Niveau unplausibel



## (iii) Asbestfasern-Daten (nur mit asbest1.dat)
##       """"""""""""""""""

A1 <- read.table("Daten4ModulA2/asbest1.dat", header=F)[,1]

## (iii-1)
## Bootstrap-Vertrauensintervall für die Dispersion
set.seed(seed=141204)
A1.boot2 <- boot(A1, f.disp, R=999, stype="i")
boot.ci(A1.boot2, conf=0.95, type="perc")
## Level     Percentile
## 95%   ( 0.688,  1.688 )

## Da die Nullhypothese "s^2/xq = 1" im
##        95%-Vertrauensintervall [0.688, 1.688]
## liegt, kann die Nullhypothese auf dem 2.5% Niveau NICHT verworfen werden.
## Folglich haben wir keine Evidenz gegen die Poisson-Verteilung als
## geeignetes Modell für diese Daten.


## (iii-2) Chi-quadrat-Test
chisq.test(A1)
## 	Chi-squared test for given probabilities

## data:  A1
## X-squared = 26.5654, df = 22, p-value = 0.2282

## Da der P-Wert von 0.2282 grösser als das Niveau von 2.5% ist,
## wird die Null-Hypothese "Daten können durch eine Poisson-Verteilung
## beschrieben werden" NICHT verworfen.


## (iii-3) (Freiwillig) Chi-quadrat-Test und Faustregel Chi2
mean(A1)  ## = 24.91304
var(A1)   ## = 30.083
(h.n <- length(A1)) ## = 23
(h.n-1)*var(A1)/ mean(A1) ## = 2755.527545

(h.n-1) + 2*sqrt(2*(h.n-1)) ## 35.2665

## Faustregel Chi2:  Da 26.57 < 35.27, ist das Poissonmodell für
##                   diesen Datensatz auf dem 2.5% Niveau plausibel


##
## Zweiter Datensatz
A2 <- read.table("Daten4ModulA2/asbest2.dat", header=F)[,1]
## nur mit  Chi-quadrat-Test
chisq.test(A2)
## X-squared = 82.4383, df = 74, p-value = 0.2348

## Da der P-Wert von 0.2348 grösser als das Niveau von 2.5% ist,
## wird die Null-Hypothese "Daten können durch eine Poisson-Verteilung
## beschrieben werden" NICHT verworfen.


## --------------------------------------------------------------------------

## Aufgabe 4
## *********

## (a) Es sind die Werte 0,1,2, ..., 9, 10 möglich.



## (b)
## "Kieselsteine"-Experiment in der Klasse (CAS DA20.11):
KS <- c()

table(KS)
## KS

## Der Wert ?? kommt am häufigsten vor.


## (c)
## Damit alle möglichen Werte ausgezählt werden:
(h <- table(factor(KS, levels=paste(0:10))))


(KS.ct <- as.data.frame(h))   ## Tabelle in ein Data-Frame umwandeln
names(KS.ct)[1] <- "k"   ## Name der ersten Spalte ändern auf "k"
str(KS.ct)  ## erste Spalte ist eine Faktorvariable!
## Faktorwerte in Integer-Werte umwandeln:
KS.ct$k <- as.integer(as.character(KS.ct$k)) #

plot(KS.ct$k, KS.ct$Freq, type="h", lwd=6, col="blue", lend=2, xlab="k",
     ylab="Häufigkeit", las=1)


## (d)
## Das Balkendiagramm gleicht am ehesten dem Szenario (iii);
## ist allerdings nicht so eindeutig


##
## (Freiwillig) Daten für dieses Extgermiment aus den vergangenen Kursen:

KSa <- c(4,6,3,5,3,3,3,4,1,2,5,2,3,5,3,4,2,2,4,4,3,4,7, # 13.1
    4,6,3,5,3,3,3,4,1,2,5,2,3,5,3,4,2,2,4,4,3,4,7, # 14.2
    1,2,2,3,3,4,4,4,4,4,5,5,5,5,6,7,7, # 14.3
    0,5,3,1,7,4,6,5,1,6,5,4,4,5,2,3,3,2,4,3,5,6,5,5,7,4,3,3,3,4, #15.4
    5,3,5,2,4,5,3,2,6,3,1,3,2,2,5,4,4,4,6,2, # 16.5
    3,3,4,4,3,7,3,4,4,3,5,7,3,3,4,3,1,4,5,2,3,0,3,5,3, # 17.6
    0,2,2,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,7,7,7 # 19.9
    )
(h <- table(factor(KSa, levels=paste(0:10))))
## 0  1  2  3  4  5  6  7  8  9 10
##  2  7 18 38 33 24  8  8  0  0  0

(KSa.ct <- as.data.frame(h))   ## Tabelle in ein Data-Frame umwandeln
names(KSa.ct)[1] <- "k"   ## Name der ersten Spalte ändern auf "k"
str(KSa.ct)  ## erste Spalte ist eine Faktorvariable!
## Faktorwerte in Integer-Werte umwandeln:
KSa.ct$k <- as.integer(as.character(KSa.ct$k)) #

plot(KSa.ct$k, KSa.ct$Freq, type="h", lwd=6, col="blue", lend=2, xlab="k",
     ylab="Häufigkeit", las=1)
## Das Balkendiagramm gleicht ziemlich klar dem Szenario (iii);

## --------------------------------------------------------------------------
