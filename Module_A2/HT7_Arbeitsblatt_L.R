##
## CAS Datenanalyse:
## Modul 'Statistische Grundlagen der Datenanalyse'
##
## Lösung HT 7
## ************************************************

## Aufgabe 1
## *********
#load("Daten4ModulA2/SpeedOfLight.RData") # --> SpeedOfLight
load("./Module_A2/data/SpeedOfLight.RData") # --> SpeedOfLight

SoL <- SpeedOfLight
head(SoL)

## (a) Visualisierung:
#source("../CAS-DA_ModulA2_RFn-myqqnorm.R")  ## --> myqqnorm()
source("./Module_A2/RFn-qqnormSim.R")  ## --> qqnormSim()
par(mfrow=c(2,2))
boxplot(SoL, las=1, col="blue", horizontal=TRUE,
        xlab="Abweichung von 24800 ns",  ylab="")
hist(SoL, col="gray", las=1); box()
qqnormSim(SoL, SEED=1102); qqline(SoL) ## Ausreisser knapp sichtbar.
qqnormSim(SoL, rob=TRUE, SEED=1102); qqline(SoL)
## Es sind klar 2 Ausreisser auf der linken Seite sichtbar.
## Die Daten sind also nicht normalverteilt!

## ohne Ausreisser:
SoLr <- SoL[SoL>0]  ## Ausreisser haben hier einen Wert kleiner 0!
qqnormSim(SoLr, rob=TRUE, SEED=1102); qqline(SoLr)
## Für die restlichen Daten haben wir keine Evidenz, dass Sie nicht
## normalverteilt sein könnten.


## (b) t-Test
## Mit allen Beobachtungen
t.test(SoL, alternative="two.sided", conf.level=0.95)#
## t = 19.818, df = 65, p-value < 2.2e-16
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  23.57059 28.85365
## sample estimates:
## mean of x
##  26.21212

## 95%-VI: [23.57059, 28.85365]


## Ohne die beiden Ausreisser
t.test(SoLr, alternative="two.sided", conf.level=0.95)
## t = 43.671, df = 63, p-value < 2.2e-16
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  26.4802 29.0198
## sample estimates:
## mean of x
##     27.75

## 95%-VI: [26.4802, 29.0198]
##         Es ist deutlich nach rechts verschoben und deutlich kürzer, obwohl
##         zur Berechnung weniger Daten zur Verfügung stehen.


## (d)
## Vertrauensintervall für die gemessene Lichtgeschindigkeit:
7400 / ((24800 + c(26.4802, 29.0198)) * 1e-9)
## = [298'068'834, 298'038'346]
## Der heute gültige Wert für die Lichtgeschwindigkeit von 299'792'458 m/sec
## liegt nicht in diesem Intervall! Wahrscheinlich liegt diese Abweichung an
## systematischen Messfehlern.




## ----------------------------------------------------------------------------
## Aufgabe 2
## ~~~~~~~~~

P <- data.frame(Proband=1:14,
                Auto1=c(37.0, 25.8, 16.2, 24.2, 22.0, 33.4, 23.8, 58.2,
                         33.6, 24.4, 23.4, 21.2, 36.2, 29.8),
                Auto2=c(17.8, 20.2, 16.8, 41.4, 21.4, 38.4, 16.8, 32.2,
                         27.8, 23.2, 29.6, 20.6, 32.2, 53.8))
(P$Diff <- P$Auto1 - P$Auto2)

##
## (a)
wilcox.test(P$Diff, alternative="two.sided", mu=0, conf.level=0.95)
## oder
wilcox.test(x=P$Auto1, y=P$Auto2, alternative="two.sided", mu=0, paired=TRUE,
       conf.level=0.95)
## 	Wilcoxon signed rank test with continuity correction
## data:  P$Auto1 and P$Auto2
## V = 63.5, p-value = 0.5097
## alternative hypothesis: true location shift is not equal to 0
## Warning message:
## In wilcox.test.default(x = P$Auto1, y = P$Auto2, alternative = "two.sided", :
##   cannot compute exact p-value with ties

## Da der P-Wert von 0.5097 grösser als das Niveau von 0.05 ist,
## kann die Nullhypothese nicht verworfen werden.
## Wir kommen also zum gleichen Schluss wie mit dem t-Test.


##
## (b)
## Fehlentscheid, den wir begangen haben könnten: Wir haben die Nullhypothese
## fälschlicherweise beibehalten. Leider können wir die Wahrscheinlichkeit,
## diesen Fehlentscheid getroffen zu haben, nicht bestimmen, weil die
## Alternative zu wenig spezifisch ist.


## ----------------------------------------------------------------------------
## Aufgabe 3:
## ~~~~~~~~~~
AF <- c(4.5, 11.5, 16, 20.7, 20.8, 31, 34.5, 46, 61 )
length(AF) # = 9

##
## Vorbemerkungen:
par(mfrow=c(2,2))
boxplot(AF, las=1, col="blue", horizontal=TRUE,
        xlab="Verhältnis Breite zu Länge",  ylab="")
stripchart(AF, method="stack", pch=4)
hist(AF, col="gray", nclass=4, las=1); box()
qqnormSim(AF); qqline(AF)
## Anmerkung; kann aus diesen Grafiken nicht schliessen, dass das
## Normalverteilungsmodell ungeeignet ist, die Daten zu beschreiben.
summary(AF)
## Wir werden die Daten wie verlangt mit der Exponetialverteilung
## auswerten.

## (a)
## Mittelwert
mean(AF)  # = 27.33333
## d.h. die mittlere Ausfallzeit beträgt 27'333 Betriebsstunden

## Schätzung der Rate (= lambda)
1/mean(AF) # = 0.03658537
## d.h. es hat 0.0000365 Ausfälle pro Betriebsstunde

## Anpassung visualisieren
par(mfrow=c(1,1))
hist(AF, col="gray", nclass=4, las=1, freq=FALSE, ylim=c(0,0.04)); box()
curve(dexp(x, rate=1/mean(AF)), col="red", add=TRUE)



## (b) 95% Vertrauensintervall:
## (b-i) mit Bootstrap-Perzentil-VI
library(boot)
f.rate <- function(x, ind){
    1/mean(x[ind])
}

set.seed(seed=4711)
AF.boot <- boot(AF, f.rate, R=4999, stype="i")
boot.ci(AF.boot, conf=0.95, type="perc")
## Intervals :
## Level     Percentile
## 95%   ( 0.0257,  0.0581 )
## Calculations and Intervals on Original Scale


## (b-ii) mit Bootstrap-bca-VI
boot.ci(AF.boot, conf=0.95, type="bca")
## Intervals :
## Level       BCa
## 95%    ( 0.0249,  0.0552 )
## Calculations and Intervals on Original Scale

## Die Unterschiede sind klein zwischen den beiden Bootstrap-VIen.
## Nicht überraschend, da nur ganz kleiner Bias und kleine Abweichung
## von der Symmetrie erkennbar:
par(mfrow=c(1,1))
hist(AF.boot$t, col="gray", xlab=expression(lambda~'*'),
     main="Bootstrap-Verteilung")
abline(v=c(1/mean(AF), mean(AF.boot$t)), col=c("red", "blue"),
       lty=c(1,2), lwd=c(3,3))
##


## (b-iii) mit zGWS-Approximation
(h.n <- length(AF))  ## =9
c(-1.96/sqrt(h.n)+1,  1.96/sqrt(h.n)+1)/mean(AF)
##  [0.01268293, 0.06048780]
## Dieses Vertrauensintervall ist deutlich länger als die beiden Bootstrap-VIe.


## (c)
## 1 Ausfall pro 50'000 Betriebsstunden --> Rate = lambda=
1/50 # = 0.02 (in tausend Betriebstunden)
1/50000 # = 0.00002 Ausfälle pro Betriebsstunde

## 0.02 liegt nur im 95% Vertrauensintervall, wenn es auf dem zGWS beruht, sonst nicht.
## Uaf den 95%-Bootstrap-Vertrauensintervallen wird deshalb die  die Nullhypothese
## lambda=0.02 auf dem 5% Niveau verworfen.


## ----------------------------------------------------------------------------
## Aufgabe 4:
## ~~~~~~~~~~
##

WZ <- c(2.92, 13.87, 1.78, 12.62, 5.82, 17.57, 15.98, 2.33, 4.40, 11.10, 8.75)

## (a)
## Schätzung von lambda
1/mean(WZ) # = 0.1132386


## (b) 95% Vertrauensintervall:
## mit Bootstrap
library(boot)
f.lambda <- function(x, ind){
    1/mean(x[ind])
}

set.seed(seed=4711)
WZ.boot <- boot(WZ, f.lambda, R=1999, stype="i")
boot.ci(WZ.boot, conf=0.95, type="perc")
## Level     Percentile
## 95%   ( 0.0834,  0.1787 )
## Calculations and Intervals on Original Scale


boot.ci(WZ.boot, conf=0.95, type="bca")
## Intervals :
## Level       BCa
## 95%   ( 0.0831,  0.1782 )
## Calculations and Intervals on Original Scale


## (freiwillig) mit zGWS-Approximation
c((-1.96/sqrt(11)+1)/mean(WZ),  (1.96/sqrt(11)+1)/mean(WZ))
##  [0.04631887, 0.18015838]



## =*=*=*=*=*=*=*=*=*=*=*=*=*=*=* ENDE *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=




