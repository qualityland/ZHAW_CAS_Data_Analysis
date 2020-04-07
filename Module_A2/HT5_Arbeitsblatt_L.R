##
## CAS Datenanalyse:
## Modul 'Statistische Grundlagen der Datenanalyse' A2
##
## Lösung HT 5
## ***************************************************
##
## mit Ergänzungen bei Aufgabe 1

## Aufgabe 1
## *********
## (a)
## Daten vom CAS Datenanalyse 14.3
EM <- c(6.4,  6.3, 6.2, 6.4, 6.4,  6.3, 6.5, 6.4, 6.2, 6.25,
        6.35, 6.3, 6.2, 7.1, 6.15, 6.2, 6.3, 6.0, 6.0, 7.0)
## Abstand zwischen zwei Knoten hätte  2 cm entsprechen sollen, aber ....
##
## Daten vom CAS Datenanalyse 19.9 siehe Ende dieser Aufgabe.


## (b) alles in Knoten
boxplot(EM, las=1, col="blue", horizontal=TRUE,
        xlab="Länge in 'Knoten'",  ylab="")
## Es hat zwei Ausreisser rechts.
## Nach kurzer Diskussion war klar, dass zwei Personen den Startknoten
## mitgezählt haben, die anderen nicht.
## Die Beschreibung war diesbezüglich nicht klar.

stripchart(EM, method="stack", pch=4)

hist(EM, col="blue", las=1)

scatter.smooth(EM, type="b", col="blue")
## Die Messungen zeigen über die Zeit einen Trend abwärts.
## Das Messsystem war nicht stabil. Durch das häufige strecken
## der Schnur haben sich die Knoten verzogen. So war am Schluss
## die Distanz vom ersten bis zum 8 Knoten 15 cm anstatt der
## vorgesehenen 14 cm
## Im CAS DA 15.4 wurde nach den ersten Messungen festgestellt, dass
## es nicht klar ist, was gemessen werden soll. Das Holzstäbchen ist
## krumm! Soll die Innen- oder die Aussenseite gemessen werden? Man
## hat sich auf die Aussenseite geeinigt und es wurde mit den
## Messungen weitergefahren!

abline(h=mean(EM), col="red")


## (c)
mean(EM)       # = 6.3475

mean(EM * 2)  ## = 12.695
mean(EM) * 2  ## = 12.695  gleich wie oben; also so wie es sein muss

sd(EM*2)      ## 0.5452908

## Falls Sie das kennen (robustes Streumass):
mad(EM*2)            ## 0.29652
sd(EM[-c(14,20)]*2)  ## 0.2703786


## mit Massstab = 12.8 cm
## (auch hier nicht klar, wie man genau Messen muss!
##  Werte zwischen 12.7 und 13.0 sind "möglich")

## Durch Wiederholen der Messungen können wird die Genauigkeit der Messung
## erhöhen.

## Messen ist ja heutzutage so einfach, trotzdem immer wieder mal eine
## grosse Herausforderung

##
## Daten vom CAS Datenanalyse 19.9:
EM9 <- c(5,5,6.3,5.9,6.3,6.2,7.5,6.1,6.5,6.15,6.2,6.1,6.1,6.2,6,6.4,6.1,7.1,
         6.2,6.5,6.5,6.1,6.3,6.2,6.1,6.1,7.5,6.2)
length(EM9)

boxplot(EM9, las=1, col="blue", horizontal=TRUE,
        xlab="Länge in 'Knoten'",  ylab="")
hist(EM9, col="blue", las=1, nclass=12)
## Verteilung unimodal, rechtsschief (mit Ausreisser?)
stripchart(EM9, method="stack", pch=4)
## Gemäss eigenen Aussagen haben nicht wenige den Startknoten mitgezählt.
scatter.smooth(EM9, type="b", col="blue")
## einmal erster Knoten mitgezählt (Beob.9); ab Beobachtung 5 Trend nach unten,
## zuvor ansteigender Trend
##
mean(EM9)     ## = 6.244643 Knoten
## Bei drei Messungen wurde der erste Knoten mitgezählt, bei den ersten beiden
## Messungen wurde der "Nullpunkt" am Anfanf der Schnur gelegt.
## Folglich waren die Anweisungen, wie gemessen werden soll ungenügend.


## ----------------------------------------------------------------------------

## Aufgabe 2   (Rechnen mit der Normalverteilung)
## *********

## (a-i)
pnorm(q = -1.2) ## = 0.1150697

## (a-ii) P(X < 2.2) = P(X <= 2.2)
pnorm(q = 2.2) ## 0.9860966

## (a-iii)
pnorm(q = 2) - pnorm(q = 0) ## = 0.4772499

## (a-iv)
1 - pnorm(q = 0.47) ## = 0.3191775

##
## (b-i)
qnorm(0.7, mean = 1, sd = sqrt(9)) ## K=  2.573202

## (b-ii)
## P(X >= k) = 0.2  gdw 1 - P(X <= k) = 0.2 gdw P(X <= k) = 0.8
qnorm(0.8, mean = 1, sd = 3) ## k= 3.524864

## (b-iii)
## P(-k <= X <= 2) = 0.4 gdw  P(X <= 2) - P(X <= -k) = 0.4
## gdw  P(X <= -k) = P(X <= 2) - 0.4
(h <- pnorm(2, mean = 1, sd = 3) - 0.4) ## = 0.2305587
(k <- -1 * qnorm(h, mean = 1, sd = 3))  ## = 1.211025



## ----------------------------------------------------------------------------

## Aufgabe 3 (Freiwillig)
## *********
##
## (a-i)
1 - pnorm(11.5, mean=12.9, sd=2) ## = 0.7580363

## (a-ii)
pnorm(14.8, mean=12.9, sd=2) - pnorm(11, mean=12.9, sd=2) # = 0.6578877


## (b)
## Gesucht ist P(0.3 - 0.005 <= X <= 0.3 + 0.005) für X~ N(0.302, sigma=0.003),
## also:
pnorm(0.305, mean=0.302, sd=0.003) - pnorm(0.295, mean=0.302, sd=0.003)
## = 0.8315294 (d.h. ca 83%)

## (c)
## Hier ist ein mu gesucht, so dass
## P(X <= 400) = 0.02, wenn  X ~ N(mu, sigma=4)
## Betrachten die standardisierte ZV Z:= (X-mu)/sigma, dann gilt für Z:
## P(Z <= (400-mu)/4) = 0.02
## also ist (400-mu)/4 das 0.02-Quantil der Standardnormalverteilung
(h <- qnorm(0.02,0,1)) ## = -2.053749
## somit h= (400-mu)/4  --> mu = 400 - 4*h
(h1 <- 400 - 4*h)  ## = 408.215

## Probe:
pnorm(400, mean=h1, sd=4)  ## = 0.02

## (c-ii) sigma=2.5
(h2 <- 400 - 2.5*h) ## = 405.1344
## Eine Verringerung der Standardabweichung um 1.5g führt also zu einer
## Verringerung der nötigen durchschnittlichen Füllmenge um etwas mehr als 3g!


## ----------------------------------------------------------------------------

## Aufgabe 4
## *********
## Siehe Unterricht

## (Freiwillig) Analoges mit R:

## Mittelwert aus 500 t_1-verteilten (d.h. Cauchy verteilten) Zufallszahlen
## (ein Beispiel, wo der Zentrale Grenzwertsatz nicht gilt)
n <- 500             ## Länge des Vektors aus dem der Mittelwert berechnet wird
z <- rep(NA, 10000)  ## Vektor, worin die berechneten Mittelwerte abgelegt
                     ## werden bei 10'000 Simulationswiederholungen


for(k in 1:length(z)){
    x <- rt(n,df=1)
    z[k] <- mean(x)
}
hist(z, nclass=100)
## Ganz wenige extreme Mittelwerte, die aber im Histogramm nicht sichtbar sind,
## ausser dass der Wertebereich auf der x-Achse viel, viel grösser ist als für
## die Balken notwenig scheint.



## bimodale "Normalverteilung"
x1 <- rnorm(n,-4,1)
x2 <- rnorm(n, 4,1)
h <- rbinom(n, 1, 0.5)
x <- h*x1 + (1-h)*x2
hist(x) ## bimodale Verteilung

for(k in 1:length(z)){
    x1 <- rnorm(n,-4,1)
    x2 <- rnorm(n, 4,1)
    h <- rbinom(n, 1, 0.5)
    x <- h*x1 + (1-h)*x2
    z[k] <- mean(x)
}
hist(z)
qqnorm(z)
## Obwohl die Ausgangsverteilung bimodal ist, ist die Verteilung der Mittelwert
## aus 500 Einzelwerten praktisch normalverteilt. Der Zentrale Grenzwertsatz
## ist in diesem Beispiel also anwendbar.

## ----------------------------------------------------------------------------

## Aufgabe 5
## *********

load("CAS-DA_ModulA2-HT1_Daten/qqplot.RData")## --> brust, lohn, waerme
source("../CAS-DA_ModulA2_RFn-qqnormSim.R")  ## --> qqnormSim()

par(mfcol=c(2,3))
qqnormSim(brust, rob=TRUE); mtext(text="brust",side=3, line=0.5)
qqnormSim(log(brust), rob=TRUE); mtext(text="log(brust)",side=3, line=0.5)
qqnormSim(lohn, rob=TRUE); mtext(text="lohn",side=3, line=0.5)
qqnormSim(log(lohn), rob=TRUE); mtext(text="log(lohn)",side=3, line=0.5)
qqnormSim(waerme, rob=TRUE); mtext(text="waerme",side=3, line=0.5)
qqnormSim(log(waerme), rob=TRUE); mtext(text="log(waerme)",side=3, line=0.5)

## Bei brust können sowohl die Orginal- wie auch die logarithmierten Daten
## durch eine Normalverteilung beschrieben werden, da in beiden Fällen die
## Daten im normal QQ-Plot innerhalb der stochastischen Fluktuation (grauer
## Bereich) liegen.
## lohn ist klar eine rechtsschiefe Verteilung. Der logarithmierte lohn kann
## jedoch durch eine Normalverteilung beschrieben werden, da die entsprechenden
## Daten im normal QQ-Plot innerhalb der stochastischen Fluktuation liegen.
## Die gleiche Feststellung gilt für die waerme-Daten.

## Weshalb gibt es keine klare Antwort bei den brust-Daten? -
## Da max/min < 1.5 ist, ist der Wertebereich zu klein, um zwischen normal- und
## lognormal verteilten Daten zu unterscheiden.

## ----------------------------------------------------------------------------



