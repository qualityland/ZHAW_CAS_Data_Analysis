##
## CAS Datenanalyse:
## Modul 'Statistische Grundlagen der Datenanalyse'
##
## Loesung HT 3 und 4
## *****************


## Aufgabe 1
## *********

## (a) Zeichnen Sie die Wahrscheinlichkeitsverteilung fuer B⟨π = 0.5, m = 5⟩ und
##     B ⟨π = 0.5, m = 15⟩ auf.

## Grafische Darstellung der Wahrscheinlichkeiten:
par(mfrow=c(1,2))
plot(0:5, dbinom(0:5,size=5,prob=0.5), type="h", xlab="k", ylab="Wahrscheinlichkeit",
     las=1, lwd=6, col="blue", lend=2, main="B(m=5, pi=0.5)", ylim=c(0,0.35))
plot(0:15, dbinom(0:15,size=15,prob=0.5), type="h", xlab="k", ylab="Wahrscheinlichkeit",
     las=1, lwd=6, col="blue", lend=2, main="B(m=15, pi=0.5)", ylim=c(0,0.35))



## (b)
## Wie gross ist die Wahrscheinlichkeit, bei 5 Wuerfen genau 1x Kopf zu werfen?
dbinom(1,size=5, prob=0.5)  # = 0.15625

## (c)
## Wie gross ist die Wahrscheinlichkeit, bei 15 Wuerfen genau 3x Kopf zu werfen?
dbinom(3,size=15, prob=0.5) # = 0.0138855


## (d)
## Der eine Spieler hat bei 5 Wuerfen einmal Kopf geworfen. Der andere Spieler
## hat bei 15 Wuerfen 3 mal Kopf geworfen. Beide haben also "20% Erfolg"
## gehabt. Sind die Wahrschein- lichkeiten, dass jeweils diese Ereignisse
## (oder Situationen) eintreffen, deshalb auch gleich gross?

## Wie die oben ausgefuehrte Rechnung zeigt, sind diese beiden
## Wahrscheinlichkeiten ungleich, obwohl jedes Mal ein "Erfolg
## von 20%" erreicht wurde.
## Die Aussage, "Erfolg von 20%" bezieht sich hier auf unterschiedliche
## Situationen, die so nicht vergleichbar sind: Bei m=15 gibt es viel
## weniger M?glichkeiten, drei Erfolge zu haben, als bei m=5 im Vergleich
## zu allen M?glichkeiten.


## ----------------------------------------------------------------------------

## Aufgabe 2
## *********
## Verteilung visualisieren:
par(mfrow=c(1,1))
plot(0:85, dbinom(0:85,size=85,prob=0.86), type="h", lwd=6, col="blue", lend=1,
     xlab="k", ylab="Wahrscheinlichkeit", main="B(m=85, pi=0.86)",  las=1)

## Wahrscheinlichkeiten f?r k=83, 84 und 85 berechnen:
dbinom(83:85,size=85,prob=0.86)

## Wahrscheinlichkeiten X <= 82 berechnen:
pbinom(82, size=85, prob=0.86) ## = 0.9997038
## Mit einer Wahrscheinlichkeit von 99.97% werden alle Passagiere
## mitgenommen,
## oder
1- pbinom(82, size=85, prob=0.86) ## = 0.0002961861
## d.h. nur in 0.03% der Faelle erscheinen mehr Passagiere als Plaetze
## vorhanden sind.

## Fuer die Praxis:
## Dass die Passagiere unabhaengig voneinander erscheinen, ist eher eine
## unrealistische Annahme (Familien, Anschlussflug nicht gewaehrt). Deshalb
## koennte unser Resultat auch voellig weltfremd sein. Man muss sie deshalb
## empirisch ueberpruefen


## ----------------------------------------------------------------------------

## Aufgabe 3
## *********
## Sei X die Anzahl erfolgreich abgeschlossener Service-Vertraege und
## m die Anzahl verkaufter Waermepumpen

## (a)
## Jeder Abschluss eines Service-Vertrags ist unabhaengig und wird mit
## Wahrscheinlichkeit pi abgeschlossen. Somit ist X ~ B(m, pi)

## (b)
## Die Erfolgswahrscheinlichkeit wird durch pi=X/m geschaetzt; also hier durch
15/50 ## =0.3

## (c)
## Ist die beobachtete Erfolgswahrscheinlichkeit der Filiale AA plausibel, wenn
## man von einer Erfolgswahrscheinlichkeit von 50% ausgeht?
## Testen Sie dazu aufdem 5% Niveau.

## H0: pi=0.5
## (c-i) Mit binom.test()
binom.test(x=15, n=50, p=1/2, conf.level=0.95)
## 	Exact binomial test
## data:  15 and 50
## number of successes = 15, number of trials = 50, p-value = 0.0066
## ... (cut)

## Da der P-Wert von 0.0066 kleiner als das Niveau von 5% ist, wird die
## Nullhypothese auf dem 5% Niveau verworfen.

## (c-ii) Faustregel B1': m/2 +/- sqrt(m) (Skript 3.4.i)
## plausibler Bereich (=Annahmebereich) auf 5% Niveau:
     50/2 + c(-1,1)*sqrt(50) # = [17.92893 32.07107] --> [18, 32]


## (d)
## mit binom.test
binom.test(x=15, n=50, conf.level=0.95)
## 	Exact binomial test
## data:  15 and 50
## number of successes = 15, number of trials = 50, p-value = 0.0066
## alternative hypothesis: true probability of success is not equal to 0.5
## 95 percent confidence interval:
##  0.1786178 0.4460823
## sample estimates:
## probability of success
##                    0.3

## Das 95%-Vertrauensintervall ist [0.179, 0.446]

## Freiwillig:
## - 95%-Vertrauensintervall nach Faustregel B2 (Skript 3.4.s):
15/50 + c(-1, 1) * 2 * sqrt(15/50 * (1 - 15/50) / 50)  ## = [0.1703852, 0.4296148]


## (e) siehe z.B. 3.4.p


## ----------------------------------------------------------------------

## Aufgabe 4
## *********

binom.test(x=35, n=35+37, p=0.5)
## 	Exact binomial test
## data:  35 and 35 + 37
## number of successes = 35, number of trials = 72, p-value = 0.9063
## alternative hypothesis: true probability of success is not equal to 0.5
## 95 percent confidence interval:
##  0.3665003 0.6069000
## sample estimates:
## probability of success
##              0.4861111

## Da der P-Wert von 0.9063 im Annahmebereich [0.05,1] liegt, gibt es keine
## Evidenz gegen die Nullhypothese "die mittlere Anzahl Todesf?lle ist unver?ndert".


## X2|(X1+X2=72) ~ B(0.5,72)
## plausibler Bereich auf dem 5% Niveau
## - mit Faustregel B1' (Folie 7):
72/2 + c(-1,1)*sqrt(72) # = 27.51472 44.48528 --> [28, 44]
## Da der Wert 35 im Annahmebereich (=95%-Streubereich) liegt, gibt es keine
## Evidenz gegen die Nullhypothese "die mittlere Anzahl Todesf?lle ist unver?ndert".



## ----------------------------------------------------------------------------

## Aufgabe 5
## *********
## Unser "Kieselsteine"-Experiment von HT2:
## die letzten 3 Beobachtungen wurden am HT3 gezogen
KS <- c(0,2,2,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,7,7,7,4,4,4)


table(KS)
## KS
## 0 2 3 4 5 6 7
## 1 2 3 9 6 1 3



## Damit alle moeglichen Werte ausgezaehlt werden:
(h <- table(factor(KS, levels=paste(0:10))))
(KS.ct <- as.data.frame(h))
names(KS.ct)[1] <- "k"   ## Name der ersten Spalte aendern auf "k"

## Schaetzung:
mean(KS)  ## = 4.24
## geschaetzte Erfolgswahrscheinlichkeit (bei m=10):
(KS.pi <- mean(KS)/10) ## = 0.424

KS.ct$Modell <- round(dbinom(0:10, size=10, prob=KS.pi)*length(KS),1)
KS.ct
##     k Freq Modell
## 1   0    1    0.1
## 2   1    0    0.7
## 3   2    2    2.5
## 4   3    3    4.8
## 5   4    9    6.2
## 6   5    6    5.5
## 7   6    1    3.4
## 8   7    3    1.4
## 9   8    0    0.4
## 10  9    0    0.1
## 11 10    0    0.0


## Die erwarteten Klassenhaeufigkeiten sind bei
## k=0,1, .... 9 und 10  zu klein (Faustregel AT1).

## Mit R-Funktion 'chisq-bin' von Raul Eyzaguirre
source("Module_A2/031_chisq-bin.R")  ## --> chisq.bin
chisq.bin(x=0:10, f=KS.ct$Freq, n=10, p=mean(KS)/10, estimated=TRUE)
## $Test
## [1] "Chi-square goodness of fit test for a binomial(10, 0.424) distribution"
##
## $Contribution_table
##    x obs.f     exp.f chisq.cont
## 1  0     1 0.1004997 8.05077640
## 2  1     0 0.7397896 0.73978959
## 3  2     2 2.4505530 0.08283764
## 4  3     3 4.8103448 0.68131256
## 5  4     9 6.1966595 1.26821850
## 6  5     6 5.4737159 0.05060091
## 7  6     1 3.3577192 1.65554043
## 8 7+     3 1.8707183 0.68170452
##
## $Chi_square_test
## [1] 13.21078
##
## $Degrees_of_freedom
## [1] 6
##
## $p_value
## [1] 0.03980821
##
## Warning messages:
## 1: In chisq.bin(x = 0:10, f = KS.ct$Freq, n = 10, p = mean(KS)/10,  :
##   6 Expected frequencies less than 5.
## 2: In chisq.bin(x = 0:10, f = KS.ct$Freq, n = 10, p = mean(KS)/10,  :
##   2 Expected frequencies less than 1.



## Randklassen wurden zusammengefasst, da sie zu kleine erwartete H?ufigkeiten
## hatten.

## Da der P-Wert von 0.0398 kleiner als 5% ist, haben wir Evidenz gegen
## die Nullhypothese, dass diese Daten durch die Bin(pi=0.5, m=10)-Vertreilung
## adaequat beschrieben werden koennen.


## Freiwillig: Mit Faustregel Chi2 (Skript 2.6.e und 3.5.g):
## 6 Freiheitsgrade
(6-1) + 2 * sqrt(2 * (6 - 1))  ## = 11.32456
## Da 13.21078 > 11.3, haben wir gem?ss Faustregel auf dem 2.5% Niveau Evidenz
## gegen die Nullhypothese.
##
## Wir koennen also NICHT davon ausgehen, dass das Binomial-Modell fuer unsere
## Kieselstein-Daten plausibel ist.


## ----------------------------------------------------------------------------

## Aufgabe 6
## *********

## (a) Dateneinlesen und aufbereiten:
## CAS Datenanalyse 15.4:
## coinD <- c(3,7,4,9,4,5,3,5,7,5,3,6,4,4,2,5,4,5,4,2,4,4,7,6,9,4,5,6,7)
## CAS Datenanalyse 16.5:
## coinD <- c(3,5,8,5,4,6,4,4,4,4,4,4,5,6,6,5,4,3,6,5,3,5,4,5,8,5,4)
## CAS Datenanalyse 17.6:
## coinD <- c(8,6,5,6,5,7,4,3,4,4,4,3,5,4,5,7,6,8,6,4,7,6,4,8)
## CAS Datenanalyse 19.9 (mit W?rfel: # <= 3):
coinD <- rep(0:10, c(0,1,1,4,7,5,5,4,0,0,0))


length(coinD)  ## = 27

table(coinD)

## Damit alle moeglichen Werte ausgezaehlt werden:
(h <- table(factor(coinD, levels=paste(0:10))))
## 0  1  2  3  4  5  6  7  8  9 10
## 0  1  1  4  7  5  5  4  0  0  0

(coin <- as.data.frame(h))
names(coin)[1] <- "k"   ## Name der ersten Spalte ?ndern auf "k"
coin$k <- as.integer(as.character(coin$k)) ## Variablentyp auf Integer ?ndern

## (b) Daten und Modell gegen?berstellen
coin$Modell <- dbinom(0:10, size=10, prob=0.5) * sum(coin$Freq)
coin
## Resultat vom CAS Datenanalyse 19.9
##     k Freq    Modell
## 1   0    0 0.02636719
## 2   1    1 0.26367188
## 3   2    1 1.18652344
## 4   3    4 3.16406250
## 5   4    7 5.53710937
## 6   5    5 6.64453125
## 7   6    5 5.53710937
## 8   7    4 3.16406250
## 9   8    0 1.18652344
## 10  9    0 0.26367188
## 11 10    0 0.02636719

## Die erwarteten Klassenh?ufigkeiten sind bei
## k=0,1,9 und 10  zu klein (Faustregel AT1).

## (c) Anpassungstest durchf?hren mit chisq.bin()
## - File "CAS-DA_ModulA2-HT3-4_chisq-bin.R" von Moodle,
##   Unterordner R-Skript herunterladen und in den Unterordner ablegen,
##   wo Sie die ?bungen machen.
## - File einlesen mit
source("Module_A2/031_chisq-bin.R")  ## --> chisq.bin
## - Diese Funktion ist meine Modifikation zu 'chisq-bin' von Raul Eyzaguirre

## - Anpassungstest durchf?hren; schwach besetzte Zellen werden automatisch
##   zusammengelegt:
chisq.bin(x=0:10, f=coin$Freq, n=10, p=0.5, estimated=FALSE)
## $Test
## [1] "Chi-square goodness of fit test for a binomial(10, 0.5) distribution"
##
## $Contribution_table
##     x obs.f     exp.f chisq.cont
## 1 0-1     1 0.2900391 1.73785051
## 2   2     1 1.1865234 0.02932179
## 3   3     4 3.1640625 0.22085262
## 4   4     7 5.5371094 0.38649209
## 5   5     5 6.6445313 0.40702390
## 6   6     5 5.5371094 0.05210056
## 7  7+     4 4.6406250 0.08843645
##
## $Chi_square_test
## [1] 2.922078
##
## $Degrees_of_freedom
## [1] 6
##
## $p_value
## [1] 0.8185614
##
## Warning messages:
## 1: In chisq.bin(x = 0:10, f = coin$Freq, n = 10, p = 0.5, estimated = FALSE) :
##   4 Expected frequencies less than 5.
## 2: In chisq.bin(x = 0:10, f = coin$Freq, n = 10, p = 0.5, estimated = FALSE) :
##   1 Expected frequency less than 1.

## Da der P-Wert von 0.819 gr?sser als 5% ist, haben wir keine Evidenz gegen
## die Nullhypothese, dass diese Daten durch die Bin(pi=0.5, m=10)-Vertreilung
## ad?quat beschrieben werden k?nnen.



## Freiwillig: Mit Faustregel Chi2 (Skript 2.6.e und 3.5.g):
## Da 4.77 < 5+2*sqrt(2*5)=11.32456, ist das Binomial-Modell
##                  auf dem 2.5% Niveau plausible !!!


## ----------------------------------------------------------------------------

## Aufgabe 7
## *********
## (a)
## P(K) = 0.0001
## Fragestellung: Gesucht ist P(K|S)

## (b)
## P(S|K) = 0.99
## P(K?)  = 1 - P(K) = 0.9999; wobei K? das Ereignis ist, dass
##                             man die Krankheit nicht hat
## Die Wahrscheinlichkeit, dass der Test korrekt anzeigt bei einer
## nicht kranken Person ist 0.05.  Folglich ist die Wahrscheinlichkeit
## eines positiven Test bei einer nicht kranken Person 0.05, also
## P(S|K?)= 1 - 0.95 = 0.05

## (c)
## Bayes: P(K|S) = {P(S|K)*(P(K)}/P(S)
##               = {P(S|K)*(P(K)}/{P(S|K)P(K) + P(S|K?)*P(K?) }
##               = 0.99*0.0001/( 0.99*0.0001 + 0.05*0.9999)
##               = 0.001976
##
## Die Wahrscheinlichkeit, dass eine Person die Krankheit hat, wenn
## der Test positive ausgefallen ist, betr?gt nur 0.002 (0.2%).
## Das ist sehr ?berraschend! Obwohl also ein Test sehr wirksam ist,
## im Sinne dass P(S|k) gross und P(S|K?) klein ist, heisst das noch
## lange nicht, dass eine Person die Krankheit hat, wenn der Test
## positive ausgefallen ist.

## (d)
## Wird im Kurs diskutiert ....


## ----------------------------------------------------------------------------


## Aufgabe 8
## *********

(kt <- rbind(c(20,28,23,14,12), c(14,34,21,14,12), c(4,12,10,20,53)))
dimnames(kt) <- list(paste("Region",1:3, sep=""), paste("GW",1:5,sep=""))
kt

## grafisch Darstellen:
mosaicplot(kt)

## Test auf Homogenit?t:
chisq.test(kt)
## 	Pearson's Chi-squared test
##
## data:  kt
## X-squared = 70.6416, df = 8, p-value = 3.662e-12

## Da der P-Wert von  3.662e-12 kleiner als das Niveau von 0.05 (=5%)
## ist, wird die Nullhypothese "die Verteilung der Geruchswahrnehmung ist in
## allen Regionen gleich" auf dem 5% Niveau verworfen werden.

## Wie der Mosaic-Plot zeigt, muss der Unterschied vor allem daher kommen,
## dass sich die Verteilung in Region drei von denjenigen in den Regionen
## eins und zwei unterscheidet.


## Freiwillig:
## Mit Faustregel (geht nur auf dem 2.5% Niveau):
## Da die Teststatistik mit 70.64 gr?sser als 8 + 2*sqrt(2*8) = 16 ist,
## wird die Nullhypothese, dass die Geruchswahrnehmung und die Region
## unabh?ngig ist (oder dass die Verteilung der Geruchswahrnehmung in
## den drei Regionen die gleiche ist), auf dem 2.5% Niveau verworfen.
## Damit bestehen Unterschiede.


## ----------------------------------------------------------------------------

## Aufgabe 9
## *********

## (a)
## Die relativen H?ufigkeiten der Zugelassenen sind bei den Frauen sowohl im
## Departement A als auch im Departement F h?her:
## Dep. A: 511/825=62% der M?nner werden zugelassen gegen?ber 89/108=82% der
##         Frauen
## Dep. B: 22/373=5.9% der M?nner werden zugelassen gegen?ber 24/341=7% der
##         Frauen

## Z?hlt man aber die Departemente A und F zusammen, so ergeben sich bei den
## M?nnern pl?tzlich 45% Zugelassene gegen?ber nur 25% der Frauen. Dies liegt
## daran, dass sich beim Departement A, wo relativ viele Bewerber zugelassen
## werden, nur wenige Frauen bewerben. F?r die schwer zu bestehende
## Zulassungspr?fung f?r Departement F melden sich dagegen ungef?hr gleich
## viele Frauen wie M?nner, mehr als dreimal mehr als in Departement A. Also
## macht bei den Frauen Departement F einen viel gr?sseren Anteil aus und
## damit hat auch die hohe Durchfallquote einen viel gr?sseren Einfluss auf
## die Gesamtquote. Man nennt dieses Ph?nomen das Simpson'sche Paradox.

## (b)
##   \ Gesamtbewerbende Dep A & F  | davon zugelassen
## M                   1198        |     533
## F                    449        |     113
## total               1647        |     646

## --> P(F) =  449/1647 = 0.273
##     P(M) = 1198/1647 = 0.727
## Sei Z = zugelassen
## P(F und Z) = 113/1647 = 0.069
## P(M und Z) = 533/1647 = 0.324

## (c)
## P(Z|F) = P(F und Z)/P(F) = 0.069/0.273 = 0.253
## P(Z|M) = P(M und Z)/P(M) = 0.324/0.727 = 0.446
## Diese Zahlen entsprechen den in (a) beobachteten H?ufigkeiten.

## (d)
## Departement A                       Departement F
## P(F) = 108/933 = 0.116              P(F) = 341/714 = 0.478
## P(M) = 825/933 = 0.884              P(M) = 373/714 = 0.522
## P(F und Z) = 89/933 = 0.095         P(F und Z) = 24/714
## P(M und Z) = 511/933 = 0.548        P(M und Z) = 22/714
## P(Z|F) = 0.095/0.116 = 0.819        P(Z|F) = 0.070
## P(Z|M) = 0.548/0.884 = 0.619        P(Z|M) = 0.059


## (e) (Freiwillig)
kt1 <- cbind(c(511,353,120,138,53,22),
             c(825,560,325,417,191,373) - c(511,353,120,138,53,22))
kt2 <- cbind(c(89,17,202,131,94,24),
             c(108,25,593,375,393,341) - c(89,17,202,131,94,24))
kt <- array(dim=c(6,2,2),
            dimnames=list(Departement=LETTERS[1:6],
                          c("Zugelassen", "Abgelehnt"), c("M", "F")))
kt[,,1] <- kt1
kt[,,2] <- kt2
kt

mosaicplot(kt[,,1], color=TRUE, main="M?nner")
mosaicplot(kt[,,2], color=TRUE, "Frauen")
mosaicplot(kt, color=TRUE, main="alle")
## Ganz "?berraschend" stellt man fest, dass, obwohl sich die Zulassungsraten
## in den einzelnen Departementen stark unterscheiden, die Zulassungsraten f?r
## M?nner und Frauen in den einzelnen Departementen jedoch ungef?hr gleich ist.
## Auch sieht man, dass sich in den einzelnen Departementen das Verh?ltnis Mann
## zu Frau bei den Bewerbenden stark schwankt.


## ----------------------------------------------------------------------------

## Aufgabe 10 (Freiwillig)
## **********
## Die Zufallsvariable X repr?sentiere die Anzahl ?berlebende Fische nach
## 24 Stunden. Dann ist X ~ B(m=4, pi=0.75)

## (a)
## P(alle ?berleben) = P(X=4) = 0.75^4 = 0.3164062 oder
dbinom(4, size=4, prob=0.75)  ## = 0.3164063

## (b)
## P(h?chsten 2 ?berleben) = P(X <= 2)=
sum(dbinom(0:2, size=4, prob=0.75)) ## = 0.2617188  oder
pbinom(2, size=4, prob=0.75) ## = 0.2617188

## Zusatz:
## Erwartungswert = m*pi
4*0.75  # = 3
## Varianz = m*pi*(1-pi)
4*0.75*(1-0.75)  # = 0.75

## (c)
## Sei Xm die Anzahl ?berlebender Fische, falls es im Wassertank m Fische gibt.
## W?hle das kleinste m, sodass P(Xm = m) <= 0.05.
## Ausprobieren:
dbinom(5, size=5, prob=0.75)  ## = 0.2373047
dbinom(6, size=6, prob=0.75)  ## = 0.1779785
dbinom(7, size=7, prob=0.75)  ## = 0.1334839
dbinom(8, size=8, prob=0.75)  ## = 0.1001129
dbinom(9, size=9, prob=0.75)  ## = 0.07508469
## ... etwas m?hsam
## besser
cbind(9:15, dbinom(9:15, size=9:15, prob=0.75))
## [1,]    9 0.07508469
## [2,]   10 0.05631351
## [3,]   11 0.04223514
## [4,]   12 0.03167635
## [5,]   13 0.02375726
## [6,]   14 0.01781795
## [7,]   15 0.01336346

## L?sung: brauchen mindestens 11 Fische

## Eleganterer L?sungsweg:
## Da P(Xm = m)=0.75^m ist,
## m?ssen wir nur die Gleichung 0.75^m <= 0.05 nach m aufl?sen.
## Auf beiden Seiten logarithmieren: m*log(0.75) = log(0.05)
## also m= log(0.05)/log(0.75)
log(0.05)/log(0.75) # = 10.41334
## Also braucht es mindestens 11 Fische.


## ----------------------------------------------------------------------------

## Aufgabe 11 (Freiwillig)
## **********
## (a)
## Jede einzelne Probe ist unabh?ngig von den anderen Proben und mit
## 98% Wahrscheinlichkeit sauber. Also gilt
## P(alle Proben sauber)
##          = P(1. Probe sauber)*P(2. Probe sauber)* ... *P(10. Probe sauber)
##          = 0.98^10 =
0.98^10   # = 0.8170728

## (b)
## Beachten Sie, dass die Zufallsvariable X nur zwei Werte annehmen kann:
## X=1 oder X=11,
## denn
## - falls die Proben sauber sind, ist man nach einer Untersuchung fertig: X=1
## - sonst muss man jede Probe nochmals einzeln untersuchen (man darf nicht
##   stoppen, wenn man eine verunreinigte Probe gefunden hat, denn es k?nnte
##   ja noch eine zweite unsaubere Probe geben!), also X=11

## P(X=1) = P(alle Proben sind sauber) = 0.8171 (siehe (a))
## p(X=11) = 1 - P(X=1) = 1 - 0.8170728 = 0.1829272

## (c)
## Durchschnittliche Anzahl Analysen pro Sammelprobe:
##  E(X) = 1*P(X=1) + 11*P(X=11) =
1* 0.8171 + 11*0.1829  ## = 2.829
## Wir sparen also durchschnittlich 10 - 2.829 = 7.171 Proben ein.

## ----------------------------------------------------------------------------


