##
## CAS Datenanalyse:
## Modul 'Statistische Grundlagen der Datenanalyse'
##
## Lösung HT 6
## ************************************************
##

## Aufgabe 1 (Kostenüberschreitung)
## *********

OE <- c(-16, -404, -397, 113, -791, -165, -118, 9, -651, 383, -480, -77, 221,
        93, 107, -272, 133, 276, 36, -298, -222, -76, -427, -268, -230, 287,
        -285, -344, -394, 161)

## (a)
source("./Module_A2/043-qqnormSim.R")  ## --> qqnormSim()
par(mfrow=c(1,2), las=1)
boxplot(OE, las=1, col="blue", xlab="Kostenüberschreitung [in kFr]",  ylab="")
qqnormSim(OE, SEED=4711); qqline(OE, lty=3)
mtext(side=3, line=0.5, text="Kostenüberschreitung [in kFr]", col="black")
## Datenpunkte streuen recht gut um eine Gerade und liegen innerhalb der
## stochastischen Fluktuation. Es gibt also keine Evidenz gegen die Annahme
## der Normalverteilung.
## Im Weiteren: Da aber die Abweichung auf Kurzschwänzigkeit hindeutet, wäre
## die Abweichung ungefährlich für Schlussfolgerungen aus der schliessenden
## Statistik (= Inferenz).


## (b)
(xq <- mean(OE))  ## -136.5333
(s <- sd(OE) )    #   287.7036

par(mfrow=c(1,1))
hist(OE, probability=TRUE)
curve(dnorm(x, xq, sd=s), col="magenta", lwd=2, add=TRUE)
## In dieser Art von Grafik ist es schwierig festzustellen, ob das Modell
## die Daten gut oder ungenügend beschreibt. Besser ist der Normalplot in (a).

## (c)
t.test(OE, mu=0)
## (c-i) Mit t.test und P-Wert:
t.test(OE, alternative="less", mu=0)
## 	One Sample t-test

## data:  OE
## t = -2.5993, df = 29, p-value = 0.007269
## alternative hypothesis: true mean is less than 0
## 95 percent confidence interval:
##       -Inf -47.28286
## sample estimates:
## mean of x
## -136.5333

## Da der P-Wert von 0.007269 kleiner als das Niveau von 5% (d.h. 0.05) ist,
## wird die Nullhypothese 'Die Kostenüberschreitung ist im Mittel 0'
## auf dem 5% Niveau verworfen und die Alternative gilt, dass die
## Engineering-Abteilung ihre Kosten systematisch zu hoch einschätzt.


## (c-ii) Argumentation mit dem Vertrauensintervall:
## Da die Nullhypothese "Differenz = 0" (d.h. die Kostenüberschreitung ist im
## Mittel 0) nicht im Vertrauensintervall  (-Inf, -47.28286]  liegt, wird die
## Nullhypothese auf dem 5% Niveau verworfen und die Alternative gilt, dass die
## Engineering-Abteilung ihre Kosten systematisch zu hoch einschätzt.


## (c-iii) Von Hand - freiwillig:
length(OE)  # = 30
(xq - 0)/(s/sqrt(length(OE)))      ## = -2.599286
## Da der Teststatistikwert von 2.60 ausserhalb von +/-2 liegt,
## kann die Nullhypothese 'Die Kostenüberschreitung ist im Mittel 0'
## auf dem 5% Niveau verworfen werden.
## Da die Alternative einen negativen Wert vorsieht, erwarten wir eine
## allfällige Abweichung nur links. Das Risiko, dass die Teststatistik in
## jenen Bereich fällt, ist aber nur 2.5%. Also hätten wir die Nullhypothese
## auf dem 2.5% Niveau zugunsten der Alternative "Kosten werden zu hoch
## angesetzt" abgelehnt.




## ----------------------------------------------------------------------------

## Aufgabe 2
## *********

Kad <- c(0.95, 0.85, 0.92, 0.95, 0.93, 0.86, 1.00, 0.92, 0.85, 0.81,
         0.78, 0.93, 0.93, 1.05, 0.93, 1.06, 1.06, 0.96, 0.81, 0.96)

## (a)
qqnormSim(Kad); qqline(Kad)
## Es hat etwas wenig Beobachtungen für einen normalplot, jedoch sind keine
## groben Abweichungen von der Geraden sichtbar. Auch liegen alle Punkte
## innerhalb der stochastischen Fluktuation. Wir haben also keine Evidenz
## gegen die Annahme, dass die Daten normalverteilt sind.



## (b)
t.test(Kad, level=0.95)
## ...
## 95 percent confidence interval:
##  0.8876158 0.9633842
## sample estimates:
## mean of x
##    0.9255

## Das 95%-Vertrauensintervall ist [0.888, 0.963]


## (c)
## Die Frage kann mit dem Konzept des statistischen Hypothesentests beantwortet
## werden. Wir müssen jedoch dazu noch das Signifikanzniveau festlegen. Nehmen
## wir an, es sei 5%.

## Zur konkreten Antwort können wir zwei verschiedene Wege gehen:

## (c-i) Mit dem Hypothesentest selber:
t.test(Kad, mu=1)
## 	One Sample t-test

## data:  Kad
## t = -4.116, df = 19, p-value = 0.0005879
## alternative hypothesis: true mean is not equal to 1
## ...

## Da der P-Wert von 0.00059 kleiner als 0.05 (d.h. 5%) ist, wird die
## Nullhypothese, dass 1 ein geeigneter Wert für die erwartete Kadenz sei,
## auf dem 5% Signifikanzniveau abgelehnt.


## (c-ii) Mit dem Vertrauensintervall aus Teilaufgabe (b)
##        (d.h. Ausnutzen der Dualität)
## Da der hypothetisierte Wert von 1 nicht im 95%-Vertrauensintervall liegt,
## wird die Nullhypothese, dass 1 ein geeigneter Wert für die erwartete Kadenz
## sei, auf dem 5% Signifikanzniveau abgelehnt.

## Zugleich sehen wir auch, welche Werte plausible wären: [0.888, 0.963]
## Also die Kadenz ist kleiner als 1 und könnte mit 0.9 (als runde Zahl)
## "zusammengefasst" werden.


## ----------------------------------------------------------------------------

## Aufgabe 3
## *********
## Beispiel 1
## Gepaarte Stichprobe. Man testet hier H0: "Nach dem Rauchen höchstens so viele
## Blutplättchen wie vorher" gegen H1: "Nach dem Rauchen mehr Blutplättchen als
## vorher" (einseitig). Ein ungepaarter Vergeleich ist prinzipiell denkbar, man
## müsste eine Stichprobe Raucher nehmen, die vor der Blutentnahme eine gewisse
## Zeit nicht geraucht haben (ob sie nach der Entnahme rauchen oder nicht, ist in
## diesem Fall egal!) und eine zweite von Rauchern, die direkt von der Entnahme
## geraucht haben. Ein gepaarter Test ist sicher sinnvoller, da sich die Anzahl
## Blutplättchen von Proband zu Proband natürlich stark unterscheiden kann.

## Beispiel 2
## Ungepaarte Stichprobe. Hier interessieren nur Unterschiede, deshalb würde man
## hier wohl zweiseitig "gleich" gegen "ungleich" testen. Ein gepaartes Design
## würde hier die Gabe von zwei Eisenpräparaten für jede Maus bedeuten. Das ist
## problematisch, da die Aufnahme des zweiten Präparats von dem vorher aufgenommenen
## ersten Präparat beeinflusst werden kann. Auch wird man nicht bei der Messung der
## Konzentration unterscheiden können, aus welchem Präparat welcher Teil des Eisens
## stammt. Man müsste also eventuell mit der Gabe des zweiten Präparats warten, bis
## das erste Präparat komplett abgebaut ist. Das ungepaarte Design ist hier also
## sinnvoll, wobei natürlich sichergestellt werden sollte, dass sich die beiden
## Gruppen nicht systematisch unterscheiden, z.B. durch Randomisierung. Ein andere
## Möglichkeit des gepaarten Vergleichs ergibt sich, wenn man jeweils "Matchings"
## zwischen einer Maus aus der einen und einer aus der anderen Gruppe finden kann
## (gleiches Geschlecht, Alter, ähnliches Gewicht etc.) Dann kann man jeweils die
## zwei Präparate für ein ähnliche Paar von Mäusen vergleichen. Bei Studien mit
## menschlichen Patienten wird dies manchmal so gemacht.

## Beispiel 3
## Gepaarter Test. Da es um Unterschiede zwischen zwei neu entwickelten Typen und
## nicht um den Vergleich eines neuen Typs mit einem Standard geht, ist wohl ein
## zweiseitiger Test "gleich" gegen "ungleich" angezeigt. Ein ungepaarter Test ist
## denkbar, aber nicht unbedingt weniger aufwändig und sicher schlechter.

## Beispiel 4
## Ungepaarter Test, hier wird einseitig H0: "mittlere Gewichtszunahme bei
## Fütterungsart 2 höchstens so gross wie bei Fütterungsart 1" gegen H1:
## "mittlere Gewichtszunahme bei Fütterungsart 2 grösser als bei Fütterungsart 1".
## Ansonsten ist die Situation wie in Beispiel 2.

## Beispiel 5
## Gepaarter Test. Hier werden nicht zwei Behandlungen derselben
## Untersuchungseinheit, sondern an zwei möglichst ähnlichen
## Untersuchungseinheiten untersucht. Getestet wird einseitig H0:
## "Fremdbefruchtete Pflanzen nicht grösser als selbstbefruchtete" gegen
## H1: "Fremdbefruchtete Pflanzen grösser als selbstbefruchtete".
## Ein ungepaartes Design wäre natürlich möglich und wäre einfacher
## umzusetzen, der Vergleich möglichst ähnlicher Pflanzen ist aber sicher
## sinnvoller.

## ----------------------------------------------------------------------------

## Aufgabe 4
## *********
P <- data.frame(Proband=1:14,
                Auto1=c(37.0, 25.8, 16.2, 24.2, 22.0, 33.4, 23.8, 58.2,
                         33.6, 24.4, 23.4, 21.2, 36.2, 29.8),
                Auto2=c(17.8, 20.2, 16.8, 41.4, 21.4, 38.4, 16.8, 32.2,
                         27.8, 23.2, 29.6, 20.6, 32.2, 53.8))
P$Diff <- P$Auto1 - P$Auto2
P
summary(P)

##
## (a)
par(mfrow=c(2,1))
hist(P$Auto1)
hist(P$Auto2)
## sieht keine Unterschiede zwischen diesen beiden Histogrammen

boxplot(P[,c("Auto1", "Auto2")])
## auch zwischen den beiden Boxplots sieht man kaum einen Unterschied

par(mfrow=c(2,1))
hist(P$Diff)
boxplot(P$Diff, horizontal=TRUE)
## Bei beiden Grafiken streuen die Differenzen um 0;
## d.h. es sind keine Unterschiede auszumachen.

##
## (b)
par(mfrow=c(1,1))
qqnormSim(P$Diff); qqline(P$Diff, lty=2)
## Die Abweichung der Punkte von der Geraden deuten in Richtung
## Langschwänzigkeit.
## Da jedoch alle Punkte innerhalb der stochastischen Fluktuation liegen,
## haben wir keine Evidenz gegen die Annahme der Normalverteilung.

##
## (c)
t.test(P$Diff, alternative="two.sided", mu=0, conf.level=0.95)
## oder
t.test(x=P$Auto1, y=P$Auto2, alternative="two.sided", mu=0, paired=TRUE,
       conf.level=0.95)
## 	Paired t-test

## data:  P$Auto1 and P$Auto2
## t = 0.3582, df = 13, p-value = 0.726
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:   -6.109775  8.538347
## sample estimates:
## mean of the differences:   1.214286

## Da der P-Wert von 0.726 grösser als das Niveau von 0.05 ist,
## kann die Nullhypothese nicht verworfen werden.
## Wir können also keine unterschiedliche Parkierungszeit feststellen


## ----------------------------------------------------------------------------
