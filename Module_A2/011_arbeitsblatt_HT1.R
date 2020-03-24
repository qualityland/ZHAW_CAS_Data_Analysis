1. Beispiele zur Poisson-Verteilung
Bearbeiten Sie die Datensaetze (i) und (ii) gemaess den Teilaufgaben (a) – (c).

(i) Radioaktivitaet.
Rutherford und Geiger haben die Emission der α-Teilchen einer radioaktiven
Substanz gemessen. In der Tabelle bezeichnet k die Anzahl der α-Teilchen,
die in der Zeiteinheit (= 7.5s) beobachtet wurden; zk gibt an, wie oft
k = 0, 1, . . . , 14 α-Teilchen beobachtet wurden (total 10’129 Zerfaelle).
(Datensatz ist im File 'alpha.dat' gespeichert.)
k   0   1   2   3   4   5   6   7   8   9   10  11
zk  57  203 383 525 532 408 273 139 49 27   10  4

(ii) Verkehr.
Die Poisson-Verteilung wird auch als Modell bei geringem Verkehr verwendet.
Man geht dann davon aus, dass die Verteilung der Anzahl Autos in einem gegebenen
Zeitintervall oder Gebiet gena ̈hert Poisson verteilt sind, falls der
Verkehrsfluss ungefaehr konstant und der Verkehr gering ist (damit sich die
einzelnen Autos unabhaengig voneinander bewegen koennen).
12 13 14 15+ 0 1 1 0
    Die folgende Tabelle entha ̈lt die Anzahl Rechtsabbieger wa ̈hrend 300 mal 3 Minuten Inter- vallen an einer Kreuzung. (Datensatz ist im File verkehr.dat gespeichert.)
k 0 1 2 3 4 5 6 7 8 9 10 11 12 13+
zk 14 30 36 68 43 43 30 14 10 6 4 1 1 0

a) Zeichnen Sie ein Balkendiagramm.

b) Bestimmen Sie den Mittelwert der Daten.

c) Zeichnen Sie die Werte der Poisson-Verteilung mit Parameter λ in das Stabdiagramm ein, indem Sie als Scha ̈tzer fu ̈r den unbekannten Parameter λ den Mittelwert aus Unteraufgabe (b) verwenden.
R-Anleitung:
a) > alpha <- read.table("alpha.dat", header=TRUE) # Daten einlesen; oder mit Hilfe des Menu ̈s
> plot(alpha$k, alpha$freq, type="h", lwd=6, col="blue", lend=2, xlab="k",
  ylab="H ̈aufigkeit")
b) > n <- sum(alpha$freq)
> mu <- sum(alpha$freq*alpha$k)/n > mu
c) > yModell <- dpois(alpha$k, lambda=mu) * n
> lines(alpha$k, yModell, type="b", lwd=2, col="red")
# Anzahl Daten # Sch ̈atzen von λ # Mittelwert ansehen
# n · p􏰖k bestimmen # n · p􏰖k in Balkendiagramm einzeichnen
# Balkendiagramm; dafu ̈r wichtige Argumente: “ type=”h”, lwd=6”
Bitte wenden!
2. Kennzahlen fu ̈r beliebige Wahrscheinlichkeitsverteilungen
Das “Standard-Zufalls-Experiment” ist der Wu ̈rfel. Wenn wir mit einem “fairen” Wu ̈rfel wu ̈rfeln,
1 dann ist jede Augenzahl k, (k = 1,2,...,6) gleich wahrscheinlich, also P ⟨X = k⟩ = 6.
a) Zeichnen Sie im Fall des Wu ̈rfels die Wahrscheinlichkeiten mit einem Balkendiagramm auf.
b) Berechnen Sie im Fall des Wu ̈rfels den Erwartungswert, die Varianz und die Standardab-
weichung.
c) Berechnen Sie diese drei Kennzahlen fu ̈r den Fall eines Wu ̈rfels mit den Augenzahlen 1, 2, 3, ..., 12.
3. Asbestfasern.
Um die Asbest-Konzentration zu messen, werden unter anderem Asbestfasern auf einem Filter ausgeza ̈hlt. Dieser Teilaspekt wur- de untersucht, indem Asbest in Wasser gelo ̈st und gleichma ̈ssig u ̈ber den Filter gespru ̈ht wurde. Dann wurden 23 Proben von Fil- terstu ̈ckchen mit einem Durchmesser von 3 mm genommen und die sich darauf befindenden Asbestfasern unter dem Elektronenmikro- skop ausgeza ̈hlt. Die Daten, die man erhielt, sind im Daten Frame asbest1 gespeichert.
a) Bestimmen Sie das Maximum, das Minimum, die Spannweite und den Mittelwert der Daten. Der vollsta ̈ndige Datensatz ist im Daten Frame asbest2 gespeichert, da man noch weitere 52
Filterstu ̈ckchen ausgeza ̈hlt hatte.
b) Fu ̈hren Sie mit dem vollsta ̈ndige Datensatz Teilaufgabe (a) nochmals durch. Was fa ̈llt Ihnen auf ?
c) Zeichen Sie fu ̈r beide Datensa ̈tze jeweils ein Stabdiagramm und legen Sie daru ̈ber die Kurve der Poisson-Verteilung, wobei fu ̈r den unbekannten Parameter seine Scha ̈tzung eingesetzt werden soll. Was schliessen Sie aus diesen beiden Grafiken?
R-Anleitung:
a) > # Daten als Vektor einlesen
> asbest1 <- read.table("CAS-DA ModulA2-HT1 Daten/asbest1.dat", header=F)[,1] > min(asbest1) # ebenso mit max() und mean(); (Achtung! asbest1 ist ein Vektor)
 c) > par(mfrow=c(1,2)) # 2 Grafiken pro “Seite” > a1 <- table(asbest1)
4. Segelflugunf ̈alle.
Wenn die to ̈dlichen Segelflugunfa ̈lle in der Schweiz seit 1986 be- trachtet werden, erha ̈lt man einen Durchschnitt von 4.21 Unfa ̈llen pro Jahr. Im Jahr 2008 ereigneten sich 7 to ̈dliche Unfa ̈lle, was zu Massnahmen Anlass gab. Betrachten Sie die obige Situation mit den Methoden der Statistik:
a) Welches Modell wu ̈rden Sie verwenden, um die Anzahl Sege- lunfa ̈lle zu beschreiben? Warum?
# H ̈aufigkeitstabelle erstellen
 b) Ist die Anzahl von 7 to ̈dliche Unfa ̈llen gema ̈ss Ihrem Modell aus der vorhergehenden Tei- laufgabe plausibel? Beantworten Sie die Frage mit Faustregel P1 zum Niveau von 5%. (Freiwillig: Beantworten Sie die Frage exakt - siehe Skript.)