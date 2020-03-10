data("warpbreaks")
str(warpbreaks)

# 1a.
# Wieviel Zeilen hat der Datensatz?
dim(warpbreaks)
# 54 Zeilen

# Welchen Datentyp haben die Variablen?
sapply(warpbreaks, class)
# breaks: numeric, wool: factor, tension: factor

# 1b.
# Erzeugen Sie für die Variablen breaks und tension eine geeignete univariate
# Darstellung und übertragen Sie jeweils den R-Befehl und eine Skizze der
# Grafen auf Ihr Lösungsblatt.
hist(warpbreaks$breaks)

barplot(table(warpbreaks$tension),
        xlab = "Tension",
        ylab = "Frequency")
# oder:
plot(warpbreaks$tension)

# 1c.
# Wie würden Sie die Form der Verteilung von breaks beschreiben?
# rechtsschief

# 1d.
# Erzeugen Sie Boxplots für das Merkmal breaks nach tension aufgetrennt,
# so dass Verteilung der Anzahl Brüche durch die nebeneinander angeordneten
# Boxplots gut bzgl. der aufgesetzten Spannung verglichen werden können.
boxplot(breaks ~ tension, data = warpbreaks)
# mehr Fadenrisse und breitere Streuung bei niedriger Fadenspannung
# weniger und geringere Streuung bei mittlerer,
# die wenigsten Risse und geringste Streuung bei hoher Fadenspannung.

# 1e.
# Erzeugen Sie eine Kreuztabelle zwischen wool und tension.
table(warpbreaks[, c("wool", "tension")])
# Wolle A wurde 9 mal bei hoher Spannung untersucht.
mosaicplot(table(warpbreaks[, c("wool", "tension")]))
# oder
plot(table(warpbreaks[, c("wool", "tension")]))
# 2a.
# Wenn die Werte einer Stichprobe zwischen 10 und 15 liegen, dann muss auch das
# 20%-Quantil einen Wert zwischen 10 und 15 haben.
# TRUE

# 2b.
# Wendet man eine Logarithmus-Transformation auf ein Merkmal X an, so erhält
# man den Mittelwert des transformierten Merkmals Y, indem man auf den
# Mittelwert des Merkmals X dieselbe Logarithmus-Transformation anwendet.
# TRUE

# 2c.
# Wenn für die kumulierte Verteilungsfunktion F gilt, dass F(13.4)=0.3, dann
# sind mindestens 30% aller Beobachtungen kleiner als 14.
# FALSE

# 2d.
# Wenn die Varianz gleich Null ist, dann bedeutet dies, dass alle Beobachtungen
# in der Stichprobe denselben Wert haben.
# TRUE

# 2e.
# Eine Stichprobe, die aus 5 quantitativen Werten besteht, sollte man nicht mit
# einem Boxplot dargestellen.
# TRUE

# 3.
# Von welchem Datentyp sind die folgenden Merkmale?
# quantitativ, qualitativ, diskret, stetig, nominal, ordinal?

# 3a. Temperatur?
# quantitativ, stetig

# 3b. Anzahl Studenten pro Studiengang?
# quantitativ, diskret

# 3c. Keditwuerdigkeit (low, middle, high)
# qualitativ, ordinal

# 3d. Hautfarbe eines Neugeborenen
# qualitativ, nominal

# 3e.Listenplatz auf Warteliste
# quantitativ, diskret


# 4.
# coffee consumption
cc <- data.frame(
  year=c(as.character(1940:1955)),
  consumption=c(15.5, 15.9, 13.6, 13.1, 15.8, 16.4, 20.6, 17.4,
               18.4, 18.7, 16.1, 16.6, 16.9, 16.9, 14.7, 15.3)
)

# 4a.
# Bestimmen Sie (von Hand oder mit Hilfe von R) den Median, das 25%- Quantil
# und das 75%-Quantil aus den Daten.
summary(cc$consumption) # or:
quantile(cc$consumption, c(.5, .25, .75))

# 4b.
# Unten wurden die Daten auf zwei verschiedene Arten dargestellt. Leider sind
# die Darstellungen nicht ganz korrekt. Finden Sie 4 verschiedene Fehler oder
# Mängel in der Beschriftung und in der Grafik, und korrigieren Sie diese!
# Boxplot:
boxplot(cc$consumption)
# ylab = "Kaffeekonsum in Pfund"
# median: 16.25 (nicht 6.25!)
# 25% Quantil: 15.45 (nicht 12.70!)

# Histogramm:
# xlab = "Kaffeekonsum pro Kopf in Pfund"
# Kategorie 16-18: 6 (nicht 7!)
# Kategorie 20-22: 1 (fehlt!)
hist(cc$consumption)


# 4c.
# Welche der beiden empirischen Verteilungsfunktionen entspricht den
# Kaffeedaten? Warum?
# die linke Darstellung, weil a) die Einheit rechts nicht stimmt und
# die Kurve gleich zu start ansteigt.

# Erklären Sie, wie aus einer Darstellung der empirischen Verteilungsfunktion
# grob der Median abgelesen werden kann.
# der Median ist der Wert unter dem 50% der Daten liegen.
# Hier also wo F(x) = 0.5