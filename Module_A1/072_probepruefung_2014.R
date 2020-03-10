data("warpbreaks")

# 1.
# Im R-internen Datensatz warpbreaks sind Daten zu einer Untersuchung
# enthalten, in der untersucht werden sollte, ob und wie Brüche in Wollfäden
# (warp breaks) von der Sorte der Wolle (wool/yarn: Garn, Wolle) und der
# aufgesetzten Spannung (tension) in Webstühlen abhängt. Im Einzelnen enthält
# der Datensatz die Variablen:
# breaks  - wieviele Fadenbrüche gibt es pro Webstuhl-Bespannung
# wool    - wurde Wolle A oder Wolle B benutzt
# tension - wurde eine tiefe, mittlere oder hohe Spannung aufgesetzt.
data("warpbreaks")

# 1a.
# Wieviel Zeilen hat der Datensatz?
dim(warpbreaks) # 54 Zeilen
head(warpbreaks)

# Welchen Datentyp haben die Variablen?
str(warpbreaks)
# breaks: quantitativ-diskret (R: numeric)
# wool: qualitativ-nominal (R: factor)
# tension: qualitativ-ordinal (R: ordered factor)


# 1b.
# Erzeugen Sie für die Variablen breaks und tension eine geeignete univariate
# Darstellung und übertragen Sie jeweils den R-Befehl und eine Skizze der
# Grafen auf Ihr Lösungsblatt.
hist(warpbreaks$breaks)

plot(warpbreaks$tension)
barplot(table(warpbreaks$tension))


# 1c.
# Wie würden Sie die Form der Verteilung von breaks beschreiben?

# uni-modal, rechtsschief


# 1d.
# Erzeugen Sie Boxplots für das Merkmal breaks nach tension aufgetrennt,
# so dass Verteilung der Anzahl Brüche durch die nebeneinander angeordneten
# Boxplots gut bzgl. der aufgesetzten Spannung verglichen werden können.

boxplot(breaks ~ tension, data=warpbreaks)

# 1e.
# Erzeugen Sie eine Kreuztabelle zwischen wool und tension.

# Wie oft wurde Wolle A bei hoher Spannung untersucht?

table(warpbreaks$wool, warpbreaks$tension)
# 9 mal

# Visualisieren Sie die oben erzeugte Kreuztabelle durch einen Mosaikplot und
# übertragen Sie den R-Befehl und eine Skizze des Grafen auf ihr Lösungsblatt.
plot(table(warpbreaks$wool, warpbreaks$tension))
mosaicplot(table(warpbreaks$wool, warpbreaks$tension))



# 2a.
# Wenn die Werte einer Stichprobe zwischen 10 und 15 liegen, dann muss auch das
# 20%-Quantil einen Wert zwischen 10 und 15 haben.
# TRUE

# 2b.
# Wendet man eine Logarithmus-Transformation auf ein Merkmal X an, so erhält
# man den Mittelwert des transformierten Merkmals Y, indem man auf den
# Mittelwert des Merkmals X dieselbe Logarithmus-Transformation anwendet.
# FALSE

# 2c.
# Wenn für die kumulierte Verteilungsfunktion F gilt, dass F(13.4)=0.3, dann
# sind mindestens 30% aller Beobachtungen kleiner als 14.
# TRUE

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
# qualitativ, ordinal


# 4.
# Die Daten in der Tabelle zeigen den Kaffeekonsum
# (in der Einheit Pfund pro Person undJahr) in den USA:
#     Jahr  1940 1941 1942 1943 1944 1945 1946 1947
#     Menge 15.5 15.9 13.6 13.1 15.8 16.4 20.6 17.4
#     Jahr  1948 1949 1950 1951 1952 1953 1954 1955
#     Menge 18.4 18.7 16.1 16.6 16.9 16.9 14.7 15.3

# Ordnet man den Kaffeekonsum der Grösse nach, so ergibt sich diese
# Zahlenfolge:
#  13.1, 13.6, 14.7, 15.3, 15.5, 15.8, 15.9, 16.1,
#  16.4, 16.6, 16.9, 16.9, 17.4, 18.4, 18.7, 20.6


# 4a.
# Bestimmen Sie (von Hand oder mit Hilfe von R) den Median, das 25%- Quantil
# und das 75%-Quantil aus den Daten.
cc <- data.frame(
  year = c(1940, 1941, 1942, 1943, 1944, 1945, 1946, 1947,1948, 1949, 1950, 1951, 1952, 1953, 1954, 1955),
  consum = c(15.5, 15.9, 13.6, 13.1, 15.8, 16.4, 20.6, 17.4, 18.4, 18.7, 16.1, 16.6, 16.9, 16.9, 14.7, 15.3)
)
quantile(cc$consum)
summary(cc$consum)

# 4b.
# Unten wurden die Daten auf zwei verschiedene Arten dargestellt. Leider sind
# die Darstellungen nicht ganz korrekt. Finden Sie 4 verschiedene Fehler oder
# Mängel in der Beschriftung und in der Grafik, und korrigieren Sie diese!
# Boxplot:


# Histogramm:
hist(cc$consum)

# 4c.
# Welche der beiden empirischen Verteilungsfunktionen entspricht den
# Kaffeedaten? Warum?

# links: kurvenform und einheit entsprechen den Daten

# Erklären Sie, wie aus einer Darstellung der empirischen Verteilungsfunktion
# grob der Median abgelesen werden kann.

# an der y-Achse bei 0.5 den x-Wert ablesen.

