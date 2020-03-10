

# Daten einlesen
load("./Module_A1/data/bikes.RData")

# 1.
# Wir arbeiten mit dem Datensatz bikes.RData. Der Datensatz enthält Ausleihen
# von Velos in Washington D.C., USA, als auch Wetterdaten und besteht aus
# folgenden Variablen:

# instant     - Index, ID
# dteday      - Datum
# season      - Saison (1:spring, 2:summer, 3:fall, 4:winter)
# yr          - Jahr
# mnth        - Monat
# holiday     - Urlaubstag, oder nicht
# weekday     - Wochentag
# workingday  - Werktag
# weathersit  - Wettersituation, 1: klar, 2: wolkig, 3: Regen/Schnee, 4: Unwetter
# temp        - Temperatur in Celsius
# atemp       - gefuehlte Temperatur
# hum         - normalisierte Luftfeuchtigkeit
# windspeed   - Windgeschwindigkeit
# casual      - Gelegenheitsbenutzer
# registered  - Registrierter Nutzer
# cnt         - Gelegenheits- und Registrierte Nutzer

# 1.1
# Wie viele Zeilen und Variablen weist der Datensatz auf?

# 1.2
# Wie viele Variablen vom Typ Factor sind im Datensatz?


# 1.3
# Welchen Skalentyp hat casual?   .....skala
# 


# 1.4
# Wie kann man die Variable casual klassifizieren, als
# quantitativ-diskret, quantitativ-stetig,
# qualitativ-nominal oder qualitativ-ordinal?


# 1.5
# Hat die Variable hr (hour) im Datensatz die richtige Klasse?
# Wenn ja, begründen Sie.
# Wenn nein, überschreiben Sie diese Variable im Datensatz und ändern Sie die
# Klasse der Variable mit as.xxx, wobei xxx dem Klassennamen (z.B. numeric,
# character, factor, integer, ...) entspricht.



# 2.1
# Erzeugen Sie für die Variablen 'casual' und 'weathersit' jeweils eine
# geeignete uni- variate (= “eindimensionale”) Darstellung.
# (R Code + sinnbildhafte Skizze)

# casual


# weathersit


# 2.2
# Wie würden Sie die Verteilungform der Variable cnt beschreiben?


# 2.3
# Erzeugen Sie Boxplots von der Variable 'cnt' nach 'hr' aufgetrennt, so dass
# die Verteilung der Tageszeit (in Stunden) gut verglichen werden kann.
# (Skizze nicht notwendig) Interpretieren Sie Ihr Ergebnis. (ein Satz)
# Zweigipflige Verteilung der Ausleihen ueber den Tag mit Peaks um 8h und 17h.


# 2.4
# Erzeugen Sie einen Scatterplot von 'cnt' in Abhängigkeit von 'hr' vom
# 14. August 2011. Verbinden Sie die Punkte mit Linien
# (Hinweis: ?lines oder ?geom_line). Was erkennen Sie? (ein Satz)


# 2.5
# Berechnen Sie die arithmetischen Mittel von 'cnt' für jeden Monat des
# Jahres 2011 und zeichnen Sie diese zusätzlich in eine Grafik.
# Beschriften Sie die Grafik geeignet. Was erkennen Sie? (ein Satz)


# 2.6
# Wie hoch ist die lineare Korrelation zwischen 'sqrt(cnt)' und 'hum'?
# Interpretieren Sie ihr Ergebnis in einem Satz.



# 3.
# TRUE or FALSE?

# 3.1
# Bei der Variable ’Catholic’ handelt es sich um eine rechtsschiefe Verteilung:
# 


# 3.2
# Je höher der Anteil von Catholic in einer Region, desto kleiner der
# Prozentsatz an Maennern die in der Agrarwirtschaft arbeiten.
# 


# 3.3
# ’Das Merkmal ’Education’ enthält (mind.) einen Ausreisser.’:
# 


# 3.4
# Die Variablen ’Agriculture’ und ’Examination’ haben einen guten positiven
# linearen Zusammenhang.
# 


# 3.5
# Die Skalierung des Merkmals ’Infant.Mortality’ ist quantitativ-diskret.
# 


# 3.6
# In einem Boxplot kann man unter anderem die Standardabweichung ablesen.
# 


# 3.7
# Das 2. Quartil (der Median) umfasst die mittleren 50 Prozent der Beobachtungen.
# 


# 3.8
# Ein Boxplot visualisiert robuste Lage- und Streuungsmasse.
# 
