

# Daten einlesen
load("./Module_A1/data/bikes.RData")


# 1.1
dim(bikes)         # 8645 Beobachtungen, 17 Variablen
nrow(bikes)        # 8645 Beobachtungen
ncol(bikes)        # 17 Variablen


# 1.2
str(bikes)         # 7 FaKtoren (season, yr, mnth, hr, holiday, weekday, weathersit)


# 1.3              # Verhaeltnis-Skala (Anzahl Gelegenheitsbenutzer)
str(bikes$casual)
head(bikes$casual)


# 1.4              # quantitativ-diskret (integers)


# 1.5
# Richtige Klasse?
levels(bikes$hr)

# CAVE: as. numeric() gibt eine Vector der Levels des Faktors und NICHT die
#       Original-Werte.
bikes$hr[1:30]
as.integer(bikes$hr[1:30])

# Daher muss die Variable folgendermassen ueberschrieben werden:
bikes$hr <- as.numeric(as.character(bikes$hr))

# Ueberpruefung
table(bikes$hr)


# 2.1
# Erzeugen Sie für die Variablen casual (cnt) und weathersit jeweils eine
# geeignete uni- variate (= “eindimensionale”) Darstellung.
# (R Code + sinnbildhafte Skizze)

# Anzahl Gelegenheitsbenutzer (integer)
hist(bikes$casual,
     xlab = "Anzahl Gelegenheitsbenutzer",
     ylab = "Frequenz")

# Wetter Situation (Factor mit 4 Levels)
plot(bikes$weathersit)
barplot(table(bikes$weathersit),
        xlab = "Wetter Situation",
        ylab = "Frequenz")


# 2.2
# Wie würden Sie die Verteilungform der Variable cnt beschreiben?
hist(bikes$cnt)
# rechtsschief


# 2.3
# Erzeugen Sie Boxplots von der Variable cnt nach hr aufgetrennt, so dass
# die Verteilung der Tageszeit (in Stunden) gut verglichen werden kann.
# (Skizze nicht notwendig) Interpretieren Sie Ihr Ergebnis. (ein Satz)
boxplot(cnt ~ hr,
        data = bikes,
        xlab = "Tageszeit",
        ylab = "Anzahl Ausleihen")
# Zweigipflige Verteilung der Ausleihen ueber den Tag mit Peaks um 8h und 17h.


# 2.4
# Erzeugen Sie einen Scatterplot von cnt in Abhängigkeit von hr vom
# 14. August 2011. Verbinden Sie die Punkte mit Linien
# (Hinweis: ?lines oder ?geom_line). Was erkennen Sie? (ein Satz)
x <- bikes[bikes$dteday == "2011-08-14",]
plot(cnt ~ hr,
     data = x,
     type = "b")    # b: both (points and lines)

# Eher eingipfliger Verlauf mit Ausleih-Anstieg um 9h, einem Maximum um 14h,
# einer kleinen Delle um 16h und starkem Abfall nach 19h.


# 2.5
# Berechnen Sie die arithmetischen Mittel von cnt für jeden Monat des
# Jahres 2011 und zeichnen Sie diese zusätzlich in eine Grafik.
# Beschriften Sie die Grafik geeignet. Was erkennen Sie? (ein Satz)
library(dplyr)
m <- bikes %>% 
  select(mnth, cnt) %>% 
  group_by(mnth) %>% 
  summarise(mean_cnt=mean(cnt))

plot(mean_cnt ~ mnth, data = m)
# Ausleihen sind Jahreszeitabhaengig. Steigen stark ab April, Maximum im Juni,
# dann Abfall bis zum Jahresende.


# 2.6
# Wie hoch ist die lineare Korrelation zwischen sqrt(cnt) und hum?
# Interpretieren Sie ihr Ergebnis in einem Satz.
cor(sqrt(bikes$cnt), bikes$hum)
plot(cnt ~ hum, data = bikes, type = "h")


# 3.1
# Bei der Variable ’Catholic’ handelt es sich um eine rechtsschiefe Verteilung:
# FALSE, zweigipflig


# 3.2
# Je höher der Anteil von Catholic in einer Region, desto kleiner der
# Prozentsatz an Maennern die in der Agrarwirtschaft arbeiten.
#  FALSE, auch zweigipflig


# 3.3
# ’Das Merkmal ’Education’ enthält (mind.) einen Ausreisser.’:
# TRUE


# 3.4
# Die Variablen ’Agriculture’ und ’Examination’ haben einen guten positiven
# linearen Zusammenhang.
# TRUE


# 3.5
# Die Skalierung des Merkmals ’Infant.Mortality’ ist quantitativ-diskret.
# FALSE, quantitativ-stetig, da per in Promille


# 3.6
# In einem Boxplot kann man unter anderem die Standardabweichung ablesen.
# FALSE


# 3.7
# Das 2. Quartil (der Median) umfasst die mittleren 50 Prozent der Beobachtungen.
# FALSE, der IQR umfasst die mittleren 50%


# 3.8
# Ein Boxplot visualisiert robuste Lage- und Streuungsmasse.
# TRUE
