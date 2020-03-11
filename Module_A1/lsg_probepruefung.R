## on-the-fly Loesung waehrend der VO
load("~/workspace/zhaw-cas-da/2018/pruefung/bikes.RData")
View(bikes)

dim(bikes) # nrow(bikes); ncol(bikes)
str(bikes) # 7
sum(unlist(lapply(bikes, is.factor)))

str(bikes)
# Verhaeltnisskala
# quantitativ-diskret

bikes$hr <- as.integer(bikes$hr)
# Begruendung: Ordnung spielt Rolle, somit
# entweder geordneter Faktor oder Stunden als
# numerisch/integer abspeichern
# Achtung: je nachdem werden sich die Ergebnisse
# aendern.

hist(bikes$casual)
boxplot(bikes$casual)
barplot(table(bikes$weathersit),
        horiz = TRUE, las = 1)
plot(bikes$weathersit)

hist(bikes$cnt) # rechtsschief

boxplot(cnt ~ hr, data = bikes)
# Interpretation: nichtlinearer
# Verlauf. Um ca 7 steigen die Ausleihen,
# um 9 dann die erste Spitze
# faellt danach wieder...
# um 18-19 Uhr wieder viele Ausleihen
# Nachts weniger... bla bla

x <- bikes[bikes$dteday == "2011-08-14", ]
plot(cnt ~ hr,
     data = x,
     type = "l")
# oder:
plot(cnt ~ hr,
     data = x)
lines(x$hr, x$cnt)
# oder:
plot(cnt ~ hr,
     data = bikes %>%
       filter(dteday == "2011-08-14"),
     type = "l")
# nichtlinearer Tagesverlauf der Ausleihen mit
# Spitzen um die Mittagszeit und rund um 18-19 Uhr.

m <- bikes %>%
  group_by(mnth) %>%
  summarize(meancnt = mean(cnt))
plot(meancnt ~ mnth, data = m)
# Ausleihen stark abhaengig
# nach Monat. Sie steigen bis Juni (Spitze)
# und fallen dann wieder...

cor(sqrt(bikes$cnt), bikes$hum)
# je hoeher die Luftfeuchtigkeit,
# desto weniger Ausleihen
# (schwach neg. lin. Korrelation)

#OPTIONAL:
library(robustbase)
x <- bikes %>%
  mutate(scnt = sqrt(cnt)) %>%
  select(scnt, hum)
covMcd(x, cor = TRUE)$cor
#optional:
plot(x)

# Multipe Choice

# i) nein (ist bimodal)

# ii) ja (ist aber FALSCH!)

# iii) ja, Ausreisser ist klar ersichtlich

# iv) nein (ist ein neg. linearer Zh)

# v) laut ?swiss ist sie quantitiav-diskret, da es um Counts geht.
# Wenn man die Werte betrachtet, sind es aber eher Raten.
# Nicht so ganz klar.

# vi) nein, die Standardabweichung laesst sich nicht direkt ablesen.
# Die klass. Standardabweichung wird nicht gezeigt, und die robuste
# Standardabweichung ueber IQR muesste noch mit einem Faktor multipliziert werden.

# vii) nein, siehe Definition des Medians

# viii) ja

