# Analysieren der Daten mit min. 4 der 5 besprochenen Verfahren:
# - poisson.test()
#   - Schaetzen
#   - Testen
#   - Vertrauensintervalle
# - binom.test()
#   - Testen bei 2 Poisson-Realisationen
# - chisq.test()
#   - Dispersionstest
# - BootSim


# COVID-19 Infektionen am 14.03. und 14.04.2020
# df <- data.frame(
#   kanton=c( "AG", "AI", "AR", "BE", "BL", "BS", "FR", "GE", "GL", "GR","JU", "LU", "NE", "NW", "OW", "SG", "SH", "SO", "SZ", "TG", "TI", "UR", "VD", "VS", "ZG", "ZH"),
#   inf_1403=c( 31,    2,    5,   78,   47,  119,   36,  280,    5,   47,  15,   19,   68,    5,    8,   26,    1,   10,   13,    5,  262,    2,  350,   88,   13,  148),
#   inf_1404=c(912,   24,   79, 1470,  755,  119,  899, 4390,  105,  741, 185,  589,  606,  105,   64,  664,   57,  329,  258,  296, 2912,   78, 4741, 1664,  171, 3067),
#   einwohner=c(678207, 16145, 55234, 1034977, 288132, 194766, 318714, 49948, 40403, 198379, 73419, 409557, 17685, 43223, 37841, 507697, 81991, 273194,
#             159165, 276472, 353343, 36433, 799145, 343955, 126837, 1520968)
# )
#
# COVID-19 Infektionen am 14.03., 14.04. und 16.04.2020
# df <- data.frame(
#   kanton=c(    "AG",   "AI",   "AR",     "BE",    "BL",    "BS",    "FR",   "GE",   "GL",    "GR",   "JU",    "LU",   "NE",   "NW",   "OW",    "SG",   "SH",    "SO",    "SZ",    "TG",    "TI",   "UR",    "VD",    "VS",    "ZG",     "ZH"),
#   inf_1403=c(    31,      2,      5,       78,      47,     119,      36,    280,      5,      47,     15,      19,     68,      5,      8,      26,      1,      10,      13,       5,     262,      2,     350,      88,      13,      148),
#   inf_1404=c(   912,     24,     79,     1470,     755,     899,     879,   4390,    105,     753,    185,     589,    606,    105,     64,     664,     57,     329,     258,     296,    2912,     78,    4741,    1664,     171,     3067),
#   inf_1604=c(   943,     24,     79,     1515,     781,     917,     907,   4565,    106,     764,    189,     599,    616,    107,     66,     694,     60,     329,     265,     308,    2953,     78,    4844,    1707,     171,     3151),
#   einw10k=c(67.8207, 1.6145, 5.5234, 103.4977, 28.8132, 19.4766, 31.8714, 49.948, 4.0403, 19.8379, 7.3419, 40.9557, 17.685, 4.3223, 3.7841, 50.7697, 8.1991, 27.3194, 15.9165, 27.6472, 35.3343, 3.6433, 79.9145, 34.3955, 12.6837, 152.0968)
# )

# kanton:   Kantons (Abkuerzung)
# inf_1403: COVID-19 Infektionen am 14.03.2020
# inf_1404: COVID-19 Infektionen am 14.04.2020
# inf_1604: COVID-19 Infektionen am 16.04.2020
# einw10k:  Anzahl Einwohner (in 10'000)
df <- data.frame(
  kanton=c(    "AG",   "AI",   "AR",     "BE",    "BL",    "BS",    "FR",   "GE",   "GL",    "GR",   "JU",    "LU",   "NE",   "NW",   "OW",    "SG",   "SH",    "SO",    "SZ",    "TG",    "TI",   "UR",    "VD",    "VS",    "ZG",     "ZH"),
#  inf_1403=c(    31,      2,      5,       78,      47,     119,      36,    280,      5,      47,     15,      19,     68,      5,      8,      26,      1,      10,      13,       5,     262,      2,     350,      88,      13,      148),
  inf_1404=c(   912,     24,     79,     1470,     755,     899,     879,   4390,    105,     753,    185,     589,    606,    105,     64,     664,     57,     329,     258,     296,    2912,     78,    4741,    1664,     171,     3067),
  inf_1604=c(   943,     24,     79,     1515,     781,     917,     907,   4565,    106,     764,    189,     599,    616,    107,     66,     694,     60,     329,     265,     308,    2953,     78,    4844,    1707,     171,     3151),
  einw10k=c(67.8207, 1.6145, 5.5234, 103.4977, 28.8132, 19.4766, 31.8714, 49.948, 4.0403, 19.8379, 7.3419, 40.9557, 17.685, 4.3223, 3.7841, 50.7697, 8.1991, 27.3194, 15.9165, 27.6472, 35.3343, 3.6433, 79.9145, 34.3955, 12.6837, 152.0968)
)


## Infektionen per 10'000 Einwohnern

# 14.04.2020: Infektionen per 10'000 Einwohner
i1404_10k <- df$inf_1404 / df$einw10k

# Ueberblick ueber die absoluten Infektionszahlen per Kanton
plot(i1404_10k,
     main = "COVID-19 Infektionen am 14.04.2020",
     xlab = "Kanton",
     ylab = "Anzahl Infektionen",
     type = "h",
     lwd = 6,
     col = "blue",
     lend = 2,
     xaxt = "n")
axis(1, at = 1:length(i1404_10k), labels = df$kanton)



# Anzahl Infektionen pro 10'000 und ihre Haeufigkeit (in den Kantonen)
#plot(table(round(i1404_10k)),
plot(table(rep(round(i1404_10k), round(df$einw10k))),
     main = "COVID-19 Infektionen in den Kantonen",
     xlab = "Infektionen pro 10'000 Einwohner",
     ylab = "Haeufigkeit",
#     ylim = c(0, 60),
     col = "blue",
     type = "h",
     lwd = 3)



######################### Schaetzen #############################

### lambda schaetzen

# totale Anzahl Infektionen / pro 10'000
n <- sum(i1404_10k)

# mittlere Anzahl Infektionen per 10'000 Einwohnern
mu <- mean(i1404_10k)

# angepasstes Modell
yModel <- n * dpois(sort(i1404_10k), lambda = mu)
lines(sort(i1404_10k), yModel, type="b", lwd=2, col="red")
# yModel <- n * dpois(min(i1404_10k):max(i1404_10k), lambda = mu)
# lines(min(i1404_10k):max(i1404_10k), yModel, type="b", lwd=2, col="red")



######################### Testen #############################

# 16.04.2020: Infektionen per 10'000 Einwohner (gerundet)
i1604_10k <- round(df$inf_1604 / df$einw10k)

# Infektionen / 10'000 Einwohnern
# Waadt am 16.04.2020:
df$kanton[23]
i1604_10k[23]

# Frage: Ist diese Infektionsrate bei angenommener Poisson-Verteilung plausibel
# bei lambda=Signifikanzniveau von 95%?
poisson.test(x = 61, r = 27.85, conf.level = 0.95)

# Antwort: Nein, aufgrund des niedrigen p-Werts (6.213e-08) muss die Nullhypothese
#          verworfen werden, dass die Infektionsrate Poisson-verteilt ist.




######################### Vertrauensinterval #############################

# Frage: Auf Basis der Infektionsraten vom 14.04., was sind plausible Werte fuer lambda?
poisson.test(x = sum(i1404_10k), T = nrow(df), conf.level = 0.95)

# Antwort: Jede Poisson-Verteilung mit lambda zwischen 25.85 und 29.95 wird die Infektionsraten
#          vom 14.04. geeignet beschreiben.


# Frage: Welche Anzahl Infektionen sind fuer die Kantone plausibel

# Vertrauensintervalle fuer die Infektionszahlen vom 14.04.
VI <- matrix(NA, nrow = nrow(df), ncol = 2)

# Vertrauensintervalle fuer alle Kantone in Matrix speichern
for(i in 1:nrow(df)){
  pt <- poisson.test(x = df$inf_1404[i], T = df$einw10k[i])
  VI[i,] <- pt$conf.int
}

plot(i1404_10k,
     main = "COVID-19 Infektionen am 14.04.2020",
     sub = "(Vertrauensintervall: lila)",
     xlab = "Kantone",
     ylab = "Anzahl Infektionen",
     type = "h",
     lwd = 6,
     col = "blue",
     lend = 2,
     xaxt = "n")
axis(1, at = 1:length(i1404_10k), labels = df$kanton)
segments(1:nrow(df), VI[,1], 1:nrow(df), VI[,2], col = "violet", lwd = 2)




################# Testen bei zwei Poisson Realisationen #####################

## COVID-19 Infektionen am 14.04. und 16.04.2020
inf_ch_1404 <- sum(df$inf_1404)
inf_ch_1604 <- sum(df$inf_1604)


# Frage: Am 14.04. waren in der Schweiz 26'052 Personen infiziert, am 16.04. bereits 26'738.
#        Kann die Zunahme rein zufaellig sein oder ist sie signifikant (Niveau: 95%)?

binom.test(x=inf_ch_1604, n=inf_ch_1404 + inf_ch_1604, p=0.5)

# Antwort: Nein, der p-Wert liegt knapp ausserhalb des Annahmebereichs (5%). Die Zunahme der
#          Infektionen ist signifikant  und kann nicht mehr als zufaellig betrachtet werden.




######################### Dispersionstest #############################


### mit der Bootstrap-Methode
library(boot)

## Bootstrap-Vertrauensintervall für die Dispersion
f.disp <- function(x, ind){
  ## x   = ursprünglicher Beobachtungsvektor
  ## ind = Beobachtungsnummer für die Bootstrap-Stichprobe
  xx <-x[ind]       # erzeugen der Bootstrap-Stichprobe
  var(xx)/ mean(xx) # Berechnet die Dispersion für die Bootstrap-Stichprobe
}

set.seed(seed=123)
inf.boot2 <- boot(i1404_10k, f.disp, R=999, stype="i")
boot.ci(inf.boot2, conf=0.95, type="perc")
#  Level    Percentile     
#  95%   ( 5.87, 22.67 )

# Da die Nullhypothese "s^2/xq = 1" NICHT im 95%-Vertrauensintervall [5.87, 22.67] liegt
# kann die Nullhypothese auf dem 2.5% Niveau verworfen werden.
#
# Schluss: Die Poisson-Verteilung ist nicht geeignet um die
#          COVID-19 Infektionen in der Schweiz zu beschreiben.


### mit dem Chiquadrat-Test

chisq.test(i1404_10k)
# X-squared = 400.97, df = 25, p-value < 2.2e-16

# Da der P-Wert von 2.2e-16 kleiner als das 2.5% Niveau ist,
# wird die Null-Hypothese "Daten können durch eine Poisson-Verteilung
# beschrieben werden" verworfen.




