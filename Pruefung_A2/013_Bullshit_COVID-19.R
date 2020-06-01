
##################################### Bullshit ################################

# Daten vom 14.03.2020 und 14.04.2020

# COVID-19 Infektionen am 14.03.2020
df <- data.frame(
  kanton=c( "AG", "AI", "AR", "BE", "BL", "BS", "FR", "GE", "GL", "GR","JU", "LU", "NE", "NW", "OW", "SG", "SH", "SO", "SZ", "TG", "TI", "UR", "VD", "VS", "ZG", "ZH"),
  inf_abs=c( 31,    2,    5,   78,   47,  119,   36,  280,    5,   47,  15,   19,   68,    5,    8,   26,    1,   10,   13,    5,  262,    2,  350,   88,   13,  148),
  einw10k=c(67.8207, 1.6145, 5.5234, 103.4977, 28.8132, 19.4766, 31.8714, 49.948, 4.0403, 19.8379, 7.3419, 40.9557, 17.685, 4.3223, 3.7841, 50.7697, 8.1991, 27.3194,
            15.9165, 27.6472, 35.3343, 3.6433, 79.9145, 34.3955, 12.6837, 152.0968)
)

### Schaetzen des Parameters lambda

# Infektionen auf 10'000 Einwohner
inf_10k <- df$inf_abs / df$einw10k

# lambda hut
# Mittelwert schaetzt den Parameter lambda
lambda_hut <- mean(inf_10k)


### Test eines vorgegebenen Parameters lambda

# Frage: Sind die Extremwerte fuer Schaffhausen (SH) und das Tessin (TI) plausibel?

poisson.test(x = min(inf_10k), r = lambda_hut)





Inf_1404 <- data.frame(
  kanton=c( "AG", "AI", "AR", "BE", "BL", "BS", "FR", "GE", "GL", "GR","JU", "LU", "NE", "NW", "OW", "SG", "SH", "SO", "SZ", "TG", "TI", "UR", "VD", "VS", "ZG", "ZH"),
  inf_1404=c(912,   24,   79, 1470,  755,  119,  899, 4390,  105,  741, 185,  589,  606,  105,   64,  664,   57,  329,  258,  296, 2912,   78, 4741, 1664,  171, 3067),
  einwohner=c(67.8207, 1.6145, 5.5234, 103.4977, 28.8132, 19.4766, 31.8714, 49.948, 4.0403, 19.8379, 7.3419, 40.9557, 17.685, 4.3223, 3.7841, 50.7697, 8.1991, 27.3194,
              15.9165, 27.6472, 35.3343, 3.6433, 79.9145, 34.3955, 12.6837, 152.0968)
)




# Anzahl Infizierer Personen
# am 14.03.
n_maerz14 <- sum(c19$maerz14)

# am 14.04.
n_april14 <- sum(c19$april14)

# lambda schaetzen
# am 14.03.
mu_maerz14 <- mean(c19$maerz14)
# am 14.04.
mu_april14 <- mean(c19$april14)

# Poisson Modell
# fuer 14.03.
mod_maerz14 <- n_maerz14 * dpois(min(c19$maerz14):max(c19$maerz14), lambda = mu_maerz14)
plot(mod_maerz14, type = "l")
# fuer 14.04.
mod_april14 <- n_april14 * dpois(min(c19$april14):max(c19$april14), lambda = mu_april14)
plot(mod_april14, type = "l")

## Beispiel Hefezellen ########################################################

## Im Dataframe HZ finden sich die Daten
plot(HZ$k, HZ$freq, type="h", lwd=6, lend=2, col="blue", xlab="k", ylab="Haeufigkeit")

# Totale Anzahl Hefezellen
n <- sum(HK$freq)

# λ schaetzen
mu <- sum(HZ$freq * HZ$k) / n 

# Mittlere Anzahl Hefezellen pro Quadraetchen
mu
# [1] 4.68 > 

# Modellanpassung n · pk bestimmen
yModell <- n * dpois(HZ$k, lambda=mu)

# n * pk einzeichnen
lines(HZ$k, yModell, type="b", lwd=2, col="red")

## Beispiel Hefezellen (Ende) #################################################
