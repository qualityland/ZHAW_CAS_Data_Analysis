
### Wasserverbrauch

# Daten laden, in Tabellenform bringen und Spaltennamen saeubern
library(reshape2)
library(janitor)
path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Leistungsnachweis_A2/data/"
wv <- read.csv(paste0(path, "WasserverbrauchKtZH.csv"), na.strings = c("null"))
wv <- dcast(wv, BFS_NR + GEBIET ~ JAHR, value.var = "WvpTE")
wv <- clean_names(wv)
wv$d2018_2017 <- wv$x2018 - wv$x2017

length(wv$x2018)
head(wv)

# Schaetzung der Parameter x_quer und sigma
(xq <- mean(wv$x2018, na.rm = T))
(s <- sd(wv$x2018, na.rm = T))


# Boxplot und Histogram
boxplot(wv$x2018, horizontal = T, xlab = "in Liter")
hist(wv$x2018, ylab = "Häufigkeit", xlab = "in Liter", main = "")
#curve(dnorm(x, xq, sd=s), col="magenta", lwd=2, add=TRUE)
#curve(dnorm(x, mean=xq, sd=s), col=2, lwd=2, add=TRUE)

## Kann man eine Normalverteilung annehmen?

# qqnorm mit Simulationen
source(paste0(path, "RFn-qqnormSim.R"))

# complete cases 2018
wv.cc2018 <- wv$x2018[complete.cases(wv$x2018)]

# Standardnormal?
# mit Ausreissern
qqnormSim(wv.cc2018, SEED = 123)
qqline(wv$wv.cc2018)
# ohne Ausreisser
qqnormSim(wv.cc2018, rob = TRUE, SEED = 123)
qqline(wv.cc2018)

# Lognormal?
# mit Ausreissern
qqnormSim(log(wv.cc2018), SEED = 123)
qqline(log(wv.cc2018))
# ohne Ausreisser
qqnormSim(log(wv.cc2018), rob = TRUE, SEED = 123)
qqline(log(wv.cc2018))


# Vertrauensintervalle

# fuer mu (mit Ausreissern)
t.test(wv$x2018, alternative = "two.sided", conf.level = 0.95)
# fuer mu (ohne Ausreisser)
t.test(wv$x2018[wv$x2018 < 500], alternative="two.sided", conf.level=0.95)


# Vorzeichentsts: Binom- und Wilcoxon-Test

# Frage: Im Vergleich zu 2017 hat der Wasserverbrauch 2018 etwas abgenommen.
#        Ist diese Abnahme signifikant oder rein zufaellig?

# complete cases 2018
wv.cc.d2018_2017 <- wv$d2018_2017[complete.cases(wv$d2018_2017)]

## Binomial-Test
binom.test(
  sum(wv.cc.d2018_2017 > 0),
  n = length(wv.cc.d2018_2017),
  p = 0.5,
  alternative = "two.sided",
  conf.level = 0.95
)

## Wilcoxon-Test

wilcox.test(wv$d2018_2017, alternative="two.sided", mu=0, conf.level=0.95)
## Antwort: Da der P-Wert von 9.73e-07 kleiner als das Niveau von 0.05 ist,
## kann die Nullhypothese (Differenz = 0) verworfen werden.


head(wv)
wv$d2018_2017 <- wv$x2018 - wv$x2017
head(wv)
sum(wv$d2018_2017, na.rm = T)
