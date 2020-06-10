data.dir <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Pruefung_B2/data/"
load(paste0(data.dir, "riders.Rdata"))
str(riders)

# a)
r.ts <- ts(riders$avgnumber, start = c(1971, 9), frequency = 12)
r.ts

# b)
plot(r.ts, main = 'Busfahrgäste pro Monat in Iowa City')
# Die Reihe ist nicht stationär, weil sie sowohl einen Trend, als auch eine Saison-Komponente aufweist und
# ausserdem die Varianz nach rechts zunimmt.


# c)
# logarithmieren
r.ts.log <- ts(log(riders$avgnumber), start = c(1971, 9), frequency = 12)

# s.window=3
r.stl.3 <- stl(r.ts.log, s.window = 3)
plot(r.stl.3, main = 'STL Zerlegung mit s.window=3')
monthplot(r.stl.3, main = 'monthplot mit s.window=3')

# s.window=15
r.stl.15 <- stl(r.ts.log, s.window = 15)
plot(r.stl.15, main = 'STL Zerlegung mit s.window=15')
monthplot(r.stl.15, main = 'monthplot mit s.window=15')

# s.window='periodic'
r.stl.p <- stl(r.ts.log, s.window = 'periodic')
plot(r.stl.p, main = 'STL Zerlegung mit s.window=periodic')
monthplot(r.stl.p, main = 'monthplot mit s.window=periodic')

# s.windows=15  und 'periodic' sind sehr ähnlich, ich würde 'periodic' bevorzugen,
# weil der monthplot() die besten Ergebnisse für die Glättung liefert.

# d)
# Restterm (s.window=3)
r.stl.rest <- r.stl.3$time.series[, 3]
library(forecast)
tsdisplay(r.stl.rest)

# i)
# Die ACF zeigt im mittleren Teil noch einen Trend.
# Die PACF zeigt cut-offs bei Lag 2, 4, 7 und 9.


# ii)
# Visuell von der PACF her wären p = 2, 4, 7 und 9 Kandidaten für ein AR(p) Modell.

# iii)
(r.ar.burg <- ar.burg(r.stl.rest))
# Order selected 9


# iv)


# e)
r.train <- window(r.ts.log, end = c(1981, 12))
r.test <- window(r.ts.log, start = c(1982, 1))

(r.fit <- ets(r.train))
accuracy(forecast(r.fit, h = length(r.test)), r.test)


# f)

r.fit <- ets(r.train)

# g)

plot(forecast(r.fit))

# h)
accuracy(forecast(r.train, h=12 r.test))

