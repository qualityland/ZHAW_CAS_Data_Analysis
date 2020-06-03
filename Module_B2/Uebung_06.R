###
###  Aufgabe 1 Vorhersage eines AR(p) Prozesses
###
setwd("/Users/hofc/idp/Public/Lehre/CAS/CAS_18_8/Daten")

raw.dat   <- read.table("kraft.dat", header = FALSE)
d.kraft <- ts(raw.dat[,1], freq = 1)
plot( d.kraft)

d.kraftA <- window(d.kraft,end = 280)

### Ordnung nach dem AIC-Kriterium
r.kraft.burg.p17 <- ar.burg(d.kraftA)
r.kraft.burg.p17
r.kraft.burg.p9 <- ar.burg(d.kraftA, aic = F, order.max = 9)
r.kraft.burg.p9

### Vorhersage
r.kraft.pred.p9 <- predict(r.kraft.burg.p9, n.ahead=40)
r.kraft.pred.p17 <- predict(r.kraft.burg.p17, n.ahead=40)
### ZR darstellen
plot(d.kraft,
     xlab ="Messzeitpunkte", xlim = c(250, 350), lwd = 3)

#Vorhersage einzeichnen:
lines(r.kraft.pred.p9$pred, lty = 2, col="blue", lwd = 3)
lines(r.kraft.pred.p17$pred, lty = 2, col="red", lwd = 3)
legend( 'topleft', legend = c("AR(9)", "AR(17)"),col= c("blue", "red"), lty = 2, lwd = 2, bty = 'n')
#Prognoseband einzeichnen:
#p9
lines(r.kraft.pred.p9$pred+1.96*r.kraft.pred.p9$se,lty=2,col="blue")
lines(r.kraft.pred.p9$pred-1.96*r.kraft.pred.p9$se,lty=2,col="blue")
#p17
lines(r.kraft.pred.p17$pred+1.96*r.kraft.pred.p17$se,lty=2,col="red")
lines(r.kraft.pred.p17$pred-1.96*r.kraft.pred.p17$se,lty=2,col="red")
# Daten einzeichnen
lines(d.kraft, lwd = 2 )

### RMSE (ZR weist negative Werte auf, )
test.dat <-  window(d.kraft, start = 281)
#p9 RMSE
accuracy(r.kraft.pred.p9$pred, test.dat)[2]
#p17 RMSE
accuracy(r.kraft.pred.p17$pred, test.dat)[2]
#p0 = Mittelwert der Werte bis und mit t = 280 RMSE
accuracy(rep(mean(window(d.kraft, end = 280)),40), test.dat)[2]


###
### Darstellung des Standardfehlers
###

plot(r.kraft.pred.p9$se,ylab ="Prognostizierter Standradfehler", col ="blue", ylim = c(0, 0.35))
lines(r.kraft.pred.p17$se, col = "red")
abline( h = 0.3183947) ### Null model kein AR-Prozess korrelation nicht berücksichitgt = sd(d.kraftA)
legend( 'bottomright', legend = c("SE-AR(9)", "SE-AR(17)", "SE-Mean"),col= c("blue", "red", "black"), lty = 1, lwd = 2, bty = 'n')


###
### Aufgabe 2 und 3 Vorerhsage AR und zusammengestzt
### 
#
# Datenladen
#
# Funktion für die Residuenanalyse
zr.plot.resid <- function( res, zr.name = '')
{
  library(car)
  par(mfrow = c(2,2))
  plot( res, main = paste("Residuen von ", zr.name ,sep =""))
  qqPlot( res, main = 'qqPlot')
  acf( res , na.action = na.pass, ylim = c(-1,1), main  ='ACF')
  pacf( res, na.action = na.pass, ylim = c(-1,1), main = 'PACF')
  par( mfrow = c(1,1))
}
#Luteinizing Hormone in Blood Samples
library(forecast)
tsdisplay(lh) # Achtung ACF fängt bei lag = 1 an !!!
# oder
# layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
# plot(lh, main = "Luteinizing Hormone in Blood Samples")
# acf(lh, ylim = c(-1,1), main = "ACF")
# pacf( lh, ylim = c(-1,1), main = "PACF")
##
## AR(3)-Modell anpassen
##
fit.lh <- ar.burg(lh, order.max = 3, aic = F)
## fit.lh <- ar.burg(lh)
##
## zr.plot.resid = Funktion für Residuenanalyse
##
res <- fit.lh$resid ### ist eine ZR, resid( fit ) funktioniert nicht
### Residuenanalyse
zr.plot.resid( res = res, zr.name = 'Luteinizing Hormone')
##
## 6-Schritt-Vorhersage
##
kk <- 6
fca <- predict(fit.lh, n.ahead=kk)
## Grafische Darstellung
par( mfrow = c(1,1))
series <- lh; nobs <- length(series)
plot(series, xlim=c(1,nobs+kk), ylim=c(1,4))
# lines(nobs:(nobs+kk), c(series[nobs], fca$pred), col="red") # mit Verbindung zum letzten Messwert
lines((nobs+1):(nobs+kk),  fca$pred, col="red") # ohne Verbindung zum letzten Messwert
lines(fca$pred+qnorm(0.975)*fca$se, col="red")
lines(fca$pred-qnorm(0.975)*fca$se, col="red")
title("Luteinizing Hormone mit AR(3)-Vorhersage")

### Vergleich mit ses()
plot(ses(lh, 6))
lines(fca$pred, col ='red')
lines(fca$pred+qnorm(0.975)*fca$se, col="red")
lines(fca$pred-qnorm(0.975)*fca$se, col="red")
legend('topleft', legend = c('AR', 'ses'),col = c(2,4), lty = 1, bty = 'n')

### Evaluation
training  <- window(lh, start =1, end = 38)
test <- window(lh,start = 39)
## 6-Schritt-Vorhersage erzeugen
training.ses <- ses(training, h = 10)
#training.ar <- forecast(Arima(training, c(3,0,0)), h = 6)
accuracy(training.ses, test)[2]
#accuracy(training.ar, test)
### oder für AR RMSE
ar.fit <- ar.burg(training)
fca.ar <- predict(ar.fit, n.ahead = 10)
### AR(3)-RMSE test
accuracy(fca.ar$pred, test)[2]

##
## sunspot.year
## 
data(sunspot.year)
spy <- sunspot.year
#par(mfrow = c(2,1))
#plot(spy)
plot(sqrt(spy))
par(mfrow = c(1,1))

tsdisplay( sqrt(sunspot.year)) # Achtung ACF fängt bei 1 an !!!
# oder
#layout( matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE ))
#plot( sqrt(spy), main = "Luteinizing Hormone in Blood Samples" )
#acf( spy, ylim = c(-1,1), main = "ACF" )
#pacf( spy, ylim = c(-1,1), main = "PACF" )
##
## AR(p)-Modell anpassen
##
fit.spy <- ar.burg(spy, order.max = 9, aic = F)  ### nicht transformiert
fit.spy.sqrt <- ar.burg(sqrt(spy), order.max = 9, aic = F) # Transformation Wurzel (Varianzstabilisierung für Zähldaten)
## Residuenanalyse lh
##
res <- fit.spy$resid
zr.plot.resid( res = res, zr.name = 'sunspot.year')
### res leicht rechtsschief 
###
res <- fit.spy.sqrt$resid
zr.plot.resid( res = res, zr.name = 'sqrt(sunspot.year)')
### res ok
# 100 JahreVorhersage 
#fca.org <- predict(fit.spy, n.ahead=100) ### Forecast auf den orginal Daten
fca.sqrt <- predict(fit.spy.sqrt, n.ahead=100) ### Forecast der transformierten Daten
###
### Grafische Darstellung auf sqrt-Skala
###
par( mfrow = c(1,1) )
kk <- 100; nobs <- length(sunspot.year); start <- time( sunspot.year)[1]
plot(sqrt(sunspot.year), xlim=c(start,start+nobs+kk), ylim = c(-20, 20))
lines((start+nobs):(start+nobs+kk), c(sqrt(sunspot.year[nobs]), fca.sqrt$pred), col="red", lwd = 2)
lines((fca.sqrt$pred+1.96*fca.sqrt$se), col="red")
lines((fca.sqrt$pred-1.96*fca.sqrt$se), col="red")
abline( h = 0)
## Grafische Darstellung auf der orginal Skala fca$pred^2
par( mfrow = c(1,1) )
kk <- 100; nobs <- length(sunspot.year); start <- time( sunspot.year)[1]
plot(sunspot.year, xlim=c(start,start+nobs+kk))
lines((start+nobs):(start+nobs+kk), c(sunspot.year[nobs], fca.sqrt$pred^2), col="red", lwd = 2)
lines((fca.sqrt$pred+1.96*fca.sqrt$se)^2, col="red")
lines((fca.sqrt$pred-1.96*fca.sqrt$se)^2, col="red")
title("Sonnenflecken-Aktivität mit AR(9)-Vorhersage")

#### Vergleich mit exp.Glättung (sqrt-Skala)
library(forecast)
spy.sqrt.ses <- ses(sqrt(spy), h = 100)
plot(spy.sqrt.ses)
lines((start+nobs):(start+nobs+kk), c(sqrt(sunspot.year[nobs]), fca.sqrt$pred), col="red", lwd = 2)
lines((fca.sqrt$pred+1.96*fca.sqrt$se), col="red")
lines((fca.sqrt$pred-1.96*fca.sqrt$se), col="red")

### Evaluation auf der sqrt-Transformation
training  <- window(sqrt(spy), end = 1978)
test <- window(sqrt(spy),start = 1979)
training.ses <- ses(training, h = length(test ))
### Arima(training, c(3,0,0) = AR(3)
### training.ar <- forecast(Arima(training, c(9,0,0)), h = 10) # = AR(3), 
accuracy(training.ses, test)[2,2] # training und test 
### oder für AR RMSE
ar.fit <- ar.burg(training)
fca.ar <- predict(ar.fit, n.ahead = 10)
accuracy(fca.ar$pred, test)[2]


#
# Alpenquerender_Gueterverkehr_Schiene
# 

## Daten laden (Pfad muss angepasst werden....)
path.dat<- '~/idp/Public/Lehre/CAS/CAS_18_8/Daten/'
t.file <- paste(path.dat, 'Alpenquerender_Gueterverkehr_Schiene.csv', sep ='')
raw <- read.table(file = t.file, sep=';', header =T)
gueter <- ts(raw$Verkehr, start=1984, frequency=1)
tsdisplay(gueter)
plot( gueter) 
## Trend schätzen mit Loess und einzeichnen
xx    <- time(gueter)
trend <- loess(gueter ~ xx)
plot(gueter)
lines(trend$x, trend$fitted, col="blue")
title("Alpenquerender Güterverkehr mit Trendschätzung")

## Hineinlegen der KQ-Gerade
trend.ts <- ts(trend$fitted, start=1984, freq=1)
yy    <- window(trend.ts, 2004, 2008) 
#yy    <- window(trend.ts, 1984, 2008) ## wahrsch. sinnvoller
xx    <- time(yy)
reg   <- lm(yy ~ xx)

## evtl. besser KQ-Gerade über den gesamten Beobachtungszeitraum
## reg <- lm( gueter ~ time( gueter))

#
# Residuenanalyse
res <- resid( reg )
zr.plot.resid( res = ts(res), zr.name = 'Alpenquerender Güterverkehr')

###
res.fit <- ar.burg( reg$residuals   )
pred.rest <- predict( res.fit, n.ahead = 5)
## Trend-Extrapolation
kk       <- 5 # 2009 - 2013
trend.ex <- rev( trend.ts )[1]+( ( 1:kk ) ) * coef(reg)[2]

## Vorhersage der Zeitreihe
#fca <- list(pred=trend.ex, se=NA)
fca <- list(pred=trend.ex + pred.rest$pred , se=NA)
## Grafische Darstellung
kk <- 5; nobs <- length( gueter); start <- 1984
plot(gueter, xlim=c(start,start+nobs+kk))
lines((start+nobs):(start+nobs+kk), c(gueter[nobs], fca$pred), col="red")
title("Extrapolation der Reihe Güterverkehr")

#### Vergleich mit exp.Glättung
plot(holt(gueter, h = 5, na.action =na.pass))
lines(2009:2013,  fca$pred, col="red")
### besser einfach eine KQ-Gerade verwenden,da nur Trend sichtbar
zeit <- time(gueter)
r.lm <- lm(gueter ~ zeit)
pred.lm <- predict( r.lm , newdata = data.frame(zeit = 2009:2013))
lines( 2009:2013, pred.lm, col = 'magenta', lwd = 2)


### Evaluation
training  <- window(gueter, end = 1998)
test <- window(gueter,start = 1999, end = 2008)

### einfache lineare Regression
zeit <- time(training)
r.lm <- lm(training ~ zeit)
pred.lm <- predict(r.lm, newdata = data.frame(zeit = 1999:2008))
pred.lm <- ts(pred.lm, start = 1999, frequency = 1)
### regression an loess angepasst
## Trend schätzen mit Loess und einzeichnen
train.trend <- loess(training ~ time(training))
## Hineinlegen der KQ-Gerade
train.trend.ts <- ts(train.trend$fitted, start = 1984, freq = 1)
## yy    <- window(trend.ts, 1984, 2008) ## wahrsch. sinnvoller
reg   <- lm(train.trend.ts ~ time(train.trend.ts))

## Trend-Extrapolation
kk       <- 10 #
trend.ex <- rev( train.trend.ts )[1]+( ( 1:kk ) ) * coef(reg)[2]
trend.ex <- ts(trend.ex, start = 1999, frequency = 1)
### out-of-sample holt
accuracy(holt(training, h = kk), test)[2]
### out-of-sample kq-Gerade an loess angepasst
accuracy(trend.ex, test)[2]
accuracy(pred.lm ,test)[2]

##
## Arbeitslosenzahlen in Maine (Vorhersage 28 Monate)
##
## Daten einlesen
## Daten laden (Pfad muss angepasst werden....)
path.dat<- '~/idp/Public/Lehre/CAS/CAS_18_8/Daten/'
dat <- read.table(paste(path.dat, 'Maine.dat', sep = ''), header=TRUE)
tsd <- ts(dat$unemploy, start=c(1996,1), freq=12)
plot(tsd, main = 'Arbeitslosenzahlen ni Maine')

## Zeitreihe zerlegen mit STL
stl.tsd <- stl(tsd, s.window="periodic")
plot(stl.tsd, main="STL-Zerlegung der Arbeitslosenzahlen Maine")

## Vorhersage des Saisoneffekts
saiseff     <- stl.tsd$time.series[,1]
### saiseff.fca = saison effekt für die nächsten 2 Jahre (24) ab September = 9, 2006
saison.fca <-  ts(saiseff[9:36], start=c(2006, 9), freq=12)

## Vorhersage des Trends (28 Monate)
yy         <- window(stl.tsd$time.series[,2], start=c(2004,5), end=c(2006,8))
xx         <- time(yy)
xx
1/12*(0:11)
reg        <- lm(yy ~ xx)
kk         <- 28 # 4 Monate in 2006 + 24 Monate
trend.fca <- rev(yy)[1]+((1:kk)/12)*coef(reg)[2] # ../12 da freq = 12

## Vorhersage des stationären Rests
statrest <- stl.tsd$time.series[,3]
# acf(statrest, ylim=c(-1,1), main="ACF", plot =F)
# pacf(statrest, ylim=c(-1,1), main="PACF", plot =F)
fit      <- ar.burg(statrest)
sr.fca  <- predict(fit, n.ahead=28)$pred

## Vorhersage der Originalreihe
fca <- saison.fca + trend.fca + sr.fca

## Grafische Darstellung
kk <- 28; nobs <- 128; start <- 1996
plot(tsd, xlim=c(start,2009))#, ylim=c(-2,16))
lines(fca, col="red")
lines(c(time(tsd)[128], time(fca)[1]), c(tsd[128], fca[1]), col="red")
title("Vorhersage der Arbeitslosenzahlen in Maine")

#### Vergleich mit exp.Glättung (sqrt-Skala)
plot(hw(tsd, bootstrap = T, simulate = T, h = 28))
lines(fca, col="red")
lines(c(time(tsd)[128], time(fca)[1]), c(tsd[128], fca[1]), col="red") # Verbindungslinie

### Evaluation
training <- window(tsd, end = c(2004, 8))
test <- window(tsd, start = c(2004,9))

stl.train <- stl(training,s.window="periodic")
## Vorhersage des Saisoneffekts
saiseff     <- stl.train$time.series[,1]
### saiseff.fac = saison effekt für die nächsten 2 Jahre (24) ab September = 9, 2006
saison.fca <-  ts(saiseff[9:32], start=c(2004, 9), freq=12)
saison.fca
## Vorhersage des Trends
yy         <- window(stl.train$time.series[,2], start=c(2002,8), end=c(2004,8))
xx         <- time(yy)
reg        <- lm(yy ~ xx)
kk         <- 24 # 4 Monate in 2006 + 24 Monate
trend.fca <- rev(yy)[1]+((1:kk)/12)*coef(reg)[2] # ../12 da freq = 12

## Vorhersage des stationären Rests
statrest <- stl.train$time.series[,3]
# acf(statrest, ylim=c(-1,1), main="ACF", plot =F)
# pacf(statrest, ylim=c(-1,1), main="PACF", plot =F)
fit      <- ar.burg(statrest)
sr.fca  <- predict(fit, n.ahead=24)$pred

## Vorhersage der Originalreihe
fca <- saison.fca + trend.fca + sr.fca

### out-of-sample hw
accuracy( hw(training, h = 24), test)[2]
### out-of-sample stl
accuracy( fca, test)[2]

lines(test, lwd = 2)
plot(hw(training, h = 24))
lines(fca, col = 'red')

###
### Beispiel CO2-Zeitreihe
### ZR mit Trend und Saison
plot(co2); abline(v = 1995.917) # Dezember 1995
# 1/12*(0:11)
trainig <- window( co2, start = start( co2), end = c(1995,12))
### Trend extrahiren mit stl (2 Spalte ist Anzahl)
fit.stl <- stl(trainig, s.window="periodic")
## Saison
### 1 Spalte von time.series ist die Saisonkomponente
saison.stl <- fit.stl$time.series[,1 ]
### 2 Spalte von time.series ist die Trendkomponente
trend.stl <- fit.stl$time.series[,2]
### 3 Spalte von time.series ist der Restterm
rest.stl <- fit.stl$time.series[,3]

### Trend extrahiren mit decompose (2 Spalte ist Anzahl)
# fit.dc <- decompose(series)
# saison.dc <- fit.dc$seasonal
# trend.dc <- fit.dc$trend
# rest.dc <- fit.dc$random 

trend <- trend.stl ### oder trend.dc
## KQ-Anpassung über die letzten 2 Jahre
yy <- window(trend, c(1994,1), c(1995,12))
xx <- time(yy)
# xx
# 1/12*(0:12)
reg <- lm(yy ~ xx)
## Trend-Extrapolation
kk <- 24
trend.ex <- rev(trend)[1]+((1:kk)/12)*coef(reg)[2]
trend.ex <- ts(trend.ex, start = c(1996,1), frequency = 12)
plot(trainig); lines(trend.ex, col = 'red')
## Saison-Extrapolation
saison <- saison.stl ### oder saison.stl
saison.2y <- window(saison,start=c(1994,1),end=c(1995,12))
saison.ex <- ts(saison.2y, start=c(1996,1), end=c(1997,12), freq=12)
plot(trainig, ylim = c(-2, 370)); lines(saison.ex , col = 'red')

## Restterm
rest <- rest.stl### oder rest.dc
fit.rest <- ar.burg(rest)

fit.rest <- ar.burg(rest)
rest.ex <- predict(fit.rest, n.ahead = 24)$pred
## Zusammenfügen
series.ex <- trend.ex + saison.ex + rest.ex
## Darstellung
plot(co2, xlim=c(1993, 1999))
lines(series.ex, col="red", lwd=2)
lines(c(2010.917,2011),c(33773,28590.20), col="red", lwd=2)
title("Prognose CO2_Konzentration")

