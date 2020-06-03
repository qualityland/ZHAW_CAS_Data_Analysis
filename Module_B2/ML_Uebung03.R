###
### ML Übungsblatt 03
###
library( forecast)
### Achtung Registered S3 method overwritten
setwd('/Users/hofc/idp/Public/Lehre/CAS/CAS_DA/CAS_20_11/Daten')
neckerTemp <- readRDS('neckerTemp.rds')
plot(neckerTemp)
acf(neckerTemp)
plot(fit.stl <-  stl(neckerTemp, s.window = 'periodic'))
acf(fit.stl$time.series[,3])
plot(neckerTemp, xlim = c(296, 310), ylim = c(8.2,11.3), lwd = 2, ylab = 'Temperatur [°C ]',
     main = 'Temperatur des Neckers zwischen 23.10.2010-03.11.2010')
lines( ses(neckerTemp, alpha = 0.05)$fitted, col='purple', lty = 2,lwd = 2)
lines( ses(neckerTemp, alpha = 0.95)$fitted, col='green', lty = 2,lwd = 2)
lines( ses(neckerTemp)$fitted, col='red', lty = 2,lwd = 2) 
lines( holt(neckerTemp)$fitted, col='blue', lty = 2,lwd = 2) 
lines( hw(neckerTemp)$fitted, col='cyan' , lty = 2,lwd = 2) 

### Vorhersage mit lines und Glätter-Objekt$mean damit mehrere Vorhersagen
### im gleichen Plot dargestellt werden können
### simple
lines( ses(neckerTemp, alpha = 0.05, h = 48)$mean, col='purple', lty = 1,lwd = 2)
lines( ses(neckerTemp, alpha = 0.95, h = 48)$mean, col='green', lty = 1,lwd = 2)
lines( ses(neckerTemp, h = 48)$mean, col='red', lty = 1,lwd = 2)
### holt
lines( holt(neckerTemp, h = 48)$mean, col='blue', lty = 1,lwd = 2)
### hw
lines( hw(neckerTemp, h = 48)$mean, col='cyan', lty = 1,lwd = 2) 

### Legende
## accuracy
ac <- vector()
ac[1] <- accuracy(ses(neckerTemp, alpha = 0.05))[2]
ac[2] <- accuracy(ses(neckerTemp, alpha = 0.95))[2]
ac[3] <- accuracy(ses(neckerTemp))[2]
ac[4] <- accuracy(holt(neckerTemp))[2]
ac[5] <- accuracy(hw(neckerTemp))[2]
legend.text <- c('ses_0.05', 'ses_0.95', 'ses_lik', 'holt_lik', 'hw_lik')
legend.text <- paste(legend.text, 'RMSE:',round(ac,3))
legend.col <- c('purple', 'green', 'red', 'blue', 'cyan')
legend('topleft', legend = legend.text,
       col = legend.col, lty = 1, bty ='n')

## Trend kann bei holt und hw mit dem Argument damped = T gedämpft werden
### 
### holt
lines( holt(neckerTemp, h = 48, damped = T)$mean, col='blue', lty = 3,lwd = 2)
### hw
lines( hw(neckerTemp, h = 48, damped = T)$mean, col='cyan', lty = 3,lwd = 2) 

legend.text <- c(legend.text, c('holt damped', 'hw damped'))
legend.col <- c(legend.col, 'blue', 'cyan')
legend('topleft', legend = legend.text,
       col = legend.col, lty = c(rep(1,5),rep(2,3)), bty ='n')

### 
fit.ets <- ets(neckerTemp)
fit.ets$method #ETS(A,A,A) = hw
# insample RMSE
accuracy( ets(neckerTemp) )[2]
accuracy( hw(neckerTemp) )[2]


library( car)
# Funktion für die Residuenanalyse
zr.plot.resid <- function( res, zr.name = '')
{
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), respect = TRUE)
  plot( res, main = paste("Residuen von ", zr.name ,sep =""))
  car::qqPlot( res, main = 'qqPlot')
  acf( res , na.action = na.pass, ylim = c(-1,1), main  ='ACF')
  par(mfrow = c(1,1))
}


###
### Luteinizing Hormone
###
## Daten laden
tsdisplay(lh)
## Luteinizing Hormone einfaches Exponetielles Glätten
ses.fit <- ses(lh, h = 6, main = "Luteinizing Hormone", level = 0.99)
plot(ses.fit, sub ='Luteinizing Hormone')
ses.fit$model
zr.plot.resid(resid( ses.fit ), zr.name = 'Luteinizing Hormone')
ses.fit <- ses(lh,h = 6, main = "Luteinizing Hormone", level = 0.99)
# oder da Fehler nicht normalverteilt ses.fit <- ses(lh, bootstrap = T, h = 6, main = "Luteinizing Hormone", level = 0.99)



### Prüfung ob ANN die obtimale Zerlegung ist
ets(lh) ### ANN

###
### Sunspot-Daten
###
plot(sunspot.year)
sqrt.sp.y <- sqrt(sunspot.year) ### Varianz Stabilisierung
plot(sqrt.sp.y) 
## Sunspot-Daten einfaches Exponetielles Glätten
ses.fit <- ses(sqrt.sp.y, h = 100, level = 0.95)
ses.fit$model
zr.plot.resid(sqrt.sp.y, zr.name = 'sqrt(sunspot.year)')
zr.plot.resid(ses.fit$residuals, zr.name = 'resid sqrt(sunspot.year)')

plot(ses.fit, ylab = expression(sqrt(Anzahl~~~Sonnenflecken)))
abline( h = 0, col = 'red')
### Original Skala (Achtung, nicht sinnvoll für PIs, da negativ)
plot(sunspot.year, xlim = c(1700, 2100), ylim = c(-10,300))
lines(ses.fit$mean^2)
### PI-Grenzen können nicht sinnvoll zurücktransformiert werden
### Prüfung ob ANN die obtimale Zerlegung ist
ets(sqrt.sp.y) ### AAdN
plot(forecast(ets(sqrt.sp.y))) # besser opt.crit = 'amse, nmse = 30 

# zyklisches Verhalten kann nich erfasst werden
# sinnvoller (wenn zwingen exp. Glät. Verfahren eingesetzt werden muss)
plot(forecast(ets(sqrt.sp.y, opt.crit = 'amse', nmse = 11 )))

### direkt auf der Orginal (keine plausible Prognose)
plot(forecast(ets(sunspot.year), h = 100, level = 0.95))

###
### Alpenquerender Güterverkehr
###
## Daten laden
setwd("/Users/hofc/idp/Public/Lehre/CAS/CAS_DA/CAS_20_11/Daten/")
raw    <- read.delim("Alpenquerender_Gueterverkehr_Schiene.csv", sep = ';', header = T)
gueter <- ts(raw$Verkehr, start=1984, frequency=1)
plot(gueter)

## Güterverkehr mit Holt exponetielles Glätten mit Trend
holt.fit <- holt(gueter, h = 5) ### Achtung Missing Value in der Zeitreihe
zr.plot.resid(resid( holt.fit ), zr.name = 'Alpenquerender Güterverkehr')
plot(holt.fit, sub = 'Alpenquerender Güterverkehr')

### Prüfung ob es noch eine obtimaler Zerlegung (AIC) gibt
fit.ets <- ets(gueter) 
fit.ets$method ### (A,A,N) = holt()
### Vergleich mit einfacher Regression
abline(r.lm <- lm(gueter ~ time(gueter)))
zr.plot.resid(ts(resid(r.lm)), 'lm(gueter ~ time(gueter))')

###
### Arbeitslosenzahlen in Maine
###
## Daten einlesen
setwd("/Users/hofc/idp/Public/Lehre/CAS/CAS_DA/CAS_20_11/Daten/")
dat <- read.table('Maine.dat', header=TRUE)
tsd <- ts(dat$unemploy, start=c(1996,1), freq=12)

## Arbeitslosenzahlen in Maine mit Holt-Winter exponetielles Glätten mit Trend und Saison
hw.fit <- hw(tsd, bootstrap = T, simulate = T, h = 28)
plot(hw.fit, sub ='Arbeitslosenzahlen in Maine' )



### Prüfung ob Voraussetzunge für VI gegeben sind
zr.plot.resid( res = hw.fit$residuals, zr.name = 'Arbeitslosenzahlen in Maine')


### Prüfung ob AAN die obtimale Zerlegung ist
fit.ets <- ets(tsd)
fit.ets$method ### M,Ad,M
fit.ets <- ets(tsd, allow.multiplicative.trend = T)
fit.ets$method ### M,Md,M
plot(forecast(ets(tsd), h = 20),
     ylab ='Arbeitslosenrate in Maine[%]' )
lines(forecast( ets(tsd, allow.multiplicative.trend = T), h = 20)$mean, col  = 'red')

zr.plot.resid( res = resid(ets(tsd, allow.multiplicative.trend = F)), zr.name = 'Arbeitslosenzahlen in Maine')
zr.plot.resid( res = resid(ets(tsd, allow.multiplicative.trend = T)), zr.name = 'Arbeitslosenzahlen in Maine')


###
### Aufgabe 4) Evaluation der Vorhersage
###

## Luteinizing Hormone
## Daten laden
training  <- window(lh, start =1, end = 38)
test <- window(lh,start = 39)
## 10-Schritt-Vorhersage erzeugen
training.fit <- ses(training)
accuracy(forecast(training.fit, h = 10), test)

### optimales Modell nach AIC
training.fit.ets <- ets(training)
training.fit.ets$method
accuracy(forecast(training.fit.ets, h = 10), test)

### Darstellung
plot(forecast(training.fit, h = 10), main = 'Luteinizing Hormone', xlim = c(30,48))
lines(forecast(training.fit.ets, h = 10)$mean, col = 'red')
lines(test)
legend( 'topleft', legend = c('gemessen', 'ANN', training.fit.ets$method),
        col = c(1,2,4), lty = 1, lwd = 2,bty = 'n')


## Daten laden
data(sunspot.year)
sqrt.sp.y <- sqrt(sunspot.year) ### Varianz Stabilisierung

training  <- window(sqrt.sp.y, end = 1978)
test <- window(sqrt.sp.y,start = 1979)

### out-of-sample accuracy
training.fit <- ses(training)
accuracy(forecast(training.fit, h = 10), test)

### optimales Modell nach AIC
training.fit.ets <- ets(training)
accuracy(forecast(training.fit.ets, h = 10), test)

### Darstellung
plot(forecast(training.fit, h = 10), main = 'sunspot.year', xlim = c(1950,1988))
lines(forecast(training.fit.ets, h = 10)$mean, col = 'red')
lines(test)
legend( 'topleft', legend = c('gemessen', 'ANN', training.fit.ets$method ),
        col = c(1,4,2), lty = 1, lwd = 2,bty = 'n')


## Alpenquerender Güterverkehr

## Daten laden
setwd("/Users/hofc/idp/Public/Lehre/CAS/CAS_DA/CAS_20_11/Daten/")
raw    <- read.delim("Alpenquerender_Gueterverkehr_Schiene.csv", sep=';', header = T)
raw <- raw[1:25,] # NA weglassen
gueter <- ts(raw$Verkehr, start=1984, frequency=1)

training  <- window(gueter, end = 1998)
test <- window(gueter,start = 1999, end = 2008) ### NA value bei 2009

### out-of-sample accuracy
training.fit <- holt(training)
accuracy(forecast(training.fit, h = 10), test)

### optimales Modell nach AIC (Optimierung versagt, zu wenig Daten)
training.fit.ets <- ets(training)
training.fit.ets$method
accuracy(forecast(training.fit.ets, h = 10), test)

### Darstellung
plot(forecast(training.fit, h = 10), main = 'Alpenquerender Güterverkehr')
lines(training.fit$fitted, col= 'blue')
lines(forecast(training.fit.ets, h = 10)$fitted, col= 'red')
lines(forecast(training.fit.ets, h = 10)$mean, col = 'red')
lines(test)
legend( 'topleft', legend = c('gemessen', 'AAN', 'ets(MNN)'),
        col = c(1,4,2), lty = 1, lwd = 2,bty = 'n')

## Arbeitslosenzahlen in Maine
setwd("/Users/hofc/idp/Public/Lehre/CAS/CAS_DA/CAS_20_11/Daten/")
## Daten einlesen
dat <- read.table('Maine.dat', header=TRUE)
maine <- ts(dat$unemploy, start=c(1996,1), freq=12)

training  <- window(maine, end = c(2004,8))
test <- window(maine,start = c(2004,9)) ### length(test) = 24 

### out-of-sample accuracy
training.fit <- hw(training)
model.frame(training.fit)
accuracy(forecast(training.fit, h = 24), test)

### optimales Modell nach AIC
training.fit.ets <- ets(training) # ETS(MAdM)
training.fit.ets$method # ETS(MAdM)
accuracy(forecast(training.fit.ets, h = 24), test)

### Darstellung
plot(forecast(training.fit, h = 24), main = 'Arbeitslosenzahlen in Maine')
lines(forecast(training.fit.ets, h = 24)$mean, col = 'red')
lines(test)
legend( 'bottomleft', legend = c('gemessen', 'AAA', 'ets(MAdM)'),
        col = c(1,4,2), lty = 1, lwd = 2,bty = 'n')










