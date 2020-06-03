###
### Uebungsblatt 4
###
### 1) Simulierte Zeitreihenprozesse
#---------------------------
# X(t) = E(t)-0.5E(t-1)
e <- c(0,rnorm(300)) # t = 0, ..., 301
x <- e[2:301] - 0.5*e[1:300]
par( mfrow = c(1,2))
plot( ts(x)); acf( x, ylim = c(-1,1))
### oder mit arima.sim
# x.as <-arima.sim(300, model = list( ma = -0.5), n.start = 1, start.innov = 0)
# plot( x.as); acf( x.as, ylim = c(-1,1))
#---------------------------
# X(t) = 1.2X(t-1)+E(t)
x[1:300] <- c(0, rep(NA,299))
for( i in 2:301)
{
  x[i] <- 1.2*x[i-1] + rnorm(1)
}
par( mfrow = c(1,2))
plot( ts(x)); acf( x, ylim = c(-1,1))
#---------------------------
# X(t) = 0.5X(t-1)-E(t)
x <- 0
for( i in 2:301)
{
  x[i] <- 0.5*x[i-1] + rnorm(1)
}
par( mfrow = c(1,2))
plot( ts(x)); acf( x, ylim = c(-1,1))
#---------------------------
# X(t) = X(t-1)E(t)
log.x <- 0
for( i in 2:301)
{
  log.x[i] <- 1*log.x[i-1] + rnorm(1)
}
par( mfrow = c(2,2))
plot( ts(exp(log.x))); acf( exp(log.x), ylim = c(-1,1))
plot( ts(log.x)); acf( exp(log.x), ylim = c(-1,1))

###
### 2) Modellierung eines AR(p) Prozesses
###
setwd("/Users/hofc/idp/Public/Lehre/CAS/CAS_DA/CAS_19_10/Daten")

raw.dat   <- read.table("kraft.dat", header = FALSE)
d.kraft <- ts(raw.dat[,1], freq = 1)

# plot(stl(d.kraft, s.window = 'periodic'))
plot( d.kraft, lwd = 2)

d.kraftA <- window(d.kraft,end = 280)
plot(d.kraftA)

#Aufgrund der Versuchsanordnung erwarten wir Perioden der Länge 2 Sekunden.
#Da im zeitlichen Abstand von 0.15 Sekunden gemessen wurde, geht eine 2-sec-
#  Periode über 2/0.15 = 13.3 Messpunkte. Diese Periode ist aber mit Schwankungen
#behaftet. Man sieht sie gut im Zeitreihenplot und im Korrelogramm der
#gewöhnlichen Autokorrelationen.
abline( v = 2*c(0:7)/0.15, col ='black', lwd = 1)
abline( v = 2*seq(0.5,7.5,2)/0.15, col =rgb(0, 0, 1, alpha = 0.1), lwd = 40)
abline( v = 2*seq(1.5,6.5,2)/0.15, col= rgb(1, 0, 0, alpha = 0.1), lwd = 40)

library( forecast)
tsdisplay(d.kraftA)

### AR-Modell schätzen
kraft.burg <- ar.burg(d.kraftA)
kraft.burg
tsdisplay(kraft.burg$resid)
plot(0:24,kraft.burg$aic,type = 'o',main = "AIC",pch = 16 )
abline( v = 1)
mtext(text = 1, side = 1, at = 1, line = 1)

### AR(6) ?ok
res.aic6 <- ar.burg(d.kraftA, order.max = 6, aic = F)$resid
tsdisplay(res.aic6 )

### AR(9) ?ok
res.aic9 <- ar.burg(d.kraftA, order.max = 9, aic = F)$resid
tsdisplay(res.aic9)
library(car); qqPlot(res.aic9)
