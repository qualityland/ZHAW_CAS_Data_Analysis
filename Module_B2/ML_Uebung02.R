path.dat<- '/Users/hofc/idp/Public/Lehre/CAS/CAS_DA/CAS_20_11/Daten/'
# Uebung 2 
# Aufgabe 1

# Gütermenge Schiene
dat.ags <- read.table(paste( path.dat, 'Alpenquerender_Gueterverkehr_Schiene.csv', sep =''), sep=';', header =T)
ts.ags <- ts( na.omit(dat.ags$Verkehr), start = 1984, freq = 1)
par(mfrow = c(2,1))
plot( ts.ags, ylab = "Gütermenge [t]", main  ='Alpenquerender Güterverkehr Schiene')
acf( ts.ags)

# Trend modellieren
par(mfrow = c(2,1))
### loess linear eher besser =>  Argument degree = 1
td.ags <- loess( ts.ags ~ time(ts.ags), family = 'symmetric', degree=1)
plot( ts.ags)
lines( td.ags$x, fitted( td.ags), col = "red")
# Restterm = ZR - Trend
rest <- ts.ags - fitted( td.ags)
rest <- ts( rest, start = 1984, freq = 1)
# Korrelogramm des Restterms
par(mfrow = c(2,1))
plot(rest)
acf(rest)
# in acf var von rest = 1, da inerhlab von acf skaliert 
# ci qnorm(0.975) * / sqrt( length( rest ))
#------------------------------------------------------------
# Inverkehrssetzung Strassenfahrzeuge in der Schweiz
dat.inver <- read.table(paste( path.dat, 'Inverkehrssetzung_Strassenfahrzeuge_Schweiz.dat', sep =''), sep=';', header =T)
head(dat.inver)
ts.inver <- ts( dat.inver$Anzahl , start = c(2000,1), freq = 12 )
par( mfrow = c(2,1))
plot(ts.inver, ylab = 'Anzahl', main = 'Inverkehrssetzung Strassenfahrzeuge in der Schweiz')

# Korrelogramm inver
acf( ts.inver ) #Lag = 1/12 da freq = 12 in der ts-Funktion => eine volle Periode = 1
#acf( as.numeric( ts.inver) ) # Achtung die Lags werden Zeitschritten pro Periode angegeben !!

ts.inver.stl <- stl(ts.inver, s.window = 'periodic')
monthplot(ts.inver.stl)
# ts.inver.stl$win
# oder ts.inver.stl <- stl(ts.inver, s.window = "periodic", t.window = 43)
plot( ts.inver.stl)
head( ts.inver.stl$time.series, n = 2)

# Restterm
rest <- ts.inver.stl$time.series[,3] # 3 Spalte = Restterm 
par( mfrow = c(2,1))
plot( rest)
# Korrelogramm des Restterms
# Achtung lag1 = 1/frequency !!!
#acf( as.numeric( rest ))
acf( rest )
#abline( v = c(1:12)/12, col ="red", lty = 2)
### oder
# rest <- ts( rest, start = 1, freq = 1 )
# acf(rest)

#------------------------------------------------------------
# Rohoelpreisentwicklung in US$
path.dat<- '/Users/hofc/idp/Public/Lehre/CAS/CAS_DA/CAS_20_11/Daten'
load(file.path(path.dat, "Rohoelpreis.rda"))
head(Rohoelpreis)
ts.rp <- ts( Rohoelpreis$Rohoelpreis , start = c(1986,1), freq = 12 )
plot(ts.rp, ylab = 'Preis [US $]', main = 'Rohoelpreis in US $ pro Barrel ~ 160 Liter')
# besser log-Trans., da Varianz nicht konstant
par( mfrow = c(2,1))
plot( log(ts.rp), ylab = 'Preis [US $]', main = 'Rohoelpreis in US $ pro Barrel ~ 160 Liter')
acf( log(ts.rp))

## running mean über ein Jahr 
lrp.rm <- stats::filter( log( ts.rp),  filter = c(0.5, rep(1,11), 0.5)/ 12)
## oder loess
lrp.l <-loess( log( ts.rp ) ~  time(ts.rp), family = 'symmetric') #  default degree = 2 

## Plot-Device in R-Studio muss genügend gross sein
## sonst wird die Legene nicht richtig dargestellt
plot(log(ts.rp))
lines(lrp.l$x, lrp.l$fitted, col = 'red')
lines(lrp.rm,col = 'blue')
legend('topleft',
       legend = c('loess', 'rm'),
       fill = c('red', 'blue'), bty = 'n')


rest.rm <- log( ts.rp ) -lrp.rm
# allenfalls 
# rest.rm <- window(rest.rm, start=c(1991,1), end=c(2007,12))
# rest.rm <- ts( rest, start = c(1986,1), freq = 12 )
par(mfrow = c(2,1))
plot( rest.rm)
acf( rest.rm, na.action = na.pass) # Achtung NA Werte  
###
### oder (loess Glätter)
rest.l <- log( ts.rp ) - lrp.l$fitted
rest.l <- ts( rest.l, start = c(1986,1), freq = 12 )
par(mfrow = c(2,1))
plot( rest.l)
acf( rest.l, na.action = na.pass) # Achtung NA Werte 

### Vergleich der Restterme
plot(rest.l, col ='red')
lines(rest.rm, col = 'blue')
legend('topleft',
       legend = c('loess', 'rm'),
       fill = c('red', 'blue'), bty = 'n')

#------------------------------------------------------------
# Treibstoffpreise in der Schweiz (sehr ähnlich zur ZR der Rohölpreise)
path.dat<- '/Users/hofc/idp/Public/Lehre/CAS/CAS_DA/CAS_20_11/Daten/'
load(file.path(path.dat,'Treibstoffpreise_CH.rda'))
head(Treibstoffpreise_CH)
ts.tsp <- ts( Treibstoffpreise_CH[, c("Diesel", "Benzin")] , start = c(1993,5), freq = 12 ) # mts
par(mfrow = c(2,1))
plot(ts.tsp[,1], main = 'Dieselpreis', ylab = 'Preis') 
# für Diesel (Benzin dito ts.tsp[,2])
acf ( log( ts.tsp[,1]) )


rest <- log( ts.tsp[,1] ) - filter( log( ts.tsp[,1]) , c(0.5, rep(1,11), 0.5)/ 12)
#rest <- ts( rest, start = c(1993,5), freq = 12 ) 
par(mfrow = c(2,1))
plot( rest)
acf( rest, na.action = na.pass) # Achtung NA-Werte am Ende der ZR !!!!


## Aufgabe 2 Konfidenzintervall des Mittelwertes von korrelierten Daten
#------------------------------------------------------------
#------------------------------------------------------------
path.dat<- '/Users/hofc/idp/Public/Lehre/CAS/CAS_DA/CAS_20_11/Daten/'
### i)
## Yield-Daten, Zeitreihenplot
load(file.path(path.dat,"yields.rda"))
par(mar = c(4,4,0.1,0.2))
plot(yields)
## Mittelwert
n <- length( yields)
my <- mean(yields)
abline( h = my, lty = 3, col = "red", lwd = 2)

### Das einzeichnen der Konfidenzintervalle ist kein Prüfungsstoff!!!
## 95%-Konfidenzintervall (falsch, da die Korrelation nicht berücksichtigt)
y0 <-my + -1* qt( 0.975, df = 69) * sd(yields) / sqrt(length(yields))
y1 <- my + 1* qt( 0.975, df = 69) *sd(yields)/sqrt(length(yields))
segments( 71, y0, 71, y1, col = "red",lwd = 2)
segments( 70.5, y0, 71.5, y0, col = "red")
segments( 70.5, y1, 71.5, y1, col = "red")
abline( h = my, lty = 3, col = "red", lwd = 2)
points( 71, my, pch = 16, col ="red", bg = "red")
cat( '95%-CI: [ ' , c(y0, y1), ' ]' )
## ii)
## Abhängigkeitsstruktur
acf( yields, ylim = c( -1,1))
lag.plot(yields, lag = 6, layout=c(2,3), diag.col  = 0, do.lines =F, pch = 16)

### Berücksichtigung der seriellen Korrelation
### Varianz = Gamma(0) (E((X-E(X))^2) Schätzung mit mean((x-mean(x))^2)
gamma0 <- mean((yields-mean(yields))^2) #oder (var(yields)*(n-1))/n
gamma0
### argument type = 'covariance', Kovarianzen werden zurückgegeben
acf(yields,type='covariance',plot=F)$acf

### Kovarianzen (Korrelation > 0)
acf(yields)
roh <- acf(yields,plot=F)$acf[2:4] # 2:4 = lags 1:3

### iii)
### Varianz des Mittelwertes
var.hat.mu <- 1/(n^2) * gamma0* (n + 2*(n-1)*roh[1]+2*(n-2)*roh[2]+2*(n-3)*roh[3])

plot(yields)
abline( h = my, lty = 3, col = "red", lwd = 2)
### 95 -CI ohne berücksichtigte Autokorrealation
segments( 71, y0, 71, y1, col = "red",lwd = 2)
segments( 70.5, y0, 71.5, y0, col = "red")
segments( 70.5, y1, 71.5, y1, col = "red")
points( 71, my, pch = 16, col ="red", bg = "red")
cat( '95%-CI: [ ' , c(y0, y1), ' ]' )
### 95 -CI Autokorrealation berücksichtigt
y0.2 <-my  -1* qt( 0.975, df = n-1) * sqrt( var.hat.mu)
y1.2 <- my + 1* qt( 0.975, df = n-1) *sqrt( var.hat.mu)
segments( 72, y0.2, 72, y1.2, col = "blue",lwd = 2)
segments( 71.5, y0.2, 72.5, y0.2, col = "blue")
segments( 71.5, y1.2, 72.5, y1.2, col = "blue")
points( 72, my, pch = 16, col ="blue", bg = "blue")
cat( '95%-CI: [ ' , c(y0.2, y1.2), ' ]' )

###
### iv) 
###
par( mfrow = c(1,2))
acf( yields, ylim = c(-1,1))
pacf( yields, ylim = c(-1,1)) # Achtung Lag 0 nicht dargestellt
library( forecast) # Lag 0 nicht dargestellt
tsdisplay( yields )


### Aufgabe 3 
##########################################################
##########################################################
par(mfrow = c(3,3))
series1 <- arima.sim( 100, model = list( ar = -0.75))
series2 <- arima.sim( 100, model = list( ar = c(0.9, -0.3, 0.2, -0.4, 0.3, -0.3)))
series3 <- arima.sim( 100, model = list( ar = 0.24, -0.53, -0.73))

plot( series1)
legend( "topleft",legend = "1", bty = "n")
acf( series2 )
legend( "topleft",legend = "I", bty = "n")
pacf( series2 )
legend( "topleft",legend = "A", bty = "n")
plot( series2)
legend( "bottomleft",legend = "2", bty = "n")
acf( series1 )
legend( "topleft",legend = "II", bty = "n")
pacf( series3 )
legend( "topleft",legend = "B", bty = "n")

plot( series3)
legend( "topleft",legend = "3", bty = "n")
acf( series3 )
legend( "topleft",legend = "III", bty = "n")
pacf( series1 )
legend( "bottomright",legend = "C", bty = "n")

