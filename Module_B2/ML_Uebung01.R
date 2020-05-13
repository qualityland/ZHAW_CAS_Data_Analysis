############################################################################################
### Aufgabe 1 und 2
############################################################################################
#path.dat<- '~/idp/Public/Lehre/CAS/CAS_DA/CAS_19_10/Daten/'
path.dat<- '/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B2/data/'
# Gütermenge
# xls-File als csv-file abspeichern
t.file <- paste(path.dat, 'Alpenquerender_Gueterverkehr_Schiene.csv', sep ='')
dat.ags <- read.table(file = t.file, sep=';', header =T)
head(dat.ags)
ts.ags <- ts( dat.ags$Verkehr, start = 1984, freq = 1)
plot( ts.ags, ylab = "Gütermenge [t]", main  ='Alpenquerender Güterverkehr Schiene')
###
### oder Daten direkt aus dem ExcelFile mit readxl Package
###
if(F){
  library(readxl) # readxl muss installiert sein
  dat.ags <- read_xls(paste( path.dat, 'Alpenquerender_Gueterverkehr_Schiene.xls', sep ="" ))
  dat.ags <- as.data.frame(dat.ags)
  ts.ags <- ts( dat.ags$Verkehr, start = 1984, freq = 1)
  plot( ts.ags, ylab = "Gütermenge [t]", main  ='Alpenquerender Güterverkehr Schiene')
}
# Inverkehrssetzung Strassenfahrzeuge in der Schweiz
### Achtung: fileEncoding = 'latin1' # Spiel nur eine Rolle falls OSX im Einsatz ist
dat.inver <- read.table(paste( path.dat, 'Inverkehrssetzung_Strassenfahrzeuge_Schweiz.dat', sep =''), sep=';', header =T, fileEncoding ='latin1' )
head(dat.inver)
ts.inver <- ts( dat.inver$Anzahl , start = c(2000,1), freq = 12 )
plot(ts.inver, ylab = 'Anzahl', main = 'Inverkehrssetzung Strassenfahrzeuge in der Schweiz')

# Rohoelpreisentwicklung in US$
load(paste( path.dat, "Rohoelpreis.rda", sep = ''), )
head(Rohoelpreis)
### Falls Mär als M\xe4r angezeigt wird
### Umlaut Encoding mit ä ersetzen (hat keinen Einfluss auf die Darstellung)
Rohoelpreis$Monat <- as.character(Rohoelpreis$Monat)
Encoding(Rohoelpreis$Monat) <- "latin1"
ts.rp <- ts( Rohoelpreis$Rohoelpreis , start = c(1986,1), freq = 12 )
plot(ts.rp, ylab = 'Preis [US $]', main = 'Rohoelpreis in US $ pro Barrel ~ 160 Liter')

# Treibstoffpreise in der Schweiz
load(paste( path.dat, 'Treibstoffpreise_CH.rda', sep = ''))
head(Treibstoffpreise_CH)
ts.tsp <- ts( Treibstoffpreise_CH[, c("Diesel", "Benzin")] , start = c(1993,5), freq = 12 ) # mts
plot(ts.tsp, main = 'Diesel- und Benzinpreis')
### oder in einem Plot: arg plot.type = 'single', da y-Achse gleiche Skala aufweist
plot(ts.tsp, main = 'Diesel- und Benzinpreis', plot.type = 'single', col= c("red", "blue"), ylab = "CHF pro Liter")
legend( x = 'topleft', legend = c('Diesel', 'Benzin'), col= c('red', 'blue'), lty = 1, bty = 'n')

# ts.union (fill with NAs)
ts.rdb.u <- ts.union(ts.rp, ts.tsp) 
ts.rdb.u.n <- ts( ts.rdb.u , names = c("Rohöl", "Diesel", "Benzin"), start = c(1986.1), freq = 12)
### colnames(ts.rdb.u.n) <- c("Rohöl", "Diesel", "Benzin")
plot(  ts.rdb.u.n, main = 'Rohoel-, Diesel- und Benzinpreis')

# ts.intersect
ts.rdb.is <- ts.intersect(ts.rp, ts.tsp)
ts.rdb.is.n <- ts( ts.rdb.is , names = c("Rohöl", "Diesel", "Benzin"), start = c(1993.5), freq = 12)
plot(  ts.rdb.is.n, main = 'Rohoel-, Diesel- und Benzinpreis' )

# Indexierte Preise
ts.rp.s <- window( ts.rp, start = c(1993,5) ) # ZR der Treibstoffpreisse beginnen Mai 1993
ts.rp.s.i <- 100 * ts.rp.s/ts.rp.s[1]
ts.dp.i <- 100 * ts.tsp[,1]/ts.tsp[1,1]
ts.bp.i <- 100 * ts.tsp[,2]/ts.tsp[1,2]
plot( ts.rp.s.i, ylab = 'Index')
lines( ts.dp.i, col = "red")
lines( ts.bp.i, col = "blue")
title('Indexierte Preise: Rohöl, Benzin, Diesel')
legend('topleft', legend = c('Rohoel', 'Diesel', 'Benzin'), col=c('black', 'red', 'blue'), lty = 1, bty = "n")
### oder
m <- cbind(ts.rp.s.i, ts.dp.i, ts.bp.i)
plot(m, plot.type = 'single', col=c('black', 'red', 'blue'), ylab = 'Index')
title('Indexierte Preise: Rohöl, Benzin, Diesel')
legend('topleft', legend = c('Rohoel', 'Diesel', 'Benzin'), col=c('black', 'red', 'blue'), lty = 1, bty = "n")

############################################################################################
### Aufgabe 3
############################################################################################
# Da die Reihen zum Güterverkehr, zum Rohöl- und den Treibstoffpreisen keine saisonalen
# Komponenten aufweisen, können wir hier 
# Differenzen zum Lag 1 bilden. Die Resultate sind wie folgt:

# Daten Differenzen Güter
par(mfrow = c(2,1))
plot(ts.ags)
?diff # default = X[t] - X[t-1]
plot( diff( ts.ags) ) 
# Daten Rohoel, Diesel und Benzin 
plot(ts.rdb.is.n)
plot( diff( ts.rdb.is.n))
### besser log (log stabilisiert Varianz)
plot(log( ts.rdb.is.n ))
plot( diff( log(ts.rdb.is.n)))
# Inverkehrssetzung CH (besser decompose oder stl)
par(mfrow = c(2,1))
plot(ts.inver )
plot( diff(ts.inver, lag = 12), main = "Invverkehrssetzung: Differenzen Lag = 12")

# Zerlegung mit decompose() oder stl()
# funktioniert nur bei ZR mit einer Saisonkomponente
ts.inver.dec <- decompose(  ts.inver )
plot( ts.inver.dec)
str( ts.inver.dec)

### Vergleich diff, decompse
par(mfrow = c(1,2))
plot( diff(ts.inver, lag = 12), main = "Invverkehrssetzung: Differenzen Lag = 12")
plot(ts.inver.dec$random, main = "Invverkehrssetzung: Remainder von decompose()")

### nur Trend keine Saisonkomponente filtern oder glätten
### filter() oder z.B. loess mit funktion loess.smooth
### Beispiel: Güterverkehr

# filter
###
### Demo 2 HT
###
### Problem: fehlende Werte am Anfang und Ende der ZR
### -> loess
### Alpenquerender_Gueterverkehr_Schiene ts.ags
trend.f5 <- filter( ts.ags, filter = rep(1,5)/5, side = 2)
rest.f <- ts.ags- trend.f5
# loess
xl <- time( ts.ags)  
lo.ags <- loess( ts.ags ~ xl, span = 0.75, family = 'symmetric', degree = 1)
rest.l <- ts( ts.ags- lo.ags$fitted, start = c(1984,1), freq = 1)

# plot ZR inkl. Glätter
par( mfrow = c(1,2))
plot( ts.ags)
# plot filter
lines( trend.f5, col = 'red')
# plot loess
lines( as.numeric(xl), lo.ags$fitted , col = "blue") # xl ist ein ts-Objekt, lines kann nicht damit umgehen daher, as.numeric
# legend
legend('topleft', legend = c('filter', 'loess'), col=c('red', 'blue'), lty = 1, bty = "n")
## plot Resterm
plot( rest.f , mai = "Restterm", col = "red", ylim = c(-2,2))
lines( rest.l , mai = "Restterm", col = "blue")
legend('topleft', legend = c('filter', 'loess'), col=c('red', 'blue'), lty = 1, bty = "n")
###
### Beispiel: Rohoel (Diesl und Benzin analog)
###

par( mfrow = c(1,2))
plot( log(ts.rp), main = 'log(Rohölpreis) mit Trendschätzung durch filter()')
legend('topleft', legend = c('filter', 'loess'), col=c('red', 'blue'), lty = 1, bty = "n")
# filter
trend.f <- filter(log(ts.rp), filter = c(0.5, rep(1,11), 0.5)/ 12) #    c(rep(1, 101)/101))
lines(trend.f, col="red")
rest.f <- log( ts.rp)- trend.f
# loess
xl <- as.numeric( time( ts.rp) ) # x-Werte müssen für lines(... ) numerisch sein
lo.lrp <- loess( log( ts.rp) ~ xl, span = 0.75, family = 'symmetric', degree = 1) # span = 0.75 = default oder 0.1
lines( xl, lo.lrp$fitted , col = "blue")
rest.l <- ts(log(ts.rp)- lo.lrp$fitted, start = c(1986,1), freq = 12)

### plot Restterm
plot( rest.f , mai = "Restterm", col = 'red')
legend('topleft', legend = c('filter', 'loess'), col=c('red', 'blue'), lty = 1, bty = "n")
lines( rest.l , mai = "Restterm", col = 'blue')
abline(h = 0, lty = 2)
############################################################################################
### Aufgabe 4
############################################################################################

stl.fit <- stl( ts.inver, s.window = 'periodic')
plot( stl.fit)
### monthplot( stl.fit)
head( stl.fit$time.series )
# stl.fit$time.series[, "seasonal"]
# stl.fit$time.series$seasonal[,3]
stl.fit$win  
# stl.fit$win["t"] t sollte ungerade sein t.window
stl.fit <- stl( ts.inver, s.window = 'periodic' , t.window = 43)
plot( stl.fit)

head( stl.fit$time.series )

### Aufgabe 5 a)
hstart <- read.table(paste( path.dat, 'hstart.dat', sep =''), sep=';', header =F)
head( hstart)
hstart <- ts( hstart[,1], start = c( 1966,1), freq = 12)
plot( hstart)
h.stl <- stl( hstart, s.window = 'periodic')
h.season <- h.stl$time.series[,1]
hstart.ds <- hstart - h.season ### Saisonbereinigung
plot( hstart.ds, main= "Saisonbereinigte Daten\nWohnungsbaubeginn pro Monat in den USA")


### Aufgabe 5 b)
f1 <- filter( hstart.ds, rep( 1,5)/5)
f2 <- filter( f1, rep( 1,5)/5)
f3 <- filter( hstart.ds, c( 1:4,5,4:1)/25)
f4 <- filter( hstart.ds, rep( 1,9)/9)
### Aufgabe 5 c) Filter 3 und 4
lines( f3, col = "blue")
lines( f4, col = "red")
legend("topleft", legend = c("f3", "f4"), col = c("blue", "red"), lty = 1,lwd = 2)

### Aufgabe 5 d)
plot( hstart.ds, main= "Saisonbereinigte Daten")
lines( f2, col = "blue", lty = 1, lwd = 3)
lines( f3, col = "red", lty = 3, lwd = 3)

### oder
round( sum(f2-f3, na.rm  =T),10)

