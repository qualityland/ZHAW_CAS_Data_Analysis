#data.dir <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B2/data/"
data.dir <- "C:/Users/SCHMIS1M/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B2/data/"



# Kapitel 1

## Flugbuchungen (PanAm), monatlich in 1'000
data("AirPassengers")
class(AirPassengers)  # ts object
str(AirPassengers)    # 144 Beobachtungen (12 Jahre, monatlich, 1949 - 1960)
AirPassengers

plot(AirPassengers, main = 'Passenger Bookings', ylab = 'Pax')


## Lynx Trappings
data(lynx)
class(lynx)
str(lynx)
lynx

plot(lynx, main = 'Lynx Trappings', ylab = '# of lynx trapped')


## Luteinisierendes Hormon
data(lh)
class(lh)
str(lh)
lh

plot(lh, main = 'Luteinizing Hormone', ylab = 'LH level')
# lagged Scatterplot
n <- length(lh)
plot(lh[1:(n-1)], lh[2:n], main = 'Scatterplot of LH Data with Lag 1', pch = 16)


# Pearson Korrelation
cor(lh[1:(n-1)], lh[2:n])
# [1] 0.5807322

## Swiss Market Index

data("EuStockMarkets")
head(EuStockMarkets)        # multiples Zeitreihenobjekt (DAX, SMI, CAC, FTSE)

esm <- EuStockMarkets
tmp <- esm[, 2]
smi <- ts(tmp, start = start(esm), frequency = frequency(esm))
class(smi)

plot(smi, main = 'SMI Daily Closing Value')

# Log-Returns
n <- length(smi)
tmp <- log(smi[2:(n)]) - log(smi[1:(n-1)])
lret.smi <- ts(tmp, start = start(esm), frequency = frequency(esm))
plot(lret.smi, main = 'SMI Log-Returns', ylab = 'SMI Log-Returns')



# Kapitel 3 - Zeitreihenanalyse mit R

## die Klasse ts (time series)

# Anzahl Stautage pro Jahr (vor Nordportal des Gotthard-Tunnels)
rawdat <- c(88, 76, 112, 109, 91, 98, 139)
ts.dat <- ts(rawdat, start = 2004, frequency = 1)
ts.dat
str(ts.dat)

# nuetzliche Funktionen im Umgang mit ts Objekten
start(ts.dat)
end(ts.dat)
frequency(ts.dat)
deltat(ts.dat)
time(ts.dat)                              # returns
window(ts.dat, start = 2006, end = 2008)  # subsetting returning ts object
ts.dat[3:5]                               # subsetting returning numeric vector

plot(ts.dat, ylab = 'Anzahl Stautage pro Jahr', main = 'Stautage vor dem Gotthard-Tunnel')


## das forecast Package
library(forecast)
data(lh)
# display ts, ACF and PACF
tsdisplay(lh, main = "Luteinizing Hormone")

## Zeit und Datum
as.Date('2012-02-14')
as.Date('2012/02/07')
# Format
# %d - Day of the month (decimalnumber)
# %m - Month (decimal number)
# %b - Month (character, abbreviated)
# %B - Month (character, full name)
# %y - Year (decimal, two digit)
# %Y - Year (decimal, four digit)
as.Date('27.01.12', format = '%d.%m.%y')
as.Date('14. February, 2012', format = '%d. %B, %Y')

# internally stored as number of days since epoch (01.01.1970)
mydat <-as.Date('2012-02-14')
as.numeric(mydat)
# ein Datum vor dem 01.01.1970 ist negativ (intern)
jesus <- as.Date('0000-01-01')
as.numeric(jesus)

# Wochentag eines Datums
weekdays(mydat)
# [1] "Tuesday"

# Monat eines Datums
months(mydat)
# [1] "February"

# Quartal eines Datums
quarters(mydat)
# [1] "Q1"

## Rechnen mit Datum
dat <- as.Date(c('2000-01-01', '2004-04-04', '2007-08-09'))
dat + 1
# [1] "2000-01-02" "2004-04-05" "2007-08-10"
min(dat)
# [1] "2000-01-01"
max(dat)
# [1] "2007-08-09"
median(dat)
# [1] "2004-04-04"
dat[3] - dat[1]
# Time difference of 2777 days

## Zeitsequenzen
# Sequenz 12 aufeinander folgender Tage ab dem 3. August 1985
seq.aug <- seq(as.Date('1985-08-03'), by = 'days', length = 12)
seq.aug
# 12 Zeitpunkte im 2-Wochen-Abstand am dem 17. April 1992
seq.2w <- seq(as.Date('1992-04-17'), by='2 weeks', length=12)
seq.2w

## chron Package
library(chron)
dat <-
  c(
    '2007-06-09 16:43:20',
    '2007-08-29 07:22:40',
    '2007-10-21 16:48:40',
    '2007-12-17 11:18:50'
  )
dts <- substr(dat, 1, 10)    # dates
tme <- substr(dat, 12, 19)   # times
fmt <- c('y-m-d', 'h:m:s')
cdt <- chron(dates = dts, time = tme, format = fmt)
cdt
# nun kann man rechnen
cdt[2] - cdt[1]
# Time in days:
# [1] 80.61065
difftime(cdt[2], cdt[1], units = "secs")
# Time difference of 6964760 secs



# Kapitel 4 - Deskriptive Zeitreihenanalyse

# Zeitreihenplot
dat <- read.table(paste0(data.dir, 'Maine.dat'), header = TRUE)
tsd <- ts(dat, start = c(1996, 1), frequency = 12)                      # start 01/1996
plot(tsd, ylab = "%", main = "Unemployment in Maine")

load(paste0(data.dir, 'cbe.rda'))
dat <- cbe
tsd <- ts(dat, start = 1958, frequency = 12)
plot(tsd, main = "Chocolate, Beer & Electricity")
