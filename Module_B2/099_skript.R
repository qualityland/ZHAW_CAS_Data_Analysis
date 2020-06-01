
# Chapter 1

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
  