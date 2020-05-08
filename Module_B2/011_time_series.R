## Air Passenger Bookings 1949 - 1960 (PanAm)

data("AirPassengers")
str(AirPassengers)
plot(AirPassengers, ylab='Pax', main='Passenger Bookings')

## Lynx Trappings 1824 - 1934
data(lynx)
plot(lynx, ylab='# of Lynx Trapped', main='Lynx Trappings')


## Lutropin
data(lh)
lh
plot(lh, ylab='LH level', main='Luteinizing Hormone')
# Autokorrelation
plot(lh[1:47], lh[2:48],pch=20, main  ='Scatterplot of LH Data with Lag 1')
# Pearson-Korrelation
cor(lh[1:47], lh[2:48])


## Swiss Market Index
data(EuStockMarkets)
head(EuStockMarkets)
plot(EuStockMarkets, main='EU Stock Markets - Daily Closing Value')
# extract SMI
esm <- EuStockMarkets
tmp <- EuStockMarkets[,2]
smi <- ts(tmp, start=start(esm), freq=frequency(esm))
plot(smi, main='SMI Daily Closing Value')
# Log-Returns
tmp <- log(smi[2:1860])-log(smi[1:1859])
lret.smi <- ts(tmp, start=c(1991,131), freq=frequency(esm))
plot(lret.smi, main='SMI Log-Returns', ylab = "SMI Log-Returns")



