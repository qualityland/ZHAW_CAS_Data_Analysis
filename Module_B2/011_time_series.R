## Air Passenger Bookings 1949 - 1960 (PanAm)

data("AirPassengers")

# time-series
str(AirPassengers)

# plot
plot(AirPassengers, ylab='Pax', main='Passenger Bookings')

# ggplot
#ggplot(AirPassengers, aes(variable, value)) + geom_line() + xlab("") + ylab("")

## Lynx Trappings 1824 - 1934
data(lynx)

plot(lynx)


## L