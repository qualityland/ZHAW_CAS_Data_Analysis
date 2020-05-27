
data.path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Pruefung_B1/data/"
load(paste0(data.path, "Pruefung.rda"))


str(zueri)


# (a)
# Modell
fit.zh <- lm(Miete ~ m2, data = zueri)

# Koeffizienten
coef(fit.zh)

# Plot
par(mfrow = c(1, 1))
plot(Miete ~ m2, data = zueri)
abline(fit.zh, col = 'red')

# F-Test und t-Test
summary(fit.zh)


# Prognose
x0 <- data.frame(m2 = 100)
m100 <- predict(fit.zh, newdata = x0, interval = "prediction", level = 0.90)


# 
fit.zh2 <- lm(Miete ~ m2 + Zimmer, data = zueri)
summary(fit.zh2)



#### Aufgabe 2

# (a)
str(wine)

wine$LogPrice <- log(wine$price)
par(mfrow = c(1, 1))
plot(LogPrice ~ points, data = wine)

wine$LogPrice <- log(wine$price)
par(mfrow = c(1, 1))
plot(LogPrice ~ points, data = wine, main = "Logarithmierter Flaschenpreis vs. Rating")
plot(price ~ points, data = wine, main = "Orignal Flaschenpreis vs. Rating")
fit.wine <- lm(LogPrice ~ points, data = wine)
abline(fit.wine, col = 'red')


# (b)
par(mfrow = c(1, 3))
plot(fit.wine, 1:3)
load(paste0(data.path, "resplot.rda"))
resplot(fit.wine, 1:3)

par(mfrow = c(1, 1))
plot(price ~ points, data = wine)
x <- 0:100
lines(x, exp(coef(fit.wine)[1] + coef(fit.wine)[2] * x), col = 'red')


# (c)
# done

# (d)
wine$lprice <- log(wine$price)
wine$qpoints <- wine$points^2
fit.wine2 <- lm(lprice ~ points + taster_name + qpoints +
                  variety + country, data = wine)
# Schnaeppchenwein
wine[residuals(fit.wine2) == min(residuals(fit.wine2)), ]
which(wine[residuals(fit.wine2) == min(residuals(fit.wine2)), ])

fit.wine2$fitted.values[7654]

wine[7654,]
summary(residuals(fit.wine2))


# (f)
x0 = data.frame(points=92, taster_name="Roger Voss", country="Austria", variety="Riesling", qpoints=92^2)
exp(predict(fit.wine2, newdata = x0, interval = 'prediction', level = 0.90))
