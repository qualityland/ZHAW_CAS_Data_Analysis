
### Aufgabe 1 -----------------------------------------------------------------
uhren <- read.table("./Module_B1/data/AntikeUhren.dat", header = TRUE)

### a)
# Scatterplot von Preis ~ Alter
plot(Preis ~ Alter, data = uhren)

# angepasste Gerade:
fit.uhren <- lm(Preis ~ Alter, data = uhren)
abline(fit.uhren, col = "red")

# Koeffizienten:
coef(fit.uhren)

# geschätzte Geradengleichung:
# Preis = -191.66 + 10.48 * Alter


### b)

## (1) mit Hilfe von summary():
summary(fit.uhren)
# Ja, das Alter hat einen signifikanten Einfluss (5% Signifikanz-Niveau):
# p < 2.1e-06 ***


## (2) manuell:
hat_beta <- coef(fit.uhren)[2]          # geschaetztes beta
ssx <- sum((uhren$Alter - mean(uhren$Alter))^2)
se_beta <- sqrt(summary(fit.uhren)$sigma^2 / ssx)
T <- hat_beta / se_beta
T
# to be continued...


### c)
confint(fit.uhren, level = 0.99)
# Ja, Preiserhoehungen zwischen $ 5.55 und $ 15.40 sind plausibel (1% Niveau)


### d)
