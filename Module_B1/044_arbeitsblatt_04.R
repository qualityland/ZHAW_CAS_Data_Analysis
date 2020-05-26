
# Aufgabe 1 - Advertisement
# -------------------------

# Daten laden
data.path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B1/data/"
load(paste0(data.path, "Advertisement.rda"))
str(adv)

# Model anpassen
fit.adv <- lm(sales ~ ., data = adv)
summary(fit.adv)
# Call:
#   lm(formula = sales ~ ., data = adv)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -2.9257 -0.6216  0.1613  0.6923  2.9996 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.352150   1.649721  -1.426    0.156    
# TV           0.046382   0.003162  14.669  < 2e-16 ***
# radio        0.079783   0.011970   6.665 2.62e-10 ***
# newspaper    0.021015   0.015354   1.369    0.173    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.135 on 196 degrees of freedom
# Multiple R-squared:  0.6311,	Adjusted R-squared:  0.6254 
# F-statistic: 111.8 on 3 and 196 DF,  p-value: < 2.2e-16

# Vertrauensintervalle auf 99% Nievau
confint(fit.adv, level = 0.99)
#                   0.5 %     99.5 %
# (Intercept) -6.64331323 1.93901253
# TV           0.03815706 0.05460639
# radio        0.04864593 0.11091970
# newspaper   -0.01892179 0.06095199

# Prognose-Intervall
x0 <- data.frame(TV=150, radio=40, newspaper=100)
predict(fit.adv, newdata = x0, interval = 'prediction', level = 0.95)
#       fit      lwr      upr 
# 1 9.89793 7.650372 12.14549
# mittlere Verkaufszahl: 9897 Artikel
# mit 95% Wahrscheinlichkeit zwischen 7650 und 12145 Artikeln.

# t-Test
summary(fit.adv)
# ...
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.352150   1.649721  -1.426    0.156    
# TV           0.046382   0.003162  14.669  < 2e-16 ***
# radio        0.079783   0.011970   6.665 2.62e-10 ***
# newspaper    0.021015   0.015354   1.369    0.173    
summary(fit.adv)$coefficients
#                Estimate Std. Error   t value     Pr(>|t|)
# (Intercept) -2.35215035 1.64972054 -1.425787 1.555205e-01
# TV           0.04638172 0.00316194 14.668755 2.294310e-33
# radio        0.07978281 0.01197045  6.664979 2.620505e-10
# newspaper    0.02101510 0.01535358  1.368743 1.726463e-01
# (p-value nicht so leserlich wie aus summary(fit.adv)!)

# 3D Plot
library(car)
scatter3d(sales ~ TV + radio, data = adv, axis.scales = FALSE)


# Aufgabe 2 - Katheter
# --------------------

# Daten laden
data.path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B1/data/"
catheter <- read.csv(paste0(data.path, 'catheter.dat'))

# (a)
# Boxplots
par(mfrow = c(1,3))
boxplot(catheter$Groesse, main = "Patientengroesse [cm]")
boxplot(catheter$Gewicht, main = "Patientengewicht [kg]")
boxplot(catheter$y, main = "Katheterlaenge [cm]")

# Scatterplots
pairs(catheter)

# (b)
# einfache lineare Regressionen
fit.cat1 <- lm(y ~ Groesse, data = catheter)
coef(fit.cat1)
# (Intercept)     Groesse 
# -21.8911291   0.4922262 

summary(fit.cat1)$sigma
# [1] 4.008547

fit.cat2 <- lm(y ~ Gewicht, data = catheter)
coef(fit.cat2)
# (Intercept)     Gewicht 
#   2.9532420   0.3727923 

summary(fit.cat2)$sigma
# [1] 3.79671

# (c)
summary(fit.cat1)$coefficients
#                Estimate Std. Error   t value     Pr(>|t|)
# (Intercept) -21.8911291 9.92649209 -2.205324 0.0519705788
# Groesse       0.4922262 0.08352515  5.893149 0.0001524578

summary(fit.cat2)$coefficients
#              Estimate Std. Error   t value     Pr(>|t|)
# (Intercept) 2.9532420 5.38015253 0.5489142 5.951073e-01
# Gewicht     0.3727923 0.05904723 6.3134598 8.754745e-05

# (d)
fit.cat3 <- lm(y ~ Groesse + Gewicht, data = catheter)
summary(fit.cat3)
# F-Test
# F-statistic: 18.65 on 2 and 9 DF,  p-value: 0.0006301
# ==> H0: keine der Variablen hat einen Einfluss auf y
#     muss auf 95% Niveau verworfen werden.
# t-Test
# bei den t-Tests koennen wir H0: beta1 = 0 bzw. beta2 = 0
# auf 95% Niveau nicht verwerfen (p-Value > 0.05!)

# (e)
summary(fit.cat1)$sigma
# [1] 4.008547

summary(fit.cat2)$sigma
# [1] 3.79671

summary(fit.cat3)$sigma
# [1] 3.940388

# ==> keine Verbesserung der Fehlervarianz