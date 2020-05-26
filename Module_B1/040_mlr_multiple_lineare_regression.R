data.path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B1/data/"
ga <- read.table(paste0(data.path, "Softdrink.dat"), header = TRUE)

fit.ga <- lm(Zeit ~ Menge + Distanz, data = ga)

coef(fit.ga)
# (Intercept)       Menge     Distanz 
#  2.34123115  1.61590721  0.04794942 


library(car)
scatter3d(Zeit ~ Menge + Distanz, data = ga, axis.scale = FALSE)

# angepasste Werte
fitted(fit.ga)

# Residuen
resid(fit.ga)

# standard error
summary(fit.ga)$sigma
#[1] 3.259473

# Bestimmtheitsmass
summary(fit.ga)$r.squared
#[1] 0.9595937

# korrigiertes Bestimmtheitsmass (adjusted R2)
summary(fit.ga)$adj.r.squared
#[1] 0.9559205


# Vertrauensintervall
confint(fit.ga)
#                  2.5 %     97.5 %
# (Intercept) 0.06675199 4.61571030
# Menge       1.26182466 1.96998976
# Distanz     0.02297248 0.07292636

# Vorhersagen
x0 <- data.frame(Menge=10, Distanz=50)
predict(fit.ga, newdata = x0)
#        1 
# 20.89777 

# Vertrauensintervall
predict(fit.ga, newdata = x0, interval = 'confidence', level = 0.95)
#        fit      lwr      upr
# 1 20.89777 18.32074 23.47481

# Prognose-Intervall
predict(fit.ga, newdata = x0, interval = 'prediction', level = 0.95)
#        fit      lwr      upr
# 1 20.89777 13.66347 28.13208

