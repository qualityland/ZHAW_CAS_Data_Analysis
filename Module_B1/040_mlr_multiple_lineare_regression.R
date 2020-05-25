data.path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B1/data/"
ga <- read.table(paste0(data.path, "Softdrink.dat"), header = TRUE)

fit.ga <- lm(Zeit ~ Menge + Distanz, data = ga)

coef(fit.ga)
# (Intercept)       Menge     Distanz 
#  2.34123115  1.61590721  0.04794942 


library(car)
scatter3d(Zeit ~ Menge + Distanz, data = ga, axis.scale = FALSE)
