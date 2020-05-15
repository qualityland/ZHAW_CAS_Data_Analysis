#data.path <- "/Users/sschmidt/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B1/data/"
data.path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B1/data/"
ga <- read.table(paste0(data.path, "Softdrink.dat"), header = TRUE)
fit.ga <- lm(Zeit ~ Menge, data = ga)


# Residuenanalyse
# ---------------

# Tukey-Ascombe-, Normal- und Scale-Location-Plot
# (mit und ohne Simulationen)
par(mfrow = c(1,3))
# ohne Simulationen
plot(fit.ga, which = 1:3) 
load(paste0(data.path, "resplot.rda"))
# mit Simulationen
resplot(fit.ga, plots = 1:3)
