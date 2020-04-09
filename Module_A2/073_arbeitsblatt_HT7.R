load("./Module_A2/data/SpeedOfLight.RData")

hist(SpeedOfLight, xlab = "Abweichungen in ns", main = "Lichtgeschwindigkeit")
boxplot(SpeedOfLight)

source("./Module_A2/RFn-qqnormSim.R")

# kompletter Datensatz
qqnormSim(SpeedOfLight)

# Datensatz ohne Ausreisser (k < 0)
qqnormSim(SpeedOfLight[SpeedOfLight > 0])

t.test(SpeedOfLight, conf.level = 0.95)
