
###############################################################################
#############################     Daten laden     #############################
###############################################################################

# data path
data.path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Pruefung_B1/data/"

#library(readxl)
# xls
#df <- read_xls(paste0(data.path, "Alpenquerender_Gueterverkehr_Schiene.xls"))

# csv
#highway <- read.csv(paste0(data.path, 'highway.csv'))
salary <- read.csv(paste0(data.path, "salary.csv"), header=TRUE)
# dat
#windmill <- read.table(paste0(data.path, "Windmuehle.dat"), header = TRUE)

###############################################################################
#############################     Scatterplot     #############################
###############################################################################
plot(
  y ~ experience,
  data = salary,
  col = education,
  pch = education,
  xlim = c(0, 40),
  ylim = c(10000, 150000),
  xlab = "Berufserfahrung",
  ylab = "Jahreslohn"
)
legend(
  "topleft",
  c("Berufslehre", "Maturität", "Hochschulabschluss"),
  col = 1:3,
  pch = 1:3
)




par(mfrow = c(1, 1))
plot(Strom ~ Windgeschwindigkeit, data = windmill,
     main = "Strom vs. Windgeschwindigkeit",
     ylab = "Strom [A]",
     xlab = "Windgeschwindigkeit [km/h]")




###############################################################################
######################     Model & Regressionsgerade     ######################
###############################################################################



# Modell
fit.wm1 <- lm(Strom ~ Windgeschwindigkeit, data = windmill)

# Regressionsgerade
abline(fit.wm1, col = 'red')

###############################################################################
######################       graph. Residualanalyse      ######################
###############################################################################

# ohne Simulationen
par(mfrow = c(1, 3))
plot(fit.wm1, 1:3)

# mit Simulationen
load(paste0(data.path, 'resplot.rda'))
par(mfrow = c(1, 3))
resplot(fit.wm1, 1:3)
