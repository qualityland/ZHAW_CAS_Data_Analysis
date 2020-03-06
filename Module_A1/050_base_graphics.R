### Wednesday, 04.03.2020

# Slides:
#   part3.pdf
#   baseGraphics.html
#   baseGraphics_beispiel.html

############################# bla #############################
# lookup: Simpson-Paradoxon


data(Prestige, package = "carData")

str(Prestige)
dim(Prestige)
head(Prestige)
class(Prestige$type)

# Education (in years)
hist(Prestige$education)

# nicer
par(mar = c(4, 4, .3, .3))

#
hist(Prestige$education,
     xlab = "education (in years)",
     ylab = "Anzahl",
     main = "",
     col = "skyblue",
     xaxt = "n")

# Schliessen des Grafik-Devices und
# Zuruecksetzen der Grafik-Parameter mit:
dev.off()

# histogramm
hist(Prestige$income, breaks = 15)
# Zielvariable logarithmisch
hist(log(Prestige$income), breaks = 15)

# boxplot
boxplot(Prestige$income)

# boxplot(Zielvariable ~ Gruppierungsvariable, data = DatenQuelle)
boxplot(income ~ type, data = Prestige)
boxplot(education ~ type, data = Prestige)

# pie chart
pie(table(Prestige$type))

# bar chart
barplot(table(Prestige$type))

plot(Prestige$type)
methods(plot)

# rel. Haeufigkeit
barplot(prop.table(table(Prestige$type)))
# oder:
barplot(table(Prestige$type) / nrow(Prestige))

# Scatterplot
# Formelschreibweise in R:
# (Erster Variablenname ~ Zweiter Variablenname, data = DataSource)
plot(prestige ~ income, data = Prestige)
# mit roten Punkten
plot(prestige ~ income, data = Prestige,
     col = "red", pch = 20)

# diese print character gibt es:
plot(1:30, rep(1, 30), pch = 1:30)

# alle bivariaten Vergleiche mit pairs()
pairs(Prestige)
pairs(Prestige[, c(4, 1:3, 6)])


# Mosaicplot
# anderer Datensatz
# wir benoetigen mehr als eine kategorische (factor) Variable
data(Cars93, package = "MASS")
library(dplyr)
Cars93 %>% 
  select(DriveTrain, Origin, Type) %>% 
  select(DriveTrain, Origin, Type) %>% 
  summarise(n = n())

mosaicplot(table(Cars93[, c("Type", "DriveTrain", "Origin")]),
           main = "")

# oder mit package 'vcd'
library(vcd)
mosaic(table(Cars93[, c("Type", "DriveTrain", "Origin")]),
           main = "")


# Korrelation
cor(Prestige$education, Prestige$prestige) # default: method = "pearson"

# robust:
#install.packages("robustbase")
library(robustbase)

covMcd(Prestige[, c("education", "prestige")],
       cor = TRUE)$cor
# Korrelations-Matrix
# nach Pearson
cor(Prestige %>% select(-type))  # remove type (not numeric!)

# robust
covMcd(Prestige %>%  select(-type), cor = TRUE)$cor

# Spearman-Korrelation (normalerweise nur auf ordinalen Daten)
cor(Prestige %>% select(-type), method = "spearman")
