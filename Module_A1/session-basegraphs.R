library(carData)
data("Prestige")

############################# str, dim, head ##################################
str(Prestige)
View(Prestige)
dim(Prestige)

# Erste 2 Zeilen 
head(Prestige, 2)

# Wie ist type abgespeichert? (Faktor!)
class(Prestige$type)

############################# Histogram #######################################

# histogram:
hist(Prestige$education)

# schoener:
# margins (unten, links, oben, rechts)
#par(mar = c(4,4,0.3,0.3)) 

# histogram
hist(Prestige$education, 
     xlab = "education",
     ylab = "Anzahl",
     main = "",
     col = "skyblue")
# Sonderwunsch, andere labels
hist(Prestige$education, 
     xlab = "education",
     ylab = "Anzahl",
     main = "",
     col = "skyblue",
     xaxt = "n")
axis(side = 1, 
     at = 6:16,
     labels = 6:16)
# schliesse das Graphik-Device mit
#dev.off()

hist(Prestige$income, breaks = 15)
hist(log(Prestige$income), breaks = 15)


############################# Boxplot #########################################

# boxplot
boxplot(Prestige$income)

# boxplot nach Gruppen

# Formelschreibweise von R:
# boxplot(Zielvariable ~ Gruppierungsvariable, 
#   data = meinDataFrame)
#par(mar = c(4,4,0.3,0.3)) 
boxplot(income ~ type, data = Prestige)
boxplot(education ~ type, data = Prestige)

# Sonderwunsch, mehrere Graphiken in einem:
?par
#par(mfrow = c(1, 2))
boxplot(income ~ type, data = Prestige)
boxplot(education ~ type, data = Prestige)
#dev.off()

############################# Pie Chart #######################################

# pie chart
pie(table(Prestige$type))
barplot(table(Prestige$type))

############################# rel. Haeufigkeit ################################

# relative Haeufigkeit
nrow(Prestige)
table(Prestige$type)                    # abs. Haeufigkeit
table(Prestige$type) / nrow(Prestige)   # rel. Haeufigkeit


############################# Bar Plot ########################################
# rel. Haeufigkeit
barplot(table(Prestige$type) / nrow(Prestige))

# oder (hier: abs. Haeufigkeit)
plot(Prestige$type)


############################# Scatter Plot ####################################

# Scatterplot:
# Formelschreibweise in R:
# (Erster Variablenname ~ Zweiter Variablenname, 
#  data = meinDataframe)

#par(mar = c(4,4,0.3,0.3))
plot(prestige ~ income, data = Prestige)

# Spielerei:
plot(prestige ~ income, data = Prestige,
     col = "red", pch = 20)

# Welche Symbole sind per Nummer verfuegbar?
plot(1:30, rep(1, 30), pch = 1:25)

############################# bivariate Vergleiche ############################

# Alle bivariaten Vergleiche mit pairs()
pairs(Prestige[, c(4, 1:3, 6)])         # prestige zuerst

############################# Mosaicplot ######################################

# Mosaicplot
# anderer Datensatz (wir benoetigen mehr als 
# eine kategorische (factor) Variable)

data(Cars93, package = "MASS")
head(Cars93, 2)

library(dplyr)
# Cars93 %>% 
#   select(DriveTrain, Origin, Type) %>% 
#   group_by(DriveTrain, Origin, Type) %>% 
#   summarise(n = n())

# base R
mosaicplot(
  table(Cars93[, c("Type", "DriveTrain","Origin")]),
  main = ""
)

# vcd::mosaic
#install.packages("vcd")
library(vcd)
mosaic(table(Cars93[, c("DriveTrain","Type", "Origin")]))


############################# Korrelation #####################################

# klassisch:
## Korrlation zwischen education und prestige
cor(Prestige$education, Prestige$prestige)

# robust:
#install.packages("robustbase")
library(robustbase)

covMcd(Prestige[, c("education", "prestige")],
       cor = TRUE)$cor

library(dplyr)

cor(Prestige %>% select(-type))               # exclude factor 'type'
set.seed(123)
covMcd(Prestige %>% select(-type), cor = TRUE)$cor

# Spearman Korrelation (normalerweise nur
# auf ordinale Daten)
cor(Prestige %>% select(-type), method = "spearman")

