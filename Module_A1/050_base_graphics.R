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

