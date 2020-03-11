library(carData)
data(Prestige)
View(Prestige)
str(Prestige)
dim(Prestige)

hist(Prestige$education)          # Verteilung 'education'
hist(Prestige$income)             # Verteilung 'income', rechtsschief

boxplot(Prestige$income)
boxplot(income ~ type, data = Prestige)
?Prestige
boxplot(prestige ~ type, data = Prestige)

pie(table(Prestige$type))
# besser:
pdf("myfile.pdf")                 # starts the graphics device
barplot(table(Prestige$type))     # plot
dev.off()                         # close the device

# income ~ prestige, klassisch
plot(prestige ~ income, data = Prestige)
cor(Prestige[, c("income","prestige")])

# income ~ prestige, robust
library(robustbase)
covMcd(Prestige[, c("income","prestige")], cor = TRUE)$cor

pairs(Prestige)
cor(Prestige[, 1:5])              # exclude nominal variable 'type'

data(mtcars)
str(mtcars)
mtcars$vs <- factor(mtcars$vs)    # make engine type a factor
mtcars$cyl <- factor(mtcars$cyl)  # make nr of cylinders a factor
x11()
barplot(table(mtcars$cyl, mtcars$vs), col = 1:3)            # color 1-3
legend("topright", legend = c(4,6,8), col = 1:3, pch = 15)  # 
table(mtcars$vs)
table(mtcars$cyl)

View(mtcars)
