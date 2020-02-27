### Wednesday, 26.02.2020


# Slides:
#   datentypen.pdf
#   datentypen_beispiel.pdf


## Vectors


## Exercises: Vectors


# 1a. Erzeugen Sie auf zwei verschiedene Arten numerische Vektoren der ganzen
#     Zahlen von -3 bis 2 (-3,-2,-1,0,1,2).
#     Weisen Sie die Vektoren zwei Objekten v1 und v2 zu.
v1 <- -3:2
v2 <- seq(from = -3, to = 2)


# 1b. Überprüfen Sie (z.B mit identical() oder mit - (minus)) ob v1 und v2
#     gleich sind.
identical(v1, v2)


# 1c. Erzeugen Sie folgende Sequenz an Datenpunkten und weisen Sie das Ergebnis
#     einem Objekt v zu:
#     2, 3, 4, 5, 6, 7, 8,
#     4.1, 4.2, 4.3, 4.4, 4.1, 4.2, 4.3, 4.4, 4.1, 4.2, 4.3, 4.4, 
#     -3, -2, -1, 0, 1
v <- c(2:8, rep(seq(4.1, 4.4, by = 0.1), 3), -3:1)


# 1d. Wiederholen Sie v 2 mal.
rep(v, times = 2)


# 1e. Wiederholen Sie jedes Element von v exakt 2 mal.
rep(v, each = 2)


# 1f. Wieviele Elemente weist das Ergebnis von 1e) auf?
length(rep(v, each = 2))


# 2a. Erstellen Sie einen Zeichenvektor mit den ersten 15 Großbuchstaben des
#     Alphabets. (Hinweis: ?„Constants“) und weisen Sie das Ergebnis einem
#     Vektor s zu.
s <- LETTERS[1:15]
s


# 2b. Extrahieren Sie aus s die Buchstaben, die an ungeraden
#     Positionen (1,3,. . . ) stehen.
s[seq(from = 1, to = length(s), by = 2)]


# 2c. Extrahieren Sie aus s die Buchstaben, die an geraden
#     Positionen (2,4,. . . ) stehen.
s[seq(from = 2, to = length(s), by = 2)]


# 2d. Extrahieren Sie aus s alle Buchstaben ab (inklusive)
#     dem Buchstaben D.
s[which(s == "D"):length(s)]
# or:
s[-c(1:3)]


# 3a. Erzeugen Sie einen numerischen Vektor v1 der Länge 10 und Werten >= -3 und <= 3.
#     (Hinweis: zB: seq() oder ?sample)
set.seed(123)
v1 <- sample(-3:3, size = 10, replace = TRUE)
v1


# 3b. Erzeugen Sie einen logischen Vektor index, der TRUE für alle negativen
#     Werte von v1 ist.
index <- v1 < 0
v1
index


# 3c. Indizieren Sie v1 mit dem logischen Vektor und interpretieren Sie das Ergebnis.
v1[index]
# result: all negative values of v1.


# 3d. Wieviele Elemente hat das Ergebnis von 3c?
length(v1[index])


# 4a. Erstellen Sie einen numerischen Vektor v1 mit den ganzenZahlen von 0 bis
#     10 und einen numerischen Vektor v2 mit denZahlen 0 bis 15.
v1 <- 0:10
v2 <- 0:15


# 4b. Summieren Sie v1. Was ist das Ergebnis?
sum(v1)

# 4c. Bilden Sie die Differenz von v1 und v2. Interpretieren Sie das Ergebnis.
v1 - v2
# v1 is recycled until all elements of v2 have been subtracted
#  0  1  2  3  4  5  6  7  8  9  10  11  12  13  14  15
#  0  1  2  3  4  5  6  7  8  9  10   0   1   2   3   4
#  0  0  0  0  0  0  0  0  0  0   0 -11 -11 -11 -11 -11


# 4d. Berechnen Sie die eindeutigen Elemente des Ergebnisses aus 4c.
unique(v1 - v2)


# 5a. Erzeugen Sie eine Liste ll mit insgesamt 3 Listenelementen:
#     v1: numerischer Vektor der Länge 10
#     v2: character Vektor der Länge 15
#     v3: logischer Vektor der Länge 5
ll <- list(
  v1 = 1:10,
  v2 = LETTERS[1:15],
  v3 = sample(c(TRUE, FALSE), size = 5, replace = TRUE)
)

ll


# 5b. Extrahieren Sie aus ll das Element v2 und geben Sie das Ergebnis als
#     Liste zurück.
ll[2]


# 5c. Extrahieren Sie aus ll das Element v3 und geben Sie das Ergebnis als
#     Vektor zurück.
ll[[3]]
# or:
ll$v3


# 5d. Extrahieren Sie aus ll die ersten drei Elemente von v1 und geben Sie
#     das Ergebnis als Vektor zurück.
ll$v1[1:3]
# or:
ll[[1]][1:3]
# Geben Sie die Namen der Listenelemente zurück.
names(ll)



## Factors

# correct data type has a high impact:
# create numeric vector
x <- c(5, 7, 123, 22, 33)
summary(x)
is.numeric(x)
plot(x)

# make it a factor
x <- factor(x)
summary(x)
plot(x)


groesse <- factor(
  c(2, 3, 1, 1, 1, 2, 3, 3),
  levels = c(1, 2, 3),
  labels = c("klein", "mittel", "gross")
)

levels(groesse)

levels(groesse) <- c("kl","mi","gr")
groesse


## Exercises: Factors


# 6a. Weisen Sie v1 die Variable Type aus Cars93 zu.
data(Cars93, package = "MASS")
v1 <- Cars93$Type
table(v1, useNA = "always")

# 6b. Überprüfen Sie, ob v1 ein Faktor ist.
is.factor(v1)
class(v1)

# 6c. Welche Ausprägungen weist der Faktor v1 auf?
levels(v1)

# 6d. Verwenden Sie die Funktion levels() um die Ausprägungen von v1 zu ändern
#     und Zusammenzufassen:
#     - Compact und Small als Kleinwagen
#     - Midsize und Sporty als Limousine
#     - Large und Van als Van
levels(v1) <- c("Kleinwagen", "Van", "Limousine", "Kleinwagen", "Limousine", "Van")

# 6e. Überprüfen Sie das Ergebnis.
str(v1)
table(v1, useNA = "always")





## Data Frames


nn <- c("Bernhard", "Matthias", "Angelika")

df1 <- data.frame(
  namen = nn,
  geschl = c("m", "m", "w"),
  groesse = c(185, 182, 165)
)

df1
str(df1)
df1[c(1, 3), ]
df1$namen
df1[df1$groesse <= 180, ]



# Exercises
# 8.

# a. Erzeugen Sie einen Data Frame df aus Cars93 mit den folgenden Variablen:
#    Manufacturer, Type, Price und Luggage.room.
data(Cars93, package = "MASS")

df <- Cars93[ , c("Manufacturer", "Type", "Price", "Luggage.room")]
df

# better (no additional R object):
vars <- c("Manufacturer", "Type", "Price", "Luggage.room")
Cars93[, vars]

# b. Betrachten Sie die Struktur von df.
str(df)

# c. Berechnen Sie die Anzahl der Zeilen und Spalten von df.
nrow(df)
ncol(df)
dim(df)

# d. Extrahieren Sie aus df alle Zeilen, bei denen Volkswagen der
#    Hersteller (Variable Manufacturer) ist und weisen Sie das
#    Ergebnis einem Objekt df2 zu.
df2 <- df[df$Manufacturer == "Volkswagen", ]
df2

# e. Extrahieren Sie auf zwei Arten Variable Price von df2.
df2$Price
df2[, c("Price")]
df2[, "Price"]
df2[, 3]

# f. Geben Sie die Variablennamen von df2 zurück.
colnames(df2)


# Lists

lst <- list(
  v1 = 1:5,
  df = data.frame(x = 1:2, y = 3:4),
  l1 = list(a = "A", b = "B")
)

lst

str(lst)

# access list elements
class(lst[2])       # CAVE: returns a list!
class(lst[[2]])     # correct data type
class(lst$df)       # correct data type

lm1 <- lm(Price ~ Weight, data = Cars93)
lm1
lm1$coefficients


# Exercises

# 5.
# a. Erzeugen Sie eine Liste ll mit insgesamt 3 Listenelementen:
#        v1: numerischer Vektor der Länge 10
#        v2: character Vektor der Länge 15
#        v3: logischer Vektor der Länge 5
ll <- list(v1 = sample(10),
           v2 = letters[1:15],
           v3 = sample(c(TRUE, FALSE), size = 5, replace = TRUE))
ll$v1
class(ll$v1)
ll$v2
class(ll$v2)
ll$v3
class(ll$v3)

# b. Extrahieren Sie aus ll das Element v2 und geben Sie das
#    Ergebnis als Liste zurück.
ll["v2"]
class(ll["v2"])

# c. Extrahieren Sie aus ll das Element v3 und geben Sie das
#    Ergebnis als Vektor zurück.
ll$v3
class(ll$v3)

# d. Extrahieren Sie aus ll die ersten drei Elemente von v1
#    und geben Sie das Ergebnis als Vektor zurück.
ll$v1[1:3]
ll[["v1"]][1:3]

# Geben Sie die Namen der Listenelemente zurück.
names(ll)





## Functions

moments <- function(x, n = 2) {
  x ^ n
}

moments(3)
moments(n = 2, 3)
moments(2, x = 3)
moments(n = 2, x = 3)
moments(x = 3, n = 2)


mystats <- function(x, text = FALSE) {
  result <- c(median(x), sd(x), mad(x))
  names(result) <- c("Median", "SD", "MAD")
  if(text) {
    cat("Es hat geschneit\n")
  }
  return(result)
}

x <- rnorm(50)
mystats(x)
mystats(x, TRUE)



# Datenmanipulation (using dplyr)
library(dplyr)

# Keyboard shortcut for %>%:
# Cmd + Shift + M

iris <- as_tibble(iris)
iris
class(iris)

# slice() - choose rows
iris %>% slice(1)


# filter() - only rows matching conditions
iris %>% filter(Species == "setosa" & Petal.Width > 0.3)



# select() - columns / variables


# arrange() - sort
iris %>% arrange(desc(Species), Petal.Width, Petal.Length)


# mutate() - create new variables / columns


# group_by() - group data


# sumarize() - calculate statistics
# 



# Exercises "datenmanipulation_beispiel"
data(Cars93, package = "MASS")

# 1.
# 1a)
str(Cars93)
?MASS::Cars93

# 1b)
df <- as_data_frame(Cars93)
df

# 1c)
df %>%
  filter(Horsepower < 100) %>% 
  nrow

# 1d) Entfernen Sie die Variable Rev.per.mile.
df %>% 
  select(-Rev.per.mile) %>% 
  colnames

# 1e) Ordnen Sie den Datensatz absteigend nach Variable Cylinders und innerhalb
#     von Cylinders aufsteigend nach Passengers. Geben Sie anschließend nur
#     diese beiden Variablen aus.
df %>% 
  arrange(desc(Cylinders), Passengers) %>% 
  select(Cylinders, Passengers)


# 1f) Benennen Sie Variable Horsepower um. Der neue Name soll PS sein.
df %>% 
  rename(PS = Horsepower) %>% 
  colnames


# 2a) Berechnen Sie auf Variable MPG.city die Variable liter_km_city
#     dabei gilt: liter_km_city ~ 235.2146 / MPG.city
df %>% 
  mutate(liter_km_city = 235.2146 / MPG.city) %>% 
  select(liter_km_city, MPG.city)


# 2b) Erstellen Sie eine Variable 'Preiskategorie' die den Wert 'billig'
#     aufweist, wenn Variable Price kleiner dem 0.25er Quantil ist
#     (Hinweise: ?ifelse oder ?cut und ?quantile) und 'nicht billig' sonst.

# 2d) Berechnen Sie für billige und nicht billige Autos (Variable Preiskategorie)
#     folgende Werte:
#        die Gruppengröße
#        die mittlere PS-Anzahl (Variable PS)
#        den Durschnittsverbrauch in der Stadt (Variable liter_km_city)

df %>% 
  rename(PS = Horsepower) %>% 
  mutate(liter_km_city = 235.2146 / MPG.city) %>% 
  mutate(Preiskategorie = case_when(Price < quantile(df$Price)["25%"] ~ "billig",
                                    Price >= quantile(df$Price)["25%"] ~ "nicht billig")) %>% 
  group_by(Preiskategorie) %>% 
  summarise(count = n(),
            mean_PS = mean(PS),
            mean_l_km_city = mean(liter_km_city))
