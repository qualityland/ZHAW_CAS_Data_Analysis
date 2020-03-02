### Wednesday, 26.02.2020

# Slides:
#   datentypen.pdf
#   datentypen_beispiel.pdf

############################# Vectors #############################

# create a numeric vector
vnum <- c(1, 3, 5.9, 7)
vnum
# check if numeric vector
is.numeric(vnum)

# create character vector
vchar <- c("eins","zwei","drei")
vchar
is.character(vchar)

# create logical vector
vlog1 <- vnum > 3
vlog1

vlog2 <- vchar == "zwei"
vlog2


# vectorized operators 'recycle' the shorter vector (waring!)
v1 <- c(1, 2, 3)
v2 <- c(4, 5)
v1 + v2


# ':' creates integer vector
v3 <- 1:10
class(v3)

# seq() creates integer
v4 <- seq(1, 10)
class(v4)
# or numeric vector
v5 <- seq(1, 10, by = 1)
class(v5)

# repeat vectors
rep1 <- rep(1:5, times = 2)
rep1

rep2 <- rep(1:5, each = 2)
rep2

# times parameter can be a vector
rep3 <- rep(c("eins", "zwei", "drei"), times = c(1, 2, 3))
rep3


############################# Exercises: Vectors #############################

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


# 3a. Erzeugen Sie einen numerischen Vektor v1 der Länge 10 und
#     Werten >= -3 und <= 3.
#     (Hinweis: zB: seq() oder ?sample)
set.seed(123)
v1 <- sample(-3:3, size = 10, replace = TRUE)
v1


# 3b. Erzeugen Sie einen logischen Vektor index, der TRUE für alle negativen
#     Werte von v1 ist.
index <- v1 < 0
v1
index


# 3c. Indizieren Sie v1 mit dem logischen Vektor und interpretieren Sie
#     das Ergebnis.
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

