### Wednesday, 26.02.2020

# Slides:
#   datentypen.pdf
#   datentypen_beispiel.pdf

############################# Factors #############################

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

# combine several levels into one
f <- factor(
  c(4, 5, 3, 2, 1, 1, 5, 2, 4),
  levels = 1:5,
  labels = c("eins", "zwei", "drei", "vier", "fünf"))
f

levels(f)[3:5] <- "drei+"
f

# change order of levels (from asc to desc)
levels(f) <- c("drei+", "zwei", "eins")
f


############################# Exercises: Factors #############################


# 6a. Weisen Sie v1 die Variable 'Type' aus Cars93 zu.
data(Cars93, package = "MASS")
v1 <- Cars93$Type
table(v1, useNA = "always")


# 6b. Überprüfen Sie, ob v1 ein Faktor ist.
is.factor(v1)
class(v1)


# 6c. Welche Ausprägungen weist der Faktor v1 auf?
levels(v1)
# Welcher numerische Code entspricht welchem level?
table(as.numeric(v1))
table(as.character(v1))  # same as:
table(v1)


# 6d. Verwenden Sie die Funktion levels() um die Ausprägungen von v1 zu ändern
#     und Zusammenzufassen:
#     - Compact und Small als Kleinwagen
#     - Midsize und Sporty als Limousine
#     - Large und Van als Van
levels(v1) <- c("Kleinwagen", "Van", "Limousine", "Kleinwagen", "Limousine", "Van")


# 6e. Überprüfen Sie das Ergebnis.
str(v1)
table(v1, useNA = "always")


# 7a. Wandeln Sie v1 aus 6d) in einen numerischen Vektor um.
v_num <- as.numeric(v1)
v_num


# 7b. Wandeln Sie v1 aus 6d) in einen character Vektor zurück.
v_char <- as.character(v1)
v_char

