### Wednesday, 26.02.2020

# Slides:
#   datentypen.pdf
#   datentypen_beispiel.pdf

############################# Lists #############################

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


############################# Exercises: Lists #############################

# 5.
# 5a. Erzeugen Sie eine Liste ll mit insgesamt 3 Listenelementen:
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

# 5b. Extrahieren Sie aus ll das Element v2 und geben Sie das
#    Ergebnis als Liste zurück.
ll["v2"]
class(ll["v2"])

# 5c. Extrahieren Sie aus ll das Element v3 und geben Sie das
#    Ergebnis als Vektor zurück.
ll$v3
class(ll$v3)

# 5d. Extrahieren Sie aus ll die ersten drei Elemente von v1
#    und geben Sie das Ergebnis als Vektor zurück.
ll$v1[1:3]
ll[["v1"]][1:3]

# Geben Sie die Namen der Listenelemente zurück.
names(ll)





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
l2 <- ll[2]
l2
class(l2)


# 5c. Extrahieren Sie aus ll das Element v3 und geben Sie das Ergebnis als
#     Vektor zurück.
l3 <- ll[[3]]
l3
class(l3)
# or:
l3 <- ll$v3
l3
class(l3)


# 5d. Extrahieren Sie aus ll die ersten drei Elemente von v1 und geben Sie
#     das Ergebnis als Vektor zurück.
ll$v1[1:3]
# or:
ll[[1]][1:3]
# Geben Sie die Namen der Listenelemente zurück.
names(ll)
