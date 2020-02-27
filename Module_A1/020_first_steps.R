### Wednesday, 26.02.2020

# Slides:
#   ersteSchritte.pdf
#   ersteSchritte_beispiel.pdf


sum(c(3, 4))
3 + 4
# internally:
"+"(3, 4)

# load dataset
data(iris)

# show structure
str(iris)

# show first/last entries
head(iris$Sepal.Length, 3)
tail(iris$Sepal.Length, 3)

# is this a vector?
is.vector(iris$Sepal.Length)

# create an integer vector
1:10

# create an integer vector
seq(1, 10)

# create a numeric vector
class(seq(1, 10, 1))

