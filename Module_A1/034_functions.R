### Wednesday, 26.02.2020

# Slides:
#   datentypen.pdf
#   datentypen_beispiel.pdf

################################# NA ################################
# NAs are often the result of data conversions
v1 <- c(1:2, "a", 3:4, "b")
v1

v2 <- as.integer(v1)
v2

# is.na() detects NAs
is.na(v2)         # logical vector
which(is.na(v2))  # indexes of NA


################################ NaN ################################
# NaN is often the result of non-valid calculations
v3 <- sqrt(c(-5, -2, 0, 2))
v3

# is.nan() detects NaN
is.nan(v1)
which(is.nan(v1))


################################ NULL ###############################
# NULL is often the result of expressions whose result is not defined
v4 <- c()
v4

# detect NULL
is.null(v4)


################################ Inf ################################
# Inf stands for Infinum (infinite), -Inf for minus infinite
v5 <- c(Inf, 2, 3, -Inf)
v5

# detect Inf, -Inf
is.infinite(v5)
# or:
is.finite(v5)


############################# Functions #############################

moments <- function(x, n = 2) {
  x ^ n
}

# position matching
moments(3)
moments(3, 2)
# name matching
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


