### Wednesday, 26.02.2020

# Slides:
#   datentypen.pdf
#   datentypen_beispiel.pdf

############################# Functions #############################

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


