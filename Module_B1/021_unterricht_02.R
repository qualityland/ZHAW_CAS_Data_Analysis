
data.path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B1/data/"


# Wir simulieren mehrere Datensets von folgendem Modell:
#       Yi = 4 − 2xi + Ei mit σ = 0.5
# und schätzen dann mittels Regression jeweils α und β.
set.seed(134)
nsim <- 100 # Anzahl Simulationen
result <- matrix(NA, nrow = nsim, ncol = 2) # result matrix
x <- seq(0, 1, 0.025)

for(i in 1:nsim){
  y <- 4 - 2 * x + rnorm(length(x), 0, 0.5)
  fit <- lm(y ~ x)
  result[i, ] <- coef(fit)
}

#par(mfrow=c(1,2))
hist(result[, 1], main = "Schaetzung fuer alpha")
hist(result[, 2], main = "Schaetzung fuer beta")

