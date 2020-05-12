
path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B1/"

set.seed(134)
nsim <- 100 # Anzahl Simulationen
result <- matrix(NA, nrow = nsim, ncol = 2)
x <- seq(0, 1, 0.025)
for(i in 1:nsim){
  y <- 4 - 2 * x + rnorm(length(x), 0, 0.5)
}
