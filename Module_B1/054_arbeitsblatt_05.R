# Daten einlesen
data.path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B1/data/"
salary <- read.csv(paste0(data.path, "salary.csv"), header = TRUE)


plot(
  y ~ experience,
  data = salary,
  col = education,
  pch = education,
  ylab = "Jahreslohn [CHF]",
  xlab = "Erfahrung [J]",
  ylim = c(10000, 150000),
  xlim = c(0, 40)
)
legend(
  "topleft",
  c("Berufslehre", "Maturitaet", "Hochschulabschluss"),
  col = 1:3,
  pch = 1:3
)
