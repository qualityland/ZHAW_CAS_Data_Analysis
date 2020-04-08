# Arbeitsblatt HT1

# Aufgabe 1

# (i) Radioaktivitaet
# -------------------

# Daten laden
alpha <- read.table("./Module_A2/data/alpha.dat", header = TRUE)

# a) Barplot
plot(
  x = alpha$k,
  y = alpha$freq,
  type = "h",
  lwd = 2,
  col = "blue",
  xlab = "k",
  ylab = expression(h[k]~"(Zerfälle / 7.5s)")
)

# b) Mittelwert
n <- sum(alpha$freq)
mu <- sum(alpha$freq * alpha$k) / n
mu

# c) Poisson-Modells in Barchart einzeichnen
pModel <- n * dpois(alpha$k, lambda = mu)
lines(alpha$k,
      pModel,
      type = "b",
      lwd = 2,
      col = "red")

# ii) Verkehr
# -----------

# Daten laden
verkehr <- read.table("./Module_A2/data/verkehr.dat", header = TRUE)

# a) Barplot
plot(
  verkehr$k,
  verkehr$freq,
  type = "h",
  lwd = 2,
  col = "blue",
  xlab = "k",
  ylab = expression(h[k] ~ "(Rechtsabbieger / 3min)")
)

# b) Mittelwert
n <- sum(verkehr$freq)
mu <- sum(verkehr$freq * verkehr$k) / n
mu


# c) Poisson-Modell in Barchart einzeichnen
pModel <- n * dpois(verkehr$k, lambda = mu)
lines(verkehr$k,
      pModel,
      type = "b",
      lwd = 2,
      col = "red")


# Aufgabe 2

# Würfel
# ------

# a) Wahrscheinlichkeitsverteilung
plot(
  1:6,
  rep(1 / 6, 6),
  type = "h",
  lwd = 2,
  col = "blue",
  xlab = "k (gewürfelte Zahl)",
  ylab = expression(h[k]~"(rel. Wahrscheinlichkeit)")
)
