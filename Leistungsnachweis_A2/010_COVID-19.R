
# Analysieren der Daten mit min. 4 der 5 besprochenen Verfahren:
# - poisson.test()
#   - Schaetzen
#   - Testen
#   - Vertrauensintervalle
# - binom.test()
#   - Testen bei 2 Poisson-Realisationen
# - chisq.test()
#   - Dispersionstest
# - BootSim


# Daten vom 14.03.2020 und 14.04.2020

c19 <- data.frame(
  kanton=c( "AG", "AI", "AR", "BE", "BL", "BS", "FR", "GE", "GL", "GR","JU", "LU", "NE", "NW", "OW", "SG", "SH", "SO", "SZ", "TG", "TI", "UR", "VD", "VS", "ZG", "ZH"),
  maerz14=c( 31,    2,    5,   78,   47,  119,   36,  280,    5,   47,  15,   19,   68,    5,    8,   26,    1,   10,   13,    5,  262,    2,  350,   88,   13,  148),
  april14=c(912,   24,   79, 1470,  755,  119,  899, 4390,  105,  741, 185,  589,  606,  105,   64,  664,   57,  329,  258,  296, 2912,   78, 4741, 1664,  171, 3067),
  einw10k=c(67.8207, 1.6145, 5.5234, 103.4977, 28.8132, 19.4766, 31.8714, 49.948, 4.0403, 19.8379, 7.3419, 40.9557, 17.685, 4.3223, 3.7841, 50.7697, 8.1991, 27.3194,
            15.9165, 27.6472, 35.3343, 3.6433, 79.9145, 34.3955, 12.6837, 152.0968)
)

