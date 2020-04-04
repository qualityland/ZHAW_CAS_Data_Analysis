### Wednesday, 11.03.2020
# Statistische Grundlagen der Datenanalyse
# Lektor:
#   Andreas Ruckstuhl

# Slides:
#   A2-HT2_Folien.pdf
#   A2-HT2_Anhang.pdf


# neue Probe mit 7 Hefezellen
# p-Value fuer lamba=4.68 berechnen
poisson.test(x = 7, r = 4.68)


# neue Brauerei, eine Probe mit 40 Hefezellen
# wo liegt lambda bei einem Vertrauensintervall von 95%
poisson.test(x = 40, conf.level = 0.95)


# plausible Werte fuer Auszaehlung von einem
# Hefezellen-Quadrat
poisson.test(x = 1872, T = 400, conf.level = 0.95)
