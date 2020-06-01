
# Boxplot
# Jahresdurchschnitt pro Kopf und Tag aus 192 Zürcher Gebieten
boxplot(
  wv2018,
  horizontal = TRUE,
  main = "Wasserverbrauch ZH 2018",
  sub = "Jahresdurchschnitt pro Kopf und Tag aus 192 Zürcher Gebieten",
  xlab = "in Liter"
)


# Histogramm
hist(wv2018,
     main = "Wasserverbrauch ZH 2018",
     #sub = "",
     ylab = "Häufigkeit",
     xlab = "pro Kopf und Tag in Liter"
)

ggplot(data = wv[wv$JAHR == 2018,], aes(x = WvpTE)) +
  geom_histogram()
