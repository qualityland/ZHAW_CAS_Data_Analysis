library(readxl)

# Data Frame einlesen
df2 <-
  read_xlsx(
    path = "./Leistungsnachweis_A2/data/Internetnutzung_korr.xlsx",
    range = "A3:F38",
    na = c("", "()"),
    col_names = c("Internetnutzung", "Geschlecht", "Altersklasse", "Bildungsstand", "Anz_Pers_2019", "Anz_Pers_2017")
  )


# NAs mit vorherigem Wert ersetzen
na_to_prev <- function(x) {
  if (length(x) > 0L) {
    non.na.idx <- which(!is.na(x))
    if (is.na(x[1L])) {
      non.na.idx <- c(1L, non.na.idx)
    }
    rep.int(x[non.na.idx], diff(c(non.na.idx, length(x) + 1L)))
  }
}

# Factor Levels setzen
df2$Internetnutzung <-
  factor(
    na_to_prev(df2$Internetnutzung),
    levels = c(
      "weniger als fünf Stunden pro Woche",
      "fünf Stunden oder mehr pro Woche",
      "mehr als zwanzig Stunden pro Woche"),
    labels = c("weniger 5h", "zw 5 und 20h", "ueber 20h"))

df2$Geschlecht <- 
  factor(
  na_to_prev(df2$Geschlecht),
  levels = c("Frau", "Mann"),
  labels = c("f", "m"))

df2$Altersklasse <- factor(na_to_prev(df2$Altersklasse))

df2$Bildungsstand <-
  factor(
    df2$Bildungsstand,
    levels = c(
      "Ohne nachobligatorische Ausbildung (25 Jahre und älter)",
      "Sekundarstufe II (25 Jahre und älter)",
      "Tertiärstufe (25 Jahre und älter)"),
    labels = c("Obligatorische",
               "Sekundarstufe",
               "Tertiaerstufe"))



# only complete cases (no NAs)
#df2 <- df2[complete.cases(df2),]
# Anzahl Frauen 2019 - 1400
sum(df2$Anz_Pers_2019[df2$Geschlecht == 'f'], na.rm = T)
# Anzahl Maenner 2019 - 1508
sum(df2$Anz_Pers_2019[df2$Geschlecht == 'm'], na.rm = T)
# Anzahl Frauen 2017 - 1375
sum(df2$Anz_Pers_2017[df2$Geschlecht == 'f'], na.rm = T)
# Anzahl Maenner 2017 - 1508
sum(df2$Anz_Pers_2019[df2$Geschlecht == 'm'], na.rm = T)
