library(readxl)

# Data Frame einlesen
path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Leistungsnachweis_A2/data/"
nn <-
  read_xlsx(paste0(path, "Internetnutzung_korr.xlsx"),
            range = "A3:F38", na = c("", "()"),
            col_names = c("nutzung", "geschlecht", "altersklasse", "bildungsstand", "x2019", "x2017")
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
nn$nutzung <-
  factor(na_to_prev(nn$nutzung),
         levels = c("weniger als fünf Stunden pro Woche", "fünf Stunden oder mehr pro Woche",
                    "mehr als zwanzig Stunden pro Woche"),
         labels = c("weniger 5h", "zw 5 und 20h", "ueber 20h"))

nn$geschlecht <- 
  factor(na_to_prev(nn$geschlecht), levels = c("Frau", "Mann"), labels = c("f", "m"))

nn$altersklasse <- factor(na_to_prev(nn$altersklasse))

nn$bildungsstand <-
  factor(nn$bildungsstand,
         levels = c(
           "Ohne nachobligatorische Ausbildung (25 Jahre und älter)",
           "Sekundarstufe II (25 Jahre und älter)", "Tertiärstufe (25 Jahre und älter)"),
         labels = c("Obligatorische", "Sekundarstufe", "Tertiaerstufe"))
