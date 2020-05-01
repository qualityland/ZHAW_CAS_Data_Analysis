library(readxl)
#library(janitor)

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

na_to_prev(c(NA, 1, 2, NA, NA, 3, NA, NA, 4, NA))
