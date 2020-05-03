library(readxl)
library(vcd)

# Data Frame einlesen
path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Leistungsnachweis_A2/data/"
nn <- read_xlsx(paste0(path, "Internetnutzung_korr.xlsx"), range = "A3:F38", na = c("", "()"),
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

nn$nutzung <- na_to_prev(nn$nutzung)
nn$geschlecht <- na_to_prev(nn$geschlecht)
nn$altersklasse <- na_to_prev(nn$altersklasse)

# 2017: Maenner zw. 30 und 59 Jahren
(kt <- rbind(c(102, 245, 90), c(57, 351, 150)))
dimnames(kt) <- list(c("Sekundar", "Tertiaer"), c("<5h", "5-20h", ">20h"))
kt
