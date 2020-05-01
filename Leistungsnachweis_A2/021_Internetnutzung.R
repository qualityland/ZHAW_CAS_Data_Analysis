library(readxl)
#library(janitor)

# Data Frame einlesen
df2 <-
  read_xlsx(
    path = "./Leistungsnachweis_A2/data/Internetnutzung_korr.xlsx",
    range = "E3:F38",
    na = c("", "()"),
    col_names = c("Anz_Pers_2019", "Anz_Pers_2017")
  )

# only complete cases (no NAs)
#df2 <- df2[complete.cases(df2),]

