data.dir <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Pruefung_B2/data/"
load(paste0(data.dir, "milk_data.rda"))
str(milk_data)

# Aufgabe 1

# (a)
# Definieren Sie die Spalte milk gemäss den Angaben von oben in R sinnvoll und
# korrekt als Zeitreihe. Geben Sie den verwendeten R-Befehl an.
mlk.dat <- milk_data$milk
mlk.ts <- ts(mlk.dat, start = c(1994, 1), frequency = 12)  # Monatsdaten, Start: Jan 1994



# (b)
# Schauen Sie sich die Zeitreihe in R an. Ist die Zeitreihe stationär?
# Begründen Sie ihre Antwort.
plot(mlk.ts, main = "Milchleistung, USA 1994 - 2005", ylab = "[l] pro Monat und Kuh")
# Zerlegung mit decompose()
plot(decompose(mlk.ts))
# Nicht stationär. Sowohl Trend als auch Saisonkomponente (mit Max. im Fuehjahr)
# enthalten.



# (c)
# Führen Sie eine STL-Zerlegung der Zeitreihe durch, wobei Sie annehmen können,
# dass die Saisonkomponente konstant ist.
mlk.stl <- stl(mlk.ts, s.window = 'periodic')
plot(mlk.stl)

## i)
# Welche der drei Komponeten der STL-Zerlegung dominiert die Struktur der Zeitreihe?
# Antwort: Die Saisonale.

## ii)
# Ist eine log-Transformation der Daten angebracht?
# Antwort: Nein, die Varianz ist eher konstant.



# (d)
# Nehmen Sie nun den stationären Restterm aus der STL-Zerlegung.
# Ist das Anpassen eines AR(p)-Modells angebracht?
# Argumentieren Sie mit Hilfe des ACF und PACF-Plot.
# remainder
mlk.rmdr <- mlk.stl$time.series[, 3]   # Remainder (Restterm) der STL-Zerlegung
library(forecast)
tsdisplay(mlk.rmdr)



# (e)
# Unabhängig von ihren Argumenten in d) sollen Sie nun aus der PACF die zwei
# plausibelsten Ordnungen p für ein AR(p)-Modell ablesen und angeben.
# Erklären Sie ihre Wahl.
# p=6, p=12
# drop-off nach Lag 6 bzw. 12



# (f)
# Passen Sie nun ein AR(p)-Modell an die Daten des Restterms an, welches zum
# kleinsten AIC-Wert führt. Geben Sie dessen Ordnung p an.
# Geben Sie den verwendeten R-Code an.
(mlk.ar.burg <- ar.burg(mlk.rmdr))
# Antwort: Order selected 6



# (g)
# Passen Sie nun ein additives Holt-Winters Modell an die
# in a) generierte Zeitreihe an.
mlk.hw <- hw(mlk.ts)


## i)
# Welche Bedeutung und welchen Wert hat der Parameter β?
# Welchen Schluss ziehen Sie aus diesem Resultat?
mlk.hw$model$par[2]


## ii)
# Wie gross ist die mit dem Holt-Winters Modell prognostizierte Milchleistung
# für den Juni 2006.
mlk.hw$mean[6]


# (h)
# Berechnen Sie das Zeitreihen-Regressionsmodell milk ~ zeitschritte + monat
# für die gegeben Daten (eine Transformation ist nicht nötig).

## i)
# Geben Sie den dazu verwendeten R-Befehl an.
mlk.lm <- lm(milk ~ zeitschritte + monat, data = milk_data)

## ii)
# Ändert sich die durchschnittliche Milchleistung der Kühe zwischen 1994 und
# 2005 signifikant (α = 1%) aufgrund der Resultate des Regressionsmodells?
summary(mlk.lm)
# Ja, Steigungsparameter beta fuer 'zeitschritte' ist hoch signifikant (p-Wert=5.63e-09 ***)

## iii)
# Können Sie dem p-Wert von milk vertrauen? Bitte begründen Sie ihre Antwort.
tsdisplay(mlk.lm$resid)


# (i)
# Welches AR(p)-Modell beschreibt die Residuen des Regressionsmodell aus h) gut?
mlk.ts.resid <- ts(mlk.lm$residuals)
ar.burg(mlk.ts.resid)
tsdisplay(mlk.ts.resid)

# (j)
# Verwenden sie nun die Funktion gls(), um im Zeitreihen-Regressionsmodell von
# Frage h) auch noch die Korrelation der Residuen zu berücksichtigen

## i)
# Geben Sie die dafür die notwendigen R-Befehle an.
library(nlme)
corStruct <- corARMA(form = ~ zeitschritte, p = 1, q = 0)
mlk.gls <- gls(milk ~ zeitschritte + monat, data= milk_data, correlation = corStruct)
summary(mlk.gls)

## ii)
# Wie beantworten Sie nun die Frage (h) ii)?
# p-Wert = 0.0157 > 0.01 knapp nicht signifikant
