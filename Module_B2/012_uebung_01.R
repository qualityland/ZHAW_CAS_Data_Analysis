library(readxl)

############## Daten laden ##############

# Pfad
data.path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B2/data/"

# Alpenquerender Güterverkehr
gv <- read_xls(paste0(data.path, "Alpenquerender_Gueterverkehr_Schiene.xls"))

# Neuzulassungen
nz <- read.table(paste0(data.path, "Inverkehrssetzung_Strassenfahrzeuge_Schweiz.dat"), header = T, sep = ";")

# Rohoelpreis
load(paste0(data.path, "Rohoelpreis.rda"))

############## ts object & ts.plot ##############

# Alpenquerender Güterverkehr
ts.gv <- ts(gv$Verkehr, start = c(min(gv$Jahr), 1), frequency = 1)
ts.gv
plot(ts.gv, main = "alpenquerenden Güterverkehr (Schiene)", ylab = "Gütermenge (Mio t)")

# Neuzulassungen
ts.nz <- ts()

# decompose
plot(decompose(ts.gv))


### TS Objekt erzeugen

