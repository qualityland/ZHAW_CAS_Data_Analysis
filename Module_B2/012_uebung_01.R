library(readxl)

# einlesen
path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B2/data/"
gv <- read_xls(paste0(path, "Alpenquerender_Gueterverkehr_Schiene.xls"))
nz <- read.table(paste0(path, "Inverkehrssetzung_Strassenfahrzeuge_Schweiz.dat"), header = T)

# ts object
ts.gv <- ts(gv$Verkehr, start = c(min(gv$Jahr), 1), frequency = 1)
ts.gv

# ts plot
plot(ts.gv, main = "alpenquerenden Güterverkehr auf der Schiene", ylab = "Gütermenge in Mio t")
