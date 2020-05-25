
# data path
data.path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Pruefung_B1/data/"

library(readxl)
# xls
df <- read_xls(paste0(data.path, "Alpenquerender_Gueterverkehr_Schiene.xls"))

# dat
windmill <- read.table(paste0(data.path, "Windmuehle.dat"), header = TRUE)
