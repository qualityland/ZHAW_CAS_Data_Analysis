# Aufgabe 2 - Metrische Distanzen

data.path <- '/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_C/data/'
load(paste0(data.path, 'voting_NR.Rdata'))
df.res <- as.data.frame(cmdscale(NR_voting))
plot(df.res, pch="")
text(
  df.res,
  labels = rownames(df.res),
  col = c("yellow", "orange", "green", "black", "blue", "red", "darkgreen")[NR_meta$Fraktion]
)
