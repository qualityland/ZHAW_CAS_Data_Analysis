############## Analyse von Zähldaten mit der Poisson-Verteilung ###############

# read text file into a vector
# yeast cells per square
cell_counts <- scan("./Module_A2/data/Hefezellen.dat")
cell_counts[1:9]

barplot(table(cell_counts))
