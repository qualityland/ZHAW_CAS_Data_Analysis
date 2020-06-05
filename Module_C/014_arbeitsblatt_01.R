

### Aufgabe 2

# Laden Sie den R-Datensatz body. Er enthält die Körpergrösse, das Gewicht und
# die Schuhgrösse von 600 Personen (simulierte Daten).
data.path <- '/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_C/data/'
load(paste0(data.path, 'body.Rdata'))

str(body)

# (a)
# Visualisieren Sie die Daten. Was sieht man?
pairs(body)
# Groesse, Gewicht und Schuh korrelieren miteinander.

# (b)
# Berechnen Sie die Kovarianz-Matrix der Daten (R-Befehl cov()). Berechnen Sie
# die Summe der Varianzen der 3 Variablen (d.h. Diagonale der Kovarianz-Matrix).
cov(body)
#            Groesse    Gewicht      Schuh
# Groesse 0.01284146  0.7673536  0.2863454
# Gewicht 0.76735357 58.8527013 17.3186532
# Schuh   0.28634536 17.3186532  9.7924606

# (c)
# Führen Sie mit der Funktion prcomp die Hauptkomponentenanalyse durch. Bei der
# Haupt- komponentenanalyse arbeiten wir mit zentrierten Daten. Sie können das
# manuell machen oder bei der Funktion prcomp das Argument center auf TRUE setzen
# (Defaulteinstellung).
prcomp(body, center = TRUE)
# Standard deviations (1, .., p=3):
#   [1] 8.0225106 2.0725037 0.0453369
# 
# Rotation (n x k) = (3 x 3):
#                 PC1          PC2         PC3
# Groesse -0.01271152  0.009491687  0.99987415
# Gewicht -0.95305114 -0.302668278 -0.00924306
# Schuh   -0.30254246  0.953048698 -0.01289344

# (d)
# Visualisieren Sie die Hauptkomponenten. Auf die Hauptkomponenten können Sie
# mit dem PCA-Objekt pca$x zugreifen.
library(ggfortify)


pca <- prcomp(body)
head(pca$x)



###  Aufgabe 3
load(paste0(data.path, 'abst.Rdata'))
# (a)
# Gewinnen Sie zuerst einen Überblick über die Daten. Was fällt Ihnen auf?
# Kommentieren Sie!
dim(abst)
colnames(abst)

pca3 <- prcomp(abst)
summary(pca3$x)
