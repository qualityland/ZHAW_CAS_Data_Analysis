## Modul C: Data Mining mit Schwerpunkt auf Clustering und Klassifikation
###############################################################################
## PCA Teil 2
#####################################


## Aufgabe 4: Zehnkampf
##########################
## a) 
load("Daten/zehnkampf.Rdata")
head(zehnkampf)
dim(zehnkampf)
boxplot(zehnkampf[,4:13]) # spread ist schlecht zu beurteilen, da Lage sehr verschieden
apply(zehnkampf[,4:13],2,sd) # berechne fuer jede Spalte die Standardabweichung

## b) 
pca_10 <- prcomp(zehnkampf[,4:13],  scale = TRUE)
summary(pca_10)
## Screen-Plot
names(pca_10$sdev) <- paste("PC", 1:length(pca_10$sdev), sep="")
screeplot(pca_10) 
## oder
plot(pca_10$sdev^2, type="b")
abline(h=0, lty=2)
## kumulierte Varianz gegen Anzahl PCs
plot(1:length(pca_10$sdev), cumsum(pca_10$sdev^2)/sum(pca_10$sdev^2),
     ylim = c(0,1), ylab="kumulierte Varianz", xlab="PCAs",
     type = "b")
abline(h = 0.8, lty = 2)

# c) 
plot(pca_10$x)
biplot(pca_10)
round(pca_10$rotation,2)
round(cor(zehnkampf[,4:13]), 2)
library(corrplot)
corrplot(cor(zehnkampf[,4:13]), type="lower")

## Aufgabe 5: Ausreisserdetektion mit PCA
##########################################

## a) 
load("Daten/mnist20x4and1000x0.RData")
dim(x)
image_nummer <- 46 # hier können Sie ein Bild von 1 bis 1020 auswählen 
bild <- matrix( x[image_nummer, ], ncol=28, byrow = TRUE)
image(t(bild[28:1,1:28]))


## b) 
pca_klassisch <- prcomp(x)
str(pca_klassisch)
summary(pca_klassisch)
plot(cumsum(pca_klassisch$sdev^2)/sum(pca_klassisch$sdev^2))
abline(h=0.8)
## c) 
var <- pca_klassisch$sdev^2
var_cum <-  cumsum(var)/sum(var)
plot(1:length(var), var_cum,
     ylim=c(0,1))
(pc_number <- which( var_cum> 0.8)[1])
abline( h = 0.8, v = pc_number)
grid()
axis(1, at = pc_number, labels = pc_number )

## d) 
plot(pca_klassisch$x[,1],pca_klassisch$x[,2], col = c("red", "blue")[as.factor(xlabel)], 
     pch=20, las=1, xlab="PC1", ylab="PC2")

## e) Laden Sie jetzt Pakete `MASS` und `rrcov` und führen Sie mit folgendem Befehl eine robuste Hauptkomponentenanalyse durch:
library("MASS")
library("rrcov")
pca.rob <-  PcaHubert(x, k = 2)
str(pca.rob)
plot(pca.rob@scores[,1],pca.rob@scores[,2], col = c("red", "blue")[as.factor(xlabel)], 
     pch=20, las=1, xlab="PC1", ylab="PC2")

## f) 
plot(pca.rob@sd, col = c("red", "blue")[as.factor(xlabel)], ylim=c(0,3))
abline(h = pca.rob@cutoff.sd)
plot(pca.rob@od, col = c("red", "blue")[as.factor(xlabel)])
abline(h = pca.rob@cutoff.od)
# identifiziere Zeilen, die nach diesem Kriterium Ausreisser sind:
(outlier.rows <- which( pca.rob@od >= pca.rob@cutoff.od ))
## Nicht erkannte 4
bild <- matrix( x[2, ], ncol=28, byrow = TRUE)
image(t(bild[28:1,1:28]))
## fälschlicherweise als Ausreisser ausgewiesene 0 
bild <- matrix( x[827, ], ncol=28, byrow = TRUE)
image(t(bild[28:1,1:28]))

