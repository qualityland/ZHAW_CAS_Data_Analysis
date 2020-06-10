## Modul E: Data Mining mit Schwerpunkt auf Clustering und Klassifikation
###############################################################################
## R-Code PCA
###################

## Haupkompenentenanalyse in R
#####################################
## Daten wie bei der Aufgabe 1 auf dem Arbeitsblatt 1
X <- matrix(c(-1.5, -0.5, -2, -1.5, -1, 0, 2.5, 0.5, 2, 1.5), ncol=2, byrow=TRUE)
rownames(X) <- 1:5
X
colMeans(X) #Schon Zentriert
par(mar=c(4, 4,1,1), las=1)
plot(x=X[,1], y=X[,2], ylim=c(-3, 3), xlim=c(-3, 3), pch=16, xaxs="i",  yaxs="i", ylab=expression('x'[2]), xlab=expression('x'[1]))
grid(nx=12, ny=12)
text(x=X[,1], y=X[,2], labels= rownames(X), pos=4)

## PCA mit Eigenvektoren
(co <-  cov(X))
var(X[,1])
var(X[,2])
cov(X[,1], X[,2])
cov(X[,1], X[,2])/(sd(X[,1])*sd(X[,2]))
cor(X[,1], X[,2])
sum(diag(co))
(eig <-  eigen(co))
(U <-  eig$vectors) # Rotationsmatrix
(Z <-  X %*% U) # Rotation
round(cov(Z),2) #Elemente ausserhalb der Diagonale sind 0
sum(diag(cov(Z))) # Gleiche Summe wie orginale Daten

## R-Funktionen für PCA 
?prcomp
(res_pca <- prcomp(X))
res_pca$sdev^2
summary(res_pca)
str(res_pca)
res_pca$x

## retx Kalkulation der rotierten Elemente
(res_pca2 <- prcomp(X, retx=FALSE))
res_pca2$x

## Standardisierung
(res_pca3 <- prcomp(X, scale=TRUE))

## Plotten der Hauptkomponenten
plot(PC2~PC1, data=res_pca$x, xlab="PC1", ylab="PC2")

library(ggfortify)
autoplot(res_pca)
autoplot(res_pca, scale = 0)

## Arbeitsblatt 1: Aufgabe 2
###############################
load("Daten/body.Rdata")
## a)
## Scatterplotmatrix
pairs(body) 
library(scatterplot3d)
scatterplot3d(x=body$Groesse, y= body$Gewicht, z=body$Schuh, xlab="Grösse [m]", 
              ylab="Gewicht [kg]", zlab="Schuhgrösse")
## Man sieht das die Variablen stark korreliert sind.

## b) 
cov(body)
cor(body)
sum(diag(cov(body)))
## oder
var(body$Groesse)+var(body$Gewicht)+var(body$Schuh)

## c) 
pca <- prcomp(body)
pca
pca <- prcomp(body, center=TRUE)
pca

head(pca$x)
round(cov(pca$x),2)
pca$sdev^2
# d) 
plot(PC2~PC1, data=pca$x, xlab="PC1", ylab="PC2")
pairs(pca$x)
scatterplot3d(x=pca$x[,"PC1"], y= pca$x[,"PC2"], z=pca$x[,"PC3"], 
              xlab="PC1", ylab="PC2", zlab="PC3")
library(ggfortify)
autoplot(pca, scale=0)

## e) 
plot(PC2~PC1, data=pca$x, xlab="PC1", ylab="PC2", 
     col=c(rep("blue", 500), rep("red",100)))
pairs(pca$x, col=c(rep("blue", 500), rep("red",100)))

## f) 
round(cov(pca$x),3)
## Summe der Varianz
sum(diag(cov(pca$x)))
## Anteil der ersten beiden PCs
sum(diag(cov(pca$x))[1:2])/sum(diag(cov(pca$x)))
## oder direkt
summary(pca)

## g) 
# Standardisierung
body_sta <- (body -matrix(apply(body, 2, FUN=mean), ncol=3,
                        nrow=dim(body)[1],byrow=TRUE))/
  matrix(apply(body, 2, FUN=sd),  ncol=3, nrow=dim(body)[1],
         byrow=TRUE)
# Alternative
body_sta2 <- scale(body, center = TRUE, scale = TRUE)
#Kovarianzmatrix
cov(body_sta)
sum(diag(cov(body_sta)))
# PCA
(pca_sta <- prcomp(body_sta))
#Alternative
(pca_sta2 <- prcomp(body, scale=TRUE))
summary(pca_sta)
plot(PC2~PC1, data=pca_sta$x, xlab="PC1", ylab="PC2", 
     col=c(rep("blue", 500), rep("red",100)))
pairs(pca_sta$x, col=c(rep("blue", 500), rep("red",100)))
# Summe der Varianz
sum(diag(cov(pca_sta$x)))
# Anteil der ersten beiden PCs
summary(pca_sta)

## g)
pca$rotation
pca_sta$rotation

## Arbeitsblatt 1: Aufgabe 3
###############################

load("Daten/abst.Rdata")

##a)
dim(abst)
colnames(abst)
summary(apply(abst, MARGIN=2, FUN=median))
summary(apply(abst, MARGIN=2, FUN=sd))
windows()
par(mar=c(25, 4,2,1))
boxplot(abst, las=2, ylim=c(0,100), ylab="Ja-Anteil in %", cex.axis=0.8)
abline(h=50, lty=6)

## b) 
#Skalierung?
abst.pcS <- prcomp(abst, scale=F)
par(mar=c(4, 4, 2, 1), las=1)
plot(abst.pcS$x[,1], abst.pcS$x[,2], type="n", xlab="PC1", ylab="PC2")
text(abst.pcS$x[,1], abst.pcS$x[,2], labels=row.names(abst), cex=0.8)


## c) 
summary(abst.pcS)

## d) 
names(abst.pcS$sdev) <- paste("PC", 1:length(abst.pcS$sdev), sep="")
screeplot(abst.pcS) ## or
plot(abst.pcS$sdev^2, type="b")
abline(h=0, lty=2)

## USArrest
##################
head(USArrests[,c(1,2,4, 3)])
par(mar=c(4, 4, 2, 1), las=1)
P.pcR <- prcomp(USArrests[,c(1,2,4)],scale = TRUE)  
P.pcR
summary(P.pcR)
plot(P.pcR$x, pch=20, xlab="PC1", ylab="PC2", xlim=c(-3, 3.2))
text(P.pcR$x[,1], P.pcR$x[,2], labels=rownames(USArrests), cex=0.8, pos=4)
library(ggfortify)
autoplot(P.pcR)
## Biplot
biplot(P.pcR)


