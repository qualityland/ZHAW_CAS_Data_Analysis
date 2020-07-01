## Modul C: Data Mining mit Schwerpunkt auf Clustering und Klassifikation
###############################################################################
## R-Code Clustering
################################

##Demonstration K-means Algorithmus
#####################################
set.seed(3)
par(mfrow=c(4,1), mar=c(2,1,1,1))
x <- c(1,2,3,7,10,11,13)
plot(x=x, y=rep(0,7), pch=16, yaxt="n", ylab="", xlim=c(0,14))
## Objekte zufällig einem Cluster zuordnen
(g1 <- sample(c(1,2), size=7, replace=TRUE))
plot(x=x, y=rep(0,7), col=c("blue", "red")[g1], pch=16, xlim=c(0,14))
## Mittelwert bestimmen
(mean_1 <- aggregate(x, by=list(g1), FUN=mean))
points(x=mean_1$x, y=rep(0,2), col=c("blue", "red")[mean_1$Group.1], pch=1, cex=1.6)
## Objekte dem nächsten Zentrum zuordnen
(g2 <-ifelse((x - mean_1$x[1])^2<(x - mean_1$x[2])^2, 1, 2))
plot(x=x, y=rep(0,7), col=c("blue", "red")[g2], pch=16, xlim=c(0,14))
## Mittelwert bestimmen
(mean_2 <- aggregate(x, by=list(g2), FUN=mean))
points(x=mean_2$x, y=rep(0,2), col=c("blue", "red")[mean_2$Group.1], pch=1, cex=1.6)
## Objekte dem nächsten Zentrum zuordnen
(g3 <-ifelse((x - mean_2$x[1])^2<(x - mean_2$x[2])^2, 1, 2))
plot(x=x, y=rep(0,7), col=c("blue", "red")[g3], pch=16, xlim=c(0,14))


## Within-Cluster Variation
WCV <- 0
for (i in 1:length(unique(g3))){
  for (j in 1:sum(g3==i)){
    WCV <- WCV+1/sum(g3==i)*sum((x[which(g3==i)][j]-x[which(g3==i)][j:sum(g3==i)])^2)
  }
}
WCV

set.seed(17)
par(mfrow=c(3,1), mar=c(2,1,1,1))
(g1 <- sample(c(1,2), size=7, replace=TRUE))
plot(x=x, y=rep(0,7), col=c("blue", "red")[g1], pch=16, xlim=c(0,14))
#Mittelwert
(mean_1 <- aggregate(x, by=list(g1), FUN=mean))
points(x=mean_1$x, y=rep(0,2), col=c("blue", "red")[mean_1$Group.1], pch=1, cex=1.6)
(g2 <-ifelse((x - mean_1$x[1])^2<(x - mean_1$x[2])^2, 1, 2))
plot(x=x, y=rep(0,7), col=c("blue", "red")[g2], pch=16, xlim=c(0,14))
(mean_2 <- aggregate(x, by=list(g2), FUN=mean))
points(x=mean_2$x, y=rep(0,2), col=c("blue", "red")[mean_2$Group.1], pch=1, cex=1.6)
(g3 <-ifelse((x - mean_2$x[1])^2<(x - mean_2$x[2])^2, 1, 2))
plot(x=x, y=rep(0,7), col=c("blue", "red")[g3], pch=16, xlim=c(0,14))


## Within-Cluster Variation
WCV <- 0
for (i in 1:length(unique(g3))){
  for (j in 1:sum(g3==i)){
    WCV <- WCV+1/sum(g3==i)*sum((x[which(g3==i)][j]-x[which(g3==i)][j:sum(g3==i)])^2)
  }
}
WCV

## K-means in R
##################
?kmeans
cl <- kmeans(x, centers=2)
str(cl)
par(mfrow=c(1,1), mar=c(4,4,2,1))
plot(x=x, y=rep(0,7), col = c("blue", "red")[cl$cluster], pch=16, yaxt="n", ylab="", xlim=c(0,14))
points(cl$centers, y=c(0,0), col =c("blue", "red"), pch = 4, cex = 2)
cl$withinss
cl$tot.withinss

## 2-dim Beispiel
set.seed(32)
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1.6, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
(cl <- kmeans(x, 3))
plot(x, col = cl$cluster, las=1, pch=16)
points(cl$centers, col = 1:3, pch = 8, cex = 2)
cl$withinss
cl$tot.withinss

## Arbeitsblatt 3: Aufgabe 1: MNIST
##########################################
load('Daten/mnist_2k.Rdata')

##  Übersicht
dim(x)
head(x,n=1)
x$label <- as.factor(x$label)
table(x$label)
image_nummer <- 485 # hier können Sie ein Bild von 1 bis 2000 auswählen 
bild <- matrix(as.numeric(x[image_nummer, 2:785]), ncol=28, byrow = TRUE)
image(t(bild[28:1,1:28]))
x$label[image_nummer]

##a)
set.seed(79)
res_cl <- kmeans(x[, -1], centers=10)
res_cl$tot.withinss
library(Rtsne)
restSNE <- Rtsne(x[, -1], perplexity = 20)
df <- as.data.frame(restSNE$Y)
names(df) <- c("tsne1","tsne2")
df$label <- as.factor(x$label)
df$cluster <- as.factor(res_cl$cluster)
library(ggplot2)
ggplot(data = df, aes(x = tsne1, y = tsne2, col=cluster, label=label)) + 
  geom_text(size=3) +
  ggtitle("2D t-SNE Visualisierung mit Ergebnissen aus k-means mit 10 Clustern")

##b)
table(df$label, LETTERS[df$cluster] )


## Clustering auf dem Output von t-SNE
res_cl2 <- kmeans(restSNE$Y, centers=10)
table(df$label, LETTERS[res_cl2$cluster] )

## Anzahl Cluster
##############################
set.seed(32)
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1.6, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
plot(x)
## Within-Cluster-Variation zur Bestimmung der Anzahl Cluster
wcv <-  rep(0, 10) # Initializierung
for (i in 1:10) 
  wcv[i] <- sum(kmeans(x, centers = i)$withinss)

plot(1:10, wcv, type = "b", xlab = "Anzahl Cluster", ylab = "within-cluster Variation", las=1) 
# Alternative
library(factoextra)
fviz_nbclust(x, kmeans, method="wss", k.max = 10)

## Silhouetten-Plot
cl <- kmeans(x, centers=3)
library(cluster)
plot(silhouette(cl$cluster, dist(x)))

## mittlere Silhouetten-Breite zur Bestimmung der Anzahl Cluster
sil <-  rep(0, 15) # Initializierung
for (i in 2:15) sil[i] <- summary(silhouette(x=kmeans(x, centers=i)$cluster, dist=dist(x)))$avg.width
plot(x=1:15, sil, type = "b", xlab = "Anzahl Cluster", ylab = "mittlere Silhouetten-Breite", las=1)
# Alternative
library(factoextra)
fviz_nbclust(x, kmeans, method="silhouette")
## Weitere Alternative zur Bestimmung der Anzahl Cluster
fviz_nbclust(x, kmeans, method="gap_stat")

## Arbeitsblatt 3: Aufgabe 2: Abstimmungen
##########################################
## a)
load("Daten/abst.Rdata")
set.seed(7)
## Bestimmen der optimalen Clusteranzahl
wcv <-  rep(NA, 20) # Initializierung
for (i in 1:20) 
  wcv[i] <- kmeans(abst, centers = i)$tot.withinss
plot(1:20, wcv, type = "b", xlab = "Anzahl Cluster", ylab = "within-cluster Variation", las=1)
#Alternative
library(factoextra)
fviz_nbclust(abst, kmeans, method="wss")

##b)
res.km <- kmeans(abst, 7)
## Visualisierung mit PCA
abst.pca <- prcomp(abst, scale=FALSE)
plot(abst.pca$x[,1], abst.pca$x[,2], type="n", xlab="PC1", ylab="PC2")
text(abst.pca$x[,1], abst.pca$x[,2], labels=row.names(abst), cex=0.8, col=c("green", "blue", "red", "orange", "brown", "black", "yellow")[res.km$cluster])
points(predict(abst.pca, newdata=res.km$centers), col=c("green", "blue", "red", "orange", "brown", "black", "yellow"), pch=4)

##c)
library(cluster)
plot(silhouette(x=res.km$cluster, dist=dist(abst)))
## mit nur 2 Clustern
res.km2 <- kmeans(abst, 2)
plot(silhouette(x=res.km2$cluster, dist=dist(abst)))

##d)
library(factoextra)
fviz_nbclust(abst, kmeans, method="silhouette")

## K-Medoids Clustering
#####################
set.seed(4)
par(mfrow=c(4,1), mar=c(2,1,1,1))
x <- c(1,2,3,7,10,11,13)
plot(x=x, y=rep(0,7), pch=16, yaxt="n", ylab="", xlim=c(0,14))
## k Objekte zufällig als Medoids
(m1 <- sample(1:7, size=2, replace=FALSE))
points(x=x[m1], y=rep(0,2), col=c("blue", "red"), pch=8, cex=2)
## Distanz zum Medoids
(g1 <- ifelse(abs(x-x[m1[1]])<abs(x-x[m1[2]]), 1,2))
plot(x=x, y=rep(0,7), pch=16, yaxt="n", ylab="", xlim=c(0,14), col=c("blue", "red")[g1])
points(x=x[m1], y=rep(0,2), col=c("blue", "red"), pch=8, cex=2)
## Kosten
Kosten <- 0
for (i in 1:length(unique(g1))){
  for (j in 1:sum(g1==i)){
    Kosten <- Kosten+ 1/length(x)*abs(x[which(g1==i)][j]-x[m1[i]])
  }
}
Kosten

#SWAP (Beispiel, kein Algorithmus!)
m2 <- c(2, 5)
plot(x=x, y=rep(0,7), pch=16, yaxt="n", ylab="", xlim=c(0,14), col=c("blue", "red")[g1])
points(x=x[m2], y=rep(0,2), col=c("blue", "red"), pch=8, cex=2)
Kosten <- 0
for (i in 1:length(unique(g1))){
  for (j in 1:sum(g1==i)){
    Kosten <- Kosten+ 1/length(x)*abs(x[which(g1==i)][j]-x[m2[i]])
  }
}
Kosten
g2 <- ifelse(abs(x-x[m2[1]])<abs(x-x[m2[2]]), 1,2)
plot(x=x, y=rep(0,7), pch=16, yaxt="n", ylab="", xlim=c(0,14), col=c("blue", "red")[g2])
points(x=x[m2], y=rep(0,2), col=c("blue", "red"), pch=8, cex=2)


## K-Medoids/PAM in R
##################
library(cluster)
res.pam <- pam(x, k=2)
res.pam

res.pam$clustering
res.pam$medoids
res.pam$objective

## Alternative für grössere Datensätze
res.clara <- clara(x, k=2)
res.clara

## 2-dim Beispiel
set.seed(32)
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1.6, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
library(cluster)
(cl_pam <- pam(x, 3))
par(mfrow=c(1,1), mar=c(4,4,2,1))
plot(x, col = cl_pam$cluster, las=1, pch=16)
points(cl_pam$medoids, col = 1:3, pch = 8, cex = 2)

## Anzahl Cluster
## mittlere Silhouetten-Breite zur Bestimmung der Anzahl Cluster
sil <-  rep(0, 15) # Initializierung
for (i in 2:15) sil[i] <- summary(silhouette(x=pam(x, k=i)$cluster, dist=dist(x)))$avg.width
plot(x=1:15, sil, type = "b", xlab = "Anzahl Cluster", ylab = "mittlere Silhouetten-Breite", las=1)
# Alternative
library(factoextra)
fviz_nbclust(x, pam, method="silhouette")

## direkt
library(fpc)
cl_pamk <- pamk(x, krange=2:5)
cl_pamk
str(cl_pamk)
cl_pamk$pamobject$clustering
plot(x, col = cl_pamk$pamobject$clustering, las=1)
points(cl_pamk$pamobject$medoids, col = 1:2, pch = 8, cex = 2)


## Arbeitsblatt 3: Aufgabe 3
##########################################
load("Daten/voting_NR.Rdata")

dim(as.matrix(NR_voting))
199*199
length(NR_voting)
r.pam <- pam(NR_voting, k=2)

r.pam
r.mds <- cmdscale(NR_voting, k=2)
plot(r.mds, col=(1:7)[NR_meta$Fraktion], pch=c(1,8)[r.pam$clustering])

plot(silhouette(x=r.pam$clustering, dist=NR_voting))

summary(silhouette(x=r.pam$clustering, dist=NR_voting))$avg.width

##a
set.seed(10)
sil <-  rep(0, 15) # Initializierung
for (i in 2:15) sil[i] <- summary(silhouette(x=pam(NR_voting, k=i)$clustering, 
                                             dist=NR_voting))$avg.width
plot(x=1:15, sil, type = "b", xlab = "Anzahl Cluster", 
     ylab = "mittlere Silhouetten-Breite", las=1)
abline(v=which.max(sil))
res.pam <- pam(NR_voting, k=6)
#Alternative
library(fpc)
res.pamk <- pamk(NR_voting)

## b) 
res.pam$medoids


## c) 
set.seed(20)
##MDS
res.cmd <- cmdscale(NR_voting) 
plot(res.cmd, pch=20, main="MDS", xlab="", ylab="", col=res.pam$clustering)
points(res.cmd[res.pam$medoids,], pch=8, col=1:6, cex=2)

res <- cmdscale(NR_voting)
df <- data.frame(res)
plot(df$X1, df$X2, type = 'n')
text(df$X1, df$X2, label=rownames(df), cex=0.6, col=res.pam$clustering)

##t-SNE
tsne_dist <- Rtsne(NR_voting, PCA=FALSE, is_distance=TRUE, perplexity=15, max_iter=3000)
plot(tsne_dist$Y, pch=20, main="t-SNE", xlab="", ylab="", col=res.pam$clustering)
points(tsne_dist$Y[res.pam$id.med,], pch=4, col=1:6)


## d) 
table(NR_meta$Fraktion, res.pam$clustering)


## Hierarchische Clusteranalyse
################################
## Demonstration Aufgabe
x <- c(1,3,6,6.5)
names(x) <- c('1','3','6', '6.5')
plot(x=x, y=rep(0,4), pch=1,  ylab="", xlim=c(0,8), ylim=c(0,4), las=1)
(d <- dist(x))
d[which.min(d)]
segments(x0=6, x1=6.5, y0=0.5, y1=0.5, col="blue")
segments(x0=6, x1=6, y0=0, y1=0.5, col="blue")
segments(x0=6.5, x1=6.5, y0=0, y1=0.5, col="blue")
## Single-Linkage
x2 <- c(1,3,6)
(d2 <- dist(x2))
d2[which.min(d2)]
segments(x0=1, x1=3, y0=2, y1=2, col="green")
segments(x0=1, x1=1, y0=0, y1=2, col="green")
segments(x0=3, x1=3, y0=0, y1=2, col="green")
## Single-Linkage
x3 <- c(3,6)
(d3 <- dist(x3))
d3[which.min(d3)]
segments(x0=2, x1=6.25, y0=3, y1=3, col="red")
segments(x0=2, x1=2, y0=2, y1=3, col="red")
segments(x0=6.25, x1=6.25, y0=0.5, y1=3, col="red")


## Hierarchisches Cluster in R
###########################################
h_cluster <- hclust(d, method = 'single')
summary(h_cluster)
plot(h_cluster)
plot(h_cluster, las=1, hang=-1)


## Cluster einteilen
## Anzahl Gruppen
cutree(h_cluster, k=3)  
cutree(h_cluster, k=2)  
## Höhe Schneidhöhe
cutree(h_cluster, h=1)
cutree(h_cluster, h=2.5)
## Cluster unterteilen
rect.hclust(h_cluster, k=3, border="red")

## 2-dim Beispiel
set.seed(32)
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1.6, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
plot(x)
d <- dist(x)
hc <-  hclust(d, method="ward.D2") 
plot(hc, las=1, hang=-1)
grhc <-  cutree(hc, h=5)
grhc <-  cutree(hc, k=3)
grhc
rect.hclust(hc, k=3, border="red")
plot(x, col = grhc, las=1)

## Arbeitsblatt 3: Aufgabe 4
##########################################
load('Daten/CountriesDis.RDA')   
CD.dis
## a)
c.single <-  hclust(CD.dis, method="single")
c.average <-  hclust(CD.dis, method="average")
c.complete <-  hclust(CD.dis, method="complete")
c.ward <-  hclust(CD.dis, method="ward.D2")
par(mfrow=c(2,2))
plot(c.single, labels=labels(CD.dis), main="Single", hang=-1, las=1)
rect.hclust(c.single, h=4, border="blue")
plot(c.average, labels=labels(CD.dis), main="Average", hang=-1, las=1)
rect.hclust(c.average, h=5, border="blue")
plot(c.complete, labels=labels(CD.dis), main="Complete", hang=-1, las=1)
rect.hclust(c.complete, h=5.5, border="blue")
plot(c.ward, labels=labels(CD.dis), main="Ward", hang=-1, las=1)
rect.hclust(c.ward, h=8, border="blue")
 
## b)
par(mfrow=c(1,2))
## Klassische MDS
res.cmd <- cmdscale(CD.dis) 
plot(res.cmd, pch=20, main="Klassische MDS", xlab="", ylab="", ylim=c(-2,4), col=cutree(c.ward, k=3))
text(res.cmd, labels=rownames(res.cmd), pos=3)
## Orindale MDS 
library("MASS")
res.iso <- isoMDS(CD.dis) 
plot(res.iso$points, pch=20, main="Ordinale MDS", xlab="", ylab="", ylim=c(-2,4), col=cutree(c.ward, k=3))
text(res.iso$points, labels=rownames(res.cmd), pos=3)

library(ggplot2)
df <- data.frame(res.iso$points)
colnames(df) <- c("x","y")
df$cluster <- as.factor(cutree(c.ward, k=3))
df$name <- rownames(df)
ggplot(df, aes(x=x, y=y, colour=cluster, label=name))+
  geom_point()+
  geom_text()

## c)
gr_complete <-  cutree(c.complete, h=5.5)
plot(silhouette(x=gr_complete, dist=CD.dis))

## d)
# Anzahl Cluster
sil <-  rep(0, 8) # Initializierung
for (i in 2:8) sil[i] <- summary(silhouette(x=pam(CD.dis, k=i)$cluster, dist=CD.dis))$avg.width
plot(x=1:8, sil, type = "b", xlab = "Anzahl Cluster", ylab = "mittlere Silhouetten-Breite", las=1)
# 3 Cluster
# PAM
set.seed(7)
res.pam <- pam(CD.dis, k=3)
#Visulisierung
res.cmd <- cmdscale(CD.dis) 
plot(res.cmd, pch=20, main="Lineare MDS", col=res.pam$cluster, xlab="", ylab="")
text(res.cmd, labels=rownames(res.cmd), pos=3)
points(res.cmd[rownames(res.cmd) %in% res.pam$medoids,],  pch = 8, cex = 2)




## Headmap
#####################
## Beispiel
dat <- read.table("Daten/cars_tab.txt", sep="\t",header=T)
str(dat)
table(dat$Country)

## nur numerische Merkmalsspalten selektieren
dat_numerisch <- dat[,3:8] 

## Daten transponieren
(x <- t(as.matrix(dat_numerisch )))
colnames(x) <- dat$Car

## Standard
heatmap(x, distfun = dist, hclustfun = hclust, scale="row") # complete linkage
heatmap(x, distfun = dist, hclustfun =function(d) hclust(d, method="ward.D2"), scale="row")
 
## schöne Alternative: Pheatmap   
library(pheatmap)
?pheatmap
pheatmap(x, scale="row")

## Ergänzen mit den Herkunftsländern
annot_col <- data.frame(Country=dat$Country)
## Verknüpfung annot_col mit colnames von x 
rownames(annot_col) <- colnames(x)
pheatmap(x, scale="row", annotation_col=annot_col)

pheatmap(x, scale="column", annotation_col=annot_col)

pheatmap(x, scale="row", annotation_col = annot_col,
         clustering_method = "ward.D2",
         clustering_distance_cols = "manhattan",
         clustering_distance_rows = "euclidean")

## Arbeitsblatt 3: Aufgabe 5: Heatmap
##########################################
load("Daten/abst.Rdata")
## Standard
heatmap(as.matrix(abst), scale="row")
heatmap(as.matrix(abst), scale="none")
heatmap(as.matrix(abst), scale="column")
##Elegante Alternative 
library(pheatmap)
pheatmap(abst, scale = "none")
pheatmap(abst, scale="column")

## Modellbasiertes Clustern 
############################
data(Nclus, package = "flexclust")
plot(Nclus)
library(mclust)
mc <- Mclust(Nclus, modelNames="EII") # spezifisches Modell
plot(mc, what="classification")
mc <- Mclust(Nclus) # Test aller Modelle
mc$modelName # bestes Modell
mc$classification
plot(mc, what="BIC")
plot(mc, what="classification")

## Alternative Visualisierung
library(factoextra)
fviz_mclust(mc, what="BIC",  palette = "jco")
fviz_mclust(mc, what="classification", geom = "point",  palette = "jco", xlab="x1", ylab="x2")

## Unsicherheit
mc$uncertainty
which.max(mc$uncertainty)
plot(mc, what="uncertainty")
points(x=mc$data[which.max(mc$uncertainty),1], y=mc$data[which.max(mc$uncertainty),2])
fviz_mclust(mc, "uncertainty",   palette = "jco", xlab="x1", ylab="x2")


## Arbeitsblatt 3: Aufgabe 6: Diabetes
##########################################

library(mclust)
head(diabetes)
?diabetes
diab_sd <- scale(diabetes[, -1]) # standardisieren der Daten


## a) 
data("diabetes")
pairs(diabetes[,-1],col=diabetes[,1])
boxplot(diabetes[,-1])
## PCA für visualisierung
res.pca <- prcomp(diabetes[,-1], scale=TRUE)
plot(res.pca$x, pch=20, main="Klassen", col=diabetes$class)
legend("topright", legend=levels(diabetes$class), col=1:3, pch=20)

## b) 
diab_sd <- scale(diabetes[, -1])
## K-Means
par(mfrow=c(1,2))
library(factoextra)
fviz_nbclust(diab_sd, kmeans, method="silhouette")
fviz_nbclust(diab_sd, kmeans, method="wss")
res.kmeans3 <- kmeans(diab_sd, centers=3)
plot(res.pca$x, pch=20, main="K-Means mit 3 Gruppen", col=res.kmeans3$cluster)
table(Cluster=res.kmeans3$cluster, diabetes$class)
res.kmeans5 <- kmeans(diab_sd, centers=5)
plot(res.pca$x, pch=20, main="K-Means mit 5 Gruppen", col=res.kmeans5$cluster)
table(Cluster=res.kmeans5$cluster, diabetes$class)
## PAM
library(cluster)
fviz_nbclust(diab_sd, pam, method="silhouette")
res.pam2 <- pam(diab_sd, k=2)
plot(res.pca$x, pch=20, main="pam mit 2 Gruppen", col=res.pam2$clustering)
table(Cluster=res.pam2$clustering, diabetes$class)
res.pam3 <- pam(diab_sd, k=3)
plot(res.pca$x, pch=20, main="pam mit 3 Gruppen", col=res.pam3$clustering)
table(Cluster=res.pam3$clustering, diabetes$class)
## Hierarchisches Clustering
res.hc <-  hclust(dist(diab_sd), method="ward.D2") 
plot(res.hc)
rect.hclust(res.hc, k=3, border="red")
plot(res.pca$x, pch=20, main="Hierarchisches Clustering  mit 3 Gruppen", col=cutree(res.hc, k=3))
table(Cluster=cutree(res.hc, k=3), diabetes$class) 

# c) 
res.mc <- Mclust(diab_sd) 
plot(res.mc, what="BIC")
fviz_mclust(res.mc, "BIC",  palette = "jco")
plot(res.mc, what="classification")
fviz_mclust(res.mc, "classification", geom = "point",  palette = "jco", xlab="x1", ylab="x2")
table(Cluster=res.mc$classification, diabetes$class)

## DBSCAN
##############
library(dbscan)

## Beispiel
set.seed(24)
library(mvtnorm)
mu_a <- c(0,-0.5)
mu_b <- c(2.5,0.25)
sig_a <- matrix(c(0.4,0.2,0.2,0.5), nrow=2)
sig_b <- matrix(c(0.08,-0.05, -0.05,0.08), nrow=2)
out_a<-rmvnorm(n=200,mean=mu_a,sigma=sig_a)
out_b<-rmvnorm(30,mean=mu_b,sigma=sig_b)
x <- rbind(out_a, out_b)

plot(x, col=c(rep("red", 200), rep("blue", 30)), xlim=c(-3,5), ylim=c(-2.5,1.9))
res_dbscan <- dbscan(x, eps = .5, minPts = 8)
plot(x, col=res_dbscan$cluster+1, xlim=c(-3,5), ylim=c(-2.5,1.9), las=1)


## Arbeitsblatt 3: Aufgabe 7: DBscan
##########################################

# a)	
library(factoextra)
data("multishapes")
head(multishapes)
plot(multishapes[,1:2], col=multishapes$shape)

# b) 
shape_kmeans <- kmeans(multishapes[,1:2], centers = 5)
plot(multishapes[,1:2], col=shape_kmeans$cluster)

# c) 
library(dbscan)
res.dbscan <- dbscan::dbscan(multishapes[,1:2], eps = 0.15, minPts = 5)
plot(multishapes[,1:2], col=res.dbscan$cluster+1)
table(multishapes$shape, res.dbscan$cluster)

# d)
library(mclust)
mc <- Mclust(multishapes[,1:2], G=1:15) 
plot(mc, what="BIC")
fviz_mclust(mc, "BIC",  palette = "jco")
plot(mc, what="classification")
fviz_mclust(mc, "classification", geom = "point",  xlab="x1", ylab="x2")
table(multishapes$shape, mc$classification)
