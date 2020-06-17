## Modul C: Data Mining mit Schwerpunkt auf Clustering und Klassifikation
###############################################################################
## R-Code Distanzmasse, MDS und tSNE
#####################################

## Distanzen: daisy für flower
################################
library(cluster)
str(flower)
?daisy
dist1 <- daisy(flower, type=list(asymm=3))
str(dist1)
dist1
dist2 <- daisy(flower, type=list(asymm=3, ordratio=c(7,8)))
dist2

## Arbeitsblatt 2: Aufgabe 1
###############################

D.S <- data.frame(x1=c(10, 8, 11, 9, 12, 45, 55, 59, 52, 56, 58, 57),
                  x2=c(2.8, 5.2, 4.1, 10, 6.5,5.2,5.6,1.2, 9.5,11, 8.5, 4.9))
rownames(D.S) <- LETTERS[1:nrow(D.S)]

## a)
round(daisy(D.S),1)
round(dist(D.S),1)
## Damit wir einfach die gewünschten Distanzen sehen:
round(as.matrix(daisy(D.S))[c("D", "F", "G"), "A"],1)
# manuelle Berechnung
sqrt(sum((D.S[c("A"),] - D.S[c("D"),])^2))

## b) 
?daisy
round(as.matrix(daisy(D.S, stand=TRUE))[c("D", "F", "G"), "A"],1)
# Standardisierung in daisy
x <- scale(D.S, center = TRUE, scale = FALSE)
sx <- colMeans(abs(x), na.rm = TRUE)
sx
x <- scale(x, center = FALSE, scale = sx)
round(as.matrix(daisy(x))[c("D", "F", "G"), "A"],1)

## c) 
D.S2 <- data.frame(x1=c(10, 8, 11, 9, 12, 45, 55, 59, 52, 56, 58, 57),
                   x2=c(2.8, 5.2, 4.1, 10, 6.5,5.2,5.6,1.2, 9.5,11, 8.5, 4.9),
                   x3=c("a", "a", "a", "b", "b", "b", "c", "c", "c", "d", "d", "d"))
rownames(D.S2) <- LETTERS[1:nrow(D.S2)]

round(as.matrix(daisy(D.S2, metric="gower"))[c("D", "F", "G"), "A"],2)
round(as.matrix(daisy(D.S2))[c("D", "F", "G"), "A"],2)
# manuelle Berechnung für A-D
1/3*(sum(abs(D.S2[c("A"),1:2] - D.S2[c("D"),1:2])/apply(D.S[,1:2],2, FUN=function(x) diff(range(x)))) +(1-sum(D.S2[c("A"),3] == D.S2[c("D"),3])))


## d) 
round(as.matrix(daisy(D.S2, metric="gower", type=list(ordratio=1)))[c("D", "F", "G"), "A"],2)
#manuelle Umwandlung
D.S2$x1 <- as.ordered(D.S2$x1) 
round(as.matrix(daisy(D.S2, metric="gower"))[c("D", "F", "G"), "A"],2)     
# manuelle Berechnung A-D
D.S2$x1 <- (rank(D.S2[,1])-1)/(length(D.S2[,1])-1)
1/3*(sum(abs(D.S2[c("A"),1:2] - D.S2[c("D"),1:2])/apply(D.S2[,1:2],2, FUN=function(x) diff(range(x)))) +(1-sum(D.S2[c("A"),3] == D.S2[c("D"),3])))



## MDS für eurodist
##################
?eurodist
eurodist

res.cmd <- cmdscale(eurodist, k=2)
plot(res.cmd, pch="", xlim=c(-2300, 2400))
text(res.cmd, labels=rownames(res.cmd), cex=0.8)

## Spieglung an der horizonalen Achse
plot(x=res.cmd[,1], y=-res.cmd[,2], pch="", xlim=c(-2400, 2400))
text(x=res.cmd[,1], y=-res.cmd[,2], labels=rownames(res.cmd), cex=0.8)


## ordinale MDS
library(MASS)
res.iso <- isoMDS(eurodist) 
plot(x=res.iso$points[,1], y=-res.iso$points[,2], pch="", xlim=c(-2400, 2400), ylab="", xlab="", las=1)
text(x=res.iso$points[,1], y=-res.iso$points[,2], labels=rownames(res.cmd), cex=0.8)


## Arbeitsblatt 2: Aufgabe 2
###############################

# a) 
load("Daten/voting_NR.Rdata")
res <- cmdscale(NR_voting)
df <- data.frame(res)
plot(df$X1, df$X2, type = 'n')
text(df$X1, df$X2, label=rownames(df), cex=0.6, 
     col=c("yellow", "orange", "green", "black", "blue", "red", "darkgreen")[NR_meta$Fraktion])
legend("bottomright", legend=levels(NR_meta$Fraktion), bty="n", cex=0.5, 
       fill=c("yellow", "orange", "green", "black", "blue", "red", "darkgreen"))

plot(res$points)
text(res$points, label=rownames(df), cex=0.6, 
     col=c("yellow", "orange", "green", "black", "blue", "red", "darkgreen")[NR_meta$Fraktion])
# Alternative Visualisierung mit ggplot
plot_data <- cbind(df, NR_meta)
library(ggplot2)
ggplot(plot_data, aes(x=X1, y=X2, label=Name, colour=Fraktion))+
  geom_text(size=5) +
  geom_point() +
  scale_color_manual(values=c("yellow", "orange", "green", "black", "blue", "red", "darkgreen"))

library(MASS)
res <- isoMDS(NR_voting)
df <- data.frame(res)
plot(df$points.1, df$points.2, type = 'n')
text(df$points.1, df$points.2, label=rownames(df), cex=0.6, 
     col=c("yellow", "orange", "green", "black", "blue", "red", "darkgreen")[NR_meta$Fraktion])
legend("bottomright", legend=levels(NR_meta$Fraktion), bty="n", cex=0.5, 
       fill=c("yellow", "orange", "green", "black", "blue", "red", "darkgreen"))
# Alternative Visualisierung mit ggplot
plot_data <- cbind(df, NR_meta)
library(ggplot2)
ggplot(plot_data, aes(x=points.1, y=points.2, label=Name, colour=Fraktion))+
  geom_text(size=5) +
  geom_point() +
  scale_color_manual(values=c("yellow", "orange", "green", "black", "blue", "red", "darkgreen"))



## t-SNE
################
library(Rtsne)
?Rtsne
head(iris)
set.seed(85)
tsne_out <- Rtsne(iris[, 1:4])
tsne_out <- Rtsne(iris[!duplicated(iris[1:4]),1:4])
tsne_out
plot(tsne_out$Y, col=iris$Species[!duplicated(iris[1:4])])
tsne_out$itercosts
set.seed(82)
tsne_out <- Rtsne(iris[!duplicated(iris[1:4]),1:4])
plot(tsne_out$Y, col=iris$Species[!duplicated(iris[1:4])])
tsne_out$itercosts

## t-SNE direkt auf Datenmatrix
tsne_out <- Rtsne(iris[!duplicated(iris[1:4]),1:4], PCA=FALSE)
plot(tsne_out$Y, col=iris$Species[!duplicated(iris[1:4])])

## t-SNE aus Distanzmatrix
tsne_out <- Rtsne(dist(iris[,1:4]), is_distance = TRUE)
plot(tsne_out$Y, col=iris$Species[!duplicated(iris[1:4])])


## Arbeitsblatt 2: Aufgabe 3
###############################
load('Daten/mnist_2k.Rdata')
dim(x)
x$label <- as.factor(x$label)
table(x$label)
image_nummer <- 7 # hier können Sie ein Bild von 1 bis 2000 auswählen 
bild <- matrix(as.numeric(x[image_nummer, 2:785]), ncol=28, byrow = TRUE)
image(t(bild[28:1,1:28]))
x$label[image_nummer]

# a)
library(ggfortify)
x$label <- as.factor(x$label)
res_pca <- prcomp(x[, -1])   
autoplot(res_pca, data = x, colour = 'label', label.size = 3) + 
  ggtitle("2D Score-Plot resultierend aus einer PCA")

# b) 
res.cmd <- cmdscale(dist(x[, -1]), k=2) 
d_cmd <- as.data.frame(res.cmd) 
names(d_cmd) <- c("x1","x2")
d_cmd$label <- x$label
library(ggplot2)
ggplot(data = d_cmd, aes(x = x1, y = x2, col = label)) + 
  geom_point() +
  ggtitle("2D MDS Visualisierung")

# c) 
library(Rtsne)
set.seed(1)
restSNE <- Rtsne(x[, -1], perplexity = 20)
d_tsne <- as.data.frame(restSNE$Y)
names(d_tsne) <- c("tsne1","tsne2")
d_tsne$label <- x$label
library(ggplot2)
ggplot(data = d_tsne, aes(x = tsne1, y = tsne2, col = label)) + 
  geom_point() +
  ggtitle("2D t-SNE Visualisierung")

