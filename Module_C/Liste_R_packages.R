## Liste mit den relevanten Packages für das Modul C
####################################################


library(ggfortify)
####################
X <- matrix(c(-1.5, -0.5, -2, -1.5, -1, 0, 2.5, 0.5, 2, 1.5), ncol=2, byrow=TRUE)
res_pca <- prcomp(X)
## Funktion autoplot
autoplot(res_pca, scale=0) #Visualisierung PCA-Objekt
?autoplot.prcomp
#Biplot
autoplot(res_pca, label=TRUE, label.hjust = -0.5, loadings = TRUE, loadings.colour = 'red', loadings.label = TRUE)


library(corrplot)
#####################
# Korrelationsmatrix
corrplot(cor(dat), type="lower")

library("rrcov")
#####################
# robuste PCA
pca.rob <-  PcaHubert(x, k = 2)

library(cluster)
##################
## Gower distance 
dist1 <- daisy(flower, type=list(asymm=3))
dist2 <- daisy(flower, type=list(asymm=3, ordratio=c(7,8)))
## PAM-Clustering
res.pam <- pam(x, k=2)

library(MASS)
##############
## ordinale MDS 
isoMDS(eurodist) 

library(Rtsne)
################
## t-SNE
Rtsne(iris[!duplicated(iris[1:4]),1:4])

library(factoextra)
####################
## optimale Anzahl Cluster
set.seed(1)
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1.6, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
## Ellbogen-Methode
fviz_nbclust(x, kmeans, method="wss", k.max = 10)
## Silhouetten-Methode
fviz_nbclust(x, kmeans, method="silhouette")
## Gap-Statistik
fviz_nbclust(x, kmeans, method="gap_stat")


library(fpc)
####################
## PAM Clustering mit automatische Bestimmung der Anzahl Cluster
cl_pamk <- pamk(x, krange=2:5)


library(pheatmap)
####################
## Heatmap
pheatmap(x, scale="row")

library(mclust)
#############
## Modellbasiertes Clustern 
mc <- Mclust(Nclus)

library(dbscan)
##############
## DBSCAN
res_dbscan <- dbscan(x, eps = .5, minPts = 8)