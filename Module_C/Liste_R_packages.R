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

library(MASS)
##############
## ordinale MDS 
isoMDS(eurodist) 

library(Rtsne)
################
## t-SNE
Rtsne(iris[!duplicated(iris[1:4]),1:4])
