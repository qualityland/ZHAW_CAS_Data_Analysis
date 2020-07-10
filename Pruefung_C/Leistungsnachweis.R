# UCI Heart failure clinical records Data Set
# URL: https://archive.ics.uci.edu/ml/datasets/Heart+failure+clinical+records
# Dataset URL: https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv

# Datensatz laden und aufbereiten

#df <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv')
df <- read.csv('./Pruefung_C/data/heart_failure_clinical_records_dataset.csv')
df[c(2, 4, 6, 11)] <- sapply(df[c(2, 4, 6, 11)], as.logical)
df$sex <- factor(df$sex + 1, labels = c('f', 'm'), levels = c(1, 2))
df$outcome <- factor((df$DEATH_EVENT + 1), levels=c(1, 2), labels=c('survived', 'deceased'))

# orientierende Analyse der Daten
# pairs(df)
# summary(df)
# table(df$sex)
# 
# # Altersverteilung
# hist(df$age)
# # Geschlechteraufteilung
# round(mean(df$sex), digits = 2)
# # Anämie [%]
# round(mean(df$anaemia), digits = 2)
# # Diabetiker [%]
# round(mean(df$diabetes), digits = 2)
# # Bluthochdruck-Patienten [%]
# round(mean(df$high_blood_pressure), digits = 2)
# # Raucher [%]
# round(mean(df$smoking), digits = 2)
# # Histogramme
# par(mfrow = c(2, 3))
# hist(df$age, xlab = 'Alter [Jahre]', xlim = c(35, 100), main = 'Altersverteilung')
# hist(df$ejection_fraction, xlab = 'Auswurf-Fraktion [%]', main = 'Herzleistung')
# hist(df$serum_creatinine, xlab = 'Creatinin [mg/dl]', main = 'Serum-Creatinin')
# hist(df$serum_sodium, xlab = 'Na [mEq/l]', main = 'Serum-Natrium')
# hist(df$platelets, xlab = expression(paste('[kiloplatelets/', mu, 'l]')), main = 'Thromozyten')
# hist(df$creatinine_phosphokinase, xlab = expression(paste('CK im Blut [', mu, 'g/l]')), main = 'Creatin-Kinase')

###############################################################################
###                  Visualisierung mit Dimensionsreduktion                 ###
###############################################################################

### 1. Ansatz: PCA ############################################################
# Princibal Component Analysis

# nur numerische Daten verwenden!
pat.num <- df[,c(1, 3, 5, 7, 8, 9)]
pat.pca <- prcomp(pat.num, scale. = TRUE)
summary(pat.pca)

# plot (schwarz: ueberlebt, rot: verstorben)
plot(PC1~PC2, data = pat.pca$x, col=df$outcome, pch=16)
#plot(PC1~PC2, data = pat.pca$x, col=as.factor(pat.cl$cluster), pch=16)

# data.frame mit outcome
plot.pca <- as.data.frame(pat.pca$x)
plot.pca$outcome <- df$outcome

# ggplot2
library(ggplot2)
ggplot(plot.pca, aes(PC2, PC1, colour = outcome)) +
  geom_point() +
  ggtitle('2D PCA Visualisierung')
# ==> keine deutliche Gruppierung


# 2. Ansatz: MDS ##############################################################

# Dissimilarity Matrix Calculation

# Trennung der Patientendaten vom Outcome
#pat.dat <- df[, 1:12]      # auch nicht-numerische
pat.dat <- pat.num

library(cluster)
# Distanzmatrix erstellen
pat.dist <- daisy(pat.dat, stand = TRUE)  # standardisiert
#pat.dist <- daisy(pat.dat)               # NICHT standardisiert

# Multidimensional Scaling
pat.mds <- cmdscale(pat.dist)
pat.df <- data.frame(pat.mds)

# ggplot2
library(ggplot2)
plot.dat <- cbind(pat.df, outcome=df$outcome)
ggplot(plot.dat, aes(X1, X2, colour = outcome)) +
  geom_point() +
  ggtitle('2D MDS Visualisierung')
# Cluster-Viz.
ggplot(plot.dat, aes(X1, X2, colour = as.factor(pat.cl$cluster))) +
  geom_point() +
  ggtitle('2D MDS Visualisierung')

################### isoMDS ########################
# dasselbe mit der nicht-metrischen MDS
library(MASS)
pat.iMDS <- isoMDS(pat.dist)

# data.frame mit outcome
pat.idf <- data.frame(pat.iMDS)
plot.idat <- cbind(pat.idf, outcome=df$outcome)

# ggplot2
ggplot(plot.idat, aes(points.1, points.2, colour = outcome)) +
  geom_point() +
  ggtitle('2D isoMDS Visualisierung')


################### tSNE ########################

# t-Distributed Stochastic Neighbor Embedding
library(Rtsne)
set.seed(1)

# tSNE mit scaling
pat.tSNE <- Rtsne(pat.dat, pca_scale = TRUE, perplexity = 20)
tsne.df <- as.data.frame(pat.tSNE$Y)

#names(d_tsne) <- c("tsne1","tsne2")
tsne.df$anaemia <- df$anaemia
library(ggplot2)
ggplot(data = tsne.df, aes(x = V1, y = V2, colour = anaemia)) +
  geom_point() +
  ggtitle('2D t-SNE Visualisierung')

# library(ggplot2)
# pca.plot <- ggplot(plot.pca, aes(PC2, PC1, colour = outcome)) +
#   geom_point()
# 
# mds.plot <- ggplot(plot.dat, aes(X1, X2, colour = outcome)) +
#   geom_point()
# 
# tsne.plot <- ggplot(data = tsne.df, aes(x = V1, y = V2, colour = anaemia)) +
#   geom_point()
# 
# library(gridExtra)
# grid.arrange(pca.plot, mds.plot, tsne.plot, nrow = 1)
###############################################################################
###                                Clustering                               ###
###############################################################################
# Versuchen Sie Ihren Datensatz (oder Teile davon) mit einer geeigneten Methode zu clustern.


############## Geben Sie an, wie Sie die Anzahl Cluster festlegen #############

# Bestimmen der optimalen Cluster-Anzahl
### ueber Min. der within-cluster-variation
wcv <- rep(0, 15) # init
for (i in 1:15)
  wcv[i] <- sum(kmeans(pat.num, centers = i)$withinss)
plot(1:15, wcv, type = 'b', xlab = 'Clusteranzahl')
# opt. Clusteranzahl = 5

### Alternative mit fviz_nbclust()
library(factoextra)
fviz_nbclust(pat.num, kmeans, method = 'wss')
# auch 5

######################### Visualisieren Sie Ihre Cluster ######################
library(ggplot2)

# nur numerische Daten
pat.num <- df[,c(1, 3, 5, 7, 8, 9)]
pat.cl <- kmeans(pat.num, centers = 5)

### PCA
pat.pca <- prcomp(pat.num, scale. = TRUE)
plot.pca <- as.data.frame(pat.pca$x)
plot.pca$cluster <- as.factor(pat.cl$cluster)
ggplot(plot.pca, aes(PC2, PC1, colour = cluster)) +
  geom_point() +
  ggtitle('Cluster Visualisierung (PCA)')



### MDS
pat.mds <- cmdscale(pat.dist)
pat.df <- data.frame(pat.mds)
plot.dat <- cbind(pat.df, cluster=as.factor(pat.cl$cluster))
ggplot(plot.dat, aes(X1, X2, colour = cluster)) +
  geom_point() +
  ggtitle('Cluster Visualisierung (MDS)')







############ Heatmaps
pat.mat <- t(as.matrix(pat.num))
heatmap(pat.mat)

# alternativ
library(pheatmap)
pheatmap(pat.mat, scale = 'row', kmeans_k = 5)




# Bestimmen der optimalen Cluster-Anzahl
# ueber Min. der within-cluster-variation
wcv <- rep(0, 15) # init
for (i in 1:15)
  wcv[i] <- sum(kmeans(pat.num, centers = i)$withinss)
plot(1:15, wcv, type = 'b', xlab = 'Clusteranzahl')
# opt. Clusteranzahl = 5

# Alternative mit fviz_nbclust()
library(factoextra)
fviz_nbclust(pat.num, kmeans, method = 'wss')
# auch 5

library(Rtsne)
set.seed(1)
# tSNE mit Colorierung aus dem K-Means Cluster
pat.tSNE <- Rtsne(pat.dat, pca_scale = TRUE, perplexity = 20)
tsne.df <- as.data.frame(pat.tSNE$Y)
tsne.df$cluster <- pat.cl$cluster
library(ggplot2)
ggplot(data = tsne.df, aes(x = V1, y = V2, colour = as.factor(df$anaemia))) +
  geom_point() +
  ggtitle('2D t-SNE Visualisierung')



# Geben Sie an, wie Sie die Anzahl Cluster festlegen.
# Visualisieren Sie Ihre Cluster.
# Interpretieren Sie Ihre Ergebnisse. 

################### K-Means ########################

library(factoextra)
fviz_nbclust(pat.dist, kmeans, method = 'wss')


library(cluster)
pat.pam <- pam(pat.dist, k=2)
plot(pat.dist, col = pat.pam$clustering)
points(pat.pam$medoids, col = 1:2, pch = 8, cex = 2)


heatmap(as.matrix(pat.dat))
###############################################################################
###                             Klassifikation                              ###
###############################################################################

