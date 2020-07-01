# Aufgabe 1 - 

data.path <- '/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_C/data/'
load(paste0(data.path, 'mnist_2k.RData'))

#cl <- kmeans(x[, -1], centers=10)

#plot(x, col = cl$cluster, las=1)


#library(cluster)
#plot(silhouette(x=cl$cluster, dist=dist(x)))

set.seed(79)
label <- x[, 1]
res_cl <- kmeans(x[, -1], centers=10)
res_cl$tot.withinss
library(Rtsne)
res_tSNE <- Rtsne(x[, -1], perplexity = 20)
df <- as.data.frame(res_tSNE$Y)
names(df) <- c('tsne1', 'tsne2')
df$label <- as.factor(x$label)
df$cluster <- as.factor(res_cl$cluster)

library(ggplot2)
ggplot(data=df, aes(x = tsne1, y = tsne2, col=cluster, label=label))  +
  geom_text(size=3) +
  ggtitle("2D t-SNE Vizualizierung mit k-menas mit 10 Clustern")



# Aufgabe 2 - 

data.path <- '/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_C/data/'
load(paste0(data.path, 'abst.RData'))
str(abst)

# a)
library(factoextra)
fviz_nbclust(abst, kmeans, method="wss")

abst.km <- kmeans(abst, 7)

abst.pca <- prcomp(abst, scale.=F)



# Aufgabe 3 - 

data.path <- '/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_C/data/'
load(paste0(data.path, 'voting_NR.Rdata'))

# a)
# Führen Sie mit dem K-Medoids/PAM Ansatz das Clustering für die Distanzmatrix
# NR_voting durch. Wie viele Klassen sind hier angebracht?
libarary(fpc)
nr.pam.k <- fpc::pamk(NR_voting)
nr.pam.k

library(factoextra)
fviz_nbclust(NR_voting, kmeans, method="silhouette")

library(cluster)
nr_pam <- pam(NR_voting, k=10)
plot(NR_voting, col = nr_pam$clustering, main="pam")
points(nr_pam$medoids, col = 1:3, pch = 8, cex = 2)

nr_clara <- clara(NR_voting, k=3)
plot(dat, col = nr_clara$clustering, main="clara")
points(cl_clara$medoids, col = 1:3, pch = 8, cex = 2)

# b)
# Was sind die repräsentativen Mitglieder der Cluster (Medoid)?


# c)
# Visualisieren Sie das Ergebnis der Clusteranalyse mit MDS (cmdscale)
# oder t-SNE (Rtsne).



# Aufgabe 4 -

data.path <- '/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_C/data/'
load(paste0(data.path, 'CountriesDis.RDA'))

# a)
# Führen Sie mit dieser Unähnlichkeitsmatrix hierarchische Cluster-Analysen durch,
# verwenden Sie verschiedene Linkage-Methoden. Vergleichen Sie dabei die Resultate
# der Cluster-Methoden single, complete, average und ward.D2 bezüglich der Gruppenbildung.
cd.hcl.s <- hclust(dist(CD.dis), method = 'single')
plot(cd.hcl.s, las=1, hang=-1)

cd.hcl.c <- hclust(dist(CD.dis), method = 'complete')
plot(cd.hcl.c, las=1, hang=-1)

cd.hcl.w <- hclust(dist(CD.dis), method = 'ward.D')
plot(cd.hcl.w, las=1, hang=-1)

cd.hcl.w2 <- hclust(dist(CD.dis), method = 'ward.D2')
plot(cd.hcl.w2, las=1, hang=-1)



klassen <- cutree(h_cluster, k=3)
rect.hclust(h_cluster, k=3, border="red")


# b) Inwiefern finden Sie in einer multidimensionalen Skalierung die Resultate der
# hierarchischen Cluster-Analyse wieder?



# Aufgabe 6 - Diabetes
library(mclust)

# a)
# Stellen Sie die Daten graphisch dar. Was sehen Sie?
# Macht es Sinn die Daten für das Clustering zu normalisieren?
pairs(diabetes[, -1], col=diabetes[,1])
legend('topleft', legend = levels(diabetes$class), col = c(1, 2, 3))
