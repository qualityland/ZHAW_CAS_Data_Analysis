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

abst.pca <- prcomp(abs, scale.=F)

