# UCI Heart failure clinical records Data Set
# URL: https://archive.ics.uci.edu/ml/datasets/Heart+failure+clinical+records
# Dataset URL: https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv

# Datensatz laden und aufbereiten

#df <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv')
df <- read.csv('./Pruefung_C/data/heart_failure_clinical_records_dataset.csv')
df[c(2, 4, 6, 11)] <- sapply(df[c(2, 4, 6, 11)], as.logical)
df$sex <- factor(df$sex + 1, labels = c('f', 'm'), levels = c(1, 2))
df$outcome <- factor((df$DEATH_EVENT + 1), levels=c(1, 2), labels=c('survived', 'died'))


pairs(df)
summary(df)
table(df$sex)

# Altersverteilung
hist(df$age)
# Geschlechteraufteilung
round(mean(df$sex), digits = 2)
# Anämie [%]
round(mean(df$anaemia), digits = 2)
# Diabetiker [%]
round(mean(df$diabetes), digits = 2)
# Bluthochdruck-Patienten [%]
round(mean(df$high_blood_pressure), digits = 2)
# Raucher [%]
round(mean(df$smoking), digits = 2)
# Histogramme
par(mfrow = c(2, 3))
hist(df$age, xlab = 'Alter [Jahre]', xlim = c(35, 100), main = 'Altersverteilung')
hist(df$ejection_fraction, xlab = 'Auswurf-Fraktion [%]', main = 'Herzleistung')
hist(df$serum_creatinine, xlab = 'Creatinin [mg/dl]', main = 'Serum-Creatinin')
hist(df$serum_sodium, xlab = 'Na [mEq/l]', main = 'Serum-Natrium')
hist(df$platelets, xlab = expression(paste('[kiloplatelets/', mu, 'l]')), main = 'Thromozyten')
hist(df$creatinine_phosphokinase, xlab = expression(paste('CK im Blut [', mu, 'g/l]')), main = 'Creatin-Kinase')


#round(cov(df), 2)
#df_sta <- scale(df, center = TRUE, scale = TRUE)
#round(cov(df_sta), 2)


###############################################################################
###                  Visualisierung mit Dimensionsreduktion                 ###
###############################################################################

# 1. Ansatz
################# PCA #########################
# Princibal Component Analysis

# nur quantitative Daten verwenden!
pat.pca <- prcomp(df[,c(1, 3, 5, 7, 8, 9)], scale. = TRUE)
summary(pat.pca)

# plot (schwarz: ueberlebt, rot: verstorben)
plot(PC1~PC2, data = pat.pca$x, col=df$outcome, pch=16)

# data.frame mit outcome
plot.pca <- as.data.frame(pat.pca$x)
plot.pca$outcome <- df$outcome

# ggplot2
ggplot(plot.pca, aes(PC2, PC1, colour = outcome)) +
  geom_point() +
  ggtitle('2D PCA Visualisierung')
# ==> keine deutliche Gruppierung


# 2. Ansatz
################# MDS #########################
# Dissimilarity Matrix Calculation

# Trennung der Patientendaten vom Outcome
pat.dat <- df[, 1:12]

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
pat.tSNE <- Rtsne(pat.dat, pca_scale = TRUE, perplexity = 50)
tsne.df <- as.data.frame(pat.tSNE$Y)

#names(d_tsne) <- c("tsne1","tsne2")
tsne.df$outcome <- df$outcome
library(ggplot2)
ggplot(data = tsne$df, aes(x = V1, y = V2, colour = outcome)) +
  geom_point() +
  ggtitle('2D t-SNE Visualisierung')
