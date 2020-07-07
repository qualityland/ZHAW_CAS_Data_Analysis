# UCI Heart failure clinical records Data Set
# URL: https://archive.ics.uci.edu/ml/datasets/Heart+failure+clinical+records
# Dataset URL: https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv

df <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv')

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
# 
par(mfrow = c(2, 3))
hist(df$age, xlab = 'Alter [Jahre]', xlim = c(35, 100), main = 'Altersverteilung')
hist(df$ejection_fraction, xlab = 'Auswurf-Fraktion [%]', main = 'Herzleistung')
hist(df$serum_creatinine, xlab = 'Creatinin [mg/dl]', main = 'Serum-Creatinin')
hist(df$serum_sodium, xlab = 'Na [mEq/l]', main = 'Serum-Natrium')
hist(df$platelets, xlab = expression(paste('[kiloplatelets/', mu, 'l]')), main = 'Thromozyten')
hist(df$creatinine_phosphokinase, xlab = expression(paste('CK im Blut [', mu, 'g/l]')), main = 'Creatin-Kinase')


round(cov(df), 2)
df_sta <- scale(df, center = TRUE, scale = TRUE)
round(cov(df_sta), 2)


# Dimensionsreduktion mit MDS
# Trennung des Datensatzes in Patienten.Daten und Patienten.Outcome
pat.dat <- df[, 1:12]
outcome <- factor(df[, 13], levels=c(0, 1), labels=c('überlebt', 'verstorben'))

## Distanzmatrix der Patientendaten

################# MDS #########################
# Dissimilarity Matrix Calculation
library(cluster)
pat.dist <- daisy(pat.dat, stand = TRUE)  # standardisiert
#pat.dist <- daisy(pat.dat)               # NICHT standardisiert

# Multidimensional Scaling
pat.mds <- cmdscale(pat.dist)
pat.df <- data.frame(pat.mds)

# plot
#plot(pat.df$X1, pat.df$X2, col = outcome)

# ggplot2
library(ggplot2)
plot_dat <- cbind(pat.df, outcome)

# schwarz: ueberlebt, blau: verstorben
ggplot(plot_dat, aes(X1, X2, colour = outcome)) +
  geom_point() +
  ggtitle('2D MDS Visualisierung')

################### isoMDS ########################
# dasselbe mit der nicht-metrischen MDS
library(MASS)
pat.mds <- isoMDS(pat.dist)
pat.df <- data.frame(pat.mds)
plot_dat <- cbind(pat.df, outcome)

# schwarz: ueberlebt, blau: verstorben
ggplot(plot_dat, aes(points.1, points.2, colour = outcome)) +
  geom_point() +
  ggtitle('2D isoMDS Visualisierung')

################### tSNE ########################
library(Rtsne)
set.seed(1)
restSNE <- Rtsne(pat.dat, pca_scale = TRUE, perplexity = 50)
d_tsne <- as.data.frame(restSNE$Y)
#names(d_tsne) <- c("tsne1","tsne2")
d_tsne$outcome <- outcome
library(ggplot2)
ggplot(data = d_tsne, aes(x = V1, y = V2, colour = outcome)) +
  geom_point() +
  ggtitle("2D t-SNE Visualisierung")
