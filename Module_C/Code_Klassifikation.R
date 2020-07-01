## Modul E: Data Mining mit Schwerpunkt auf Clustering und Klassifikation
###############################################################################
## R-Code Klassifikation
################################


## Einführungsbeispiel
########################
load("Daten/Studenten.Rdata")
head(Studenten)

## Aufteilung in Training und Testdaten
library(caret)
set.seed(25)
train_index <- createDataPartition(Studenten$sex, p = 0.7, list = FALSE)

train_set <- Studenten[train_index, ]
test_set <- Studenten[-train_index, ]

dim(test_set)
dim(train_set)

table(train_set$sex)
table(test_set$sex)
range(test_set$height)
range(train_set$height)

## Ansatz 1: raten
y_raten <- sample(c("Male", "Female"), dim(test_set)[1], replace = TRUE)
## Accuarcy auf Testdaten
mean(y_raten == test_set$sex)

## Analyse der Trainingsdaten
tapply(train_set$height, INDEX=train_set$sex, FUN=function(x) return(c(mean(x), sd(x))))



## Modell mit fixem Cutoff 
fun_cutoff <- function(x, cutoff) ifelse(x > cutoff, "Male", "Female")

y_2sd <- fun_cutoff(test_set$height, cutoff=157)
mean(y_2sd == test_set$sex)

y_mitte <- fun_cutoff(test_set$height, cutoff=160.5)
mean(y_mitte == test_set$sex)

## Optimierung Cutoff (Trainingsdaten)
cutoff <- seq(150, 200)
accuracy <- rep(NA, length(cutoff))
for (i in 1:length(cutoff)){
  y_hat <- fun_cutoff(train_set$height, cutoff=cutoff[i])
  accuracy[i] <- mean(y_hat == train_set$sex)
}
max(accuracy)
cutoff[which.max(accuracy)]

plot(x=cutoff, y=accuracy, type="o", las=1)
abline(v=cutoff[which.max(accuracy)], col="red")

## Testen an Testdaten
y_acc <- fun_cutoff(test_set$height, cutoff=163)
mean(y_acc == test_set$sex)

## Konfusionsmatrix
(tab <- table(predicted = y_acc, actual = test_set$sex))
confusionMatrix(data = as.factor(y_acc), reference =  test_set$sex)


## Genauigkeit nach Geschlecht
mean(y_acc[test_set$sex=="Male"] == test_set$sex[test_set$sex=="Male"])
mean(y_acc[test_set$sex=="Female"] == test_set$sex[test_set$sex=="Female"])

## Prevalence
table(Studenten$sex)/length(Studenten$sex)
table(test_set$sex)/length(test_set$sex)

## Precision/Relevanz 
## Wie viele der «entdeckten» Frauen sind korrekt.

TP <- tab[1,1]
TN <- tab[2,2]
FP <- tab[1,2]
FN <- tab[2,1]
(prec <- TP/(TP+FP))
precision(data = as.factor(y_acc), reference =  test_set$sex)

## Recall Sensitivity
## Wie viele der Frauen wurden entdeckt
(rec <- TP/(TP+FN))
recall(data = as.factor(y_acc), reference =  test_set$sex)

## Spezifität
## Wie viele der Männer wurden entdeckt
TN/(TN+FP)

## F1-Score
## Harmonisches Mittel von Recall und Precision
2*prec*rec/(prec+rec)
F_meas(data = as.factor(y_acc), reference =  test_set$sex)

## Kappa
## Relative Verbesserung gegenüber Zufallsprädikator
(observed_accuracy <- mean(y_acc == test_set$sex))
(expected_accuracy <-(TP+FP)/(TP+FP+FN+TN)*(TP+FN)/(TP+FP+FN+TN)+
  (FN+TN)/(TP+FP+FN+TN)*(FP+TN)/(TP+FP+FN+TN))
(expected_accuracy <- 1/sum(tab)^2* sum(rowSums(tab)*colSums(tab)))

(observed_accuracy - expected_accuracy)/(1 - expected_accuracy)

confusionMatrix(data = as.factor(y_acc), reference =  test_set$sex)$overall["Kappa"]

## Optimierung mit verschiedenen Metriken
cutoff <- seq(150, 200)
accuracy <- rep(NA, length(cutoff))
F1 <- rep(NA, length(cutoff))
Kappa <- rep(NA, length(cutoff))
for (i in 1:length(cutoff)){
  y_hat <- fun_cutoff(train_set$height, cutoff=cutoff[i])
  accuracy[i] <- confusionMatrix(data = as.factor(y_hat), 
                              reference =  train_set$sex)$overall["Accuracy"]
  F1[i] <-  F_meas(data = as.factor(y_hat), 
                   reference =  train_set$sex)
  Kappa[i] <- confusionMatrix(data = as.factor(y_hat), 
                           reference =  train_set$sex)$overall["Kappa"]
}


max(F1)
cutoff[which.max(F1)] 

plot(x=cutoff, y=accuracy, type="o", las=1, col="red", ylim=c(0,1),ylab = "Metrik")
abline(v=cutoff[which(accuracy==max(accuracy))], col="red", lty=2)
lines(x=cutoff, y=F1, type="o", las=1, col="blue")
abline(v=cutoff[which(F1==max(F1))], col="blue", lty=2)
lines(x=cutoff, y=Kappa, type="o", las=1, col="green")
abline(v=cutoff[which(Kappa==max(Kappa))], col="green", lty=3)
legend("topright", legend=c("Accuracy", "F1", "Kappa"), col=c("red", "blue", "green"), lwd=1)


## Demonstration knn
#######################
data <- data.frame(x= c(11,   14,  20, 30, 4, 9, 24, 28, 31, 34, 18, 13, 25),
                   klasse = c(rep("a",6),rep("b",7)))
head(data)
plot(x=data$x, y=rep(1, dim(data)[1]), col=c("green","red")[data$klasse], 
     pch=c(16, 18)[data$klasse], 
     xlab="", xlim=c(0, 35), xaxt="n", xaxs="i", yaxt="n", ylab="")
axis(1, at=seq(0, 35, 5), labels=seq(0, 35, 5))
grid(nx=35, ny=1, col="grey")
new <- data.frame(x=15)
points(x=new, y=1, pch=4)

## KNN 
k <- 1
table(data$klasse[order(abs(new$x-data$x))][1:k])
segments(x0=15-abs(new$x-data$x)[order(abs(new$x-data$x))][k], 
         x1=15+abs(new$x-data$x)[order(abs(new$x-data$x))][k], y0=0.95, y1=0.95, lwd=2, col="blue")
k <- 2
table(data$klasse[order(abs(new$x-data$x))][1:k])
segments(x0=15-abs(new$x-data$x)[order(abs(new$x-data$x))][k], 
         x1=15+abs(new$x-data$x)[order(abs(new$x-data$x))][k], y0=0.9, y1=0.9, lwd=2, col="blue")
k <- 3
table(data$klasse[order(abs(new$x-data$x))][1:k])
segments(x0=15-abs(new$x-data$x)[order(abs(new$x-data$x))][k], 
         x1=15+abs(new$x-data$x)[order(abs(new$x-data$x))][k], y0=0.85, y1=0.85, lwd=2, col="blue")
k <- 4
table(data$klasse[order(abs(new$x-data$x))][1:k])
segments(x0=15-abs(new$x-data$x)[order(abs(new$x-data$x))][k], 
         x1=15+abs(new$x-data$x)[order(abs(new$x-data$x))][k], y0=0.8, y1=0.8, lwd=2, col="blue")
k <- 5
table(data$klasse[order(abs(new$x-data$x))][1:k])
segments(x0=15-abs(new$x-data$x)[order(abs(new$x-data$x))][k], 
         x1=15+abs(new$x-data$x)[order(abs(new$x-data$x))][k], y0=0.75, y1=0.75, lwd=2, col="blue")


## KNN in R mit caret
####################
modelLookup("knn")
getModelInfo(model = "knn")

## Modelltraining
library(caret)
knn_fit <- train(klasse ~., data = data, method="knn", tuneGrid=data.frame(k=3))

knn_fit
## Vorhersage für neuen Wert
predict(knn_fit, newdata=new)

## Klassengrenzen
new_vec <- seq(0, 35, 0.1)

plot(x=data$x, y=rep(1, dim(data)[1]), col=c("green","red")[data$klasse], 
     pch=c(16, 18)[data$klasse], 
     xlab="", xlim=c(0, 35), xaxt="n", xaxs="i", yaxt="n", ylab="", ylim=c(-0.4,1.0))
axis(1, at=seq(0, 35, 5), labels=seq(0, 35, 5))
## k=1
knn_fit_1 <- train(klasse ~., data = data, method="knn", tuneGrid=data.frame(k=1))
res_knn_1 <- predict(knn_fit_1, newdata=data.frame(x=new_vec))

points(x=new_vec, y=rep(0.8, length(new_vec)), col=c("green","red")[res_knn_1], pch="-", cex=1)
mtext("k=1", side=2, at=0.8, las=2)

## k=2
knn_fit_2 <- train(klasse ~., data = data, method="knn", tuneGrid=data.frame(k=2))
res_knn_2 <- predict(knn_fit_2, newdata=data.frame(x=new_vec))
points(x=new_vec, y=rep(0.7, length(new_vec)), col=c("green","red")[res_knn_2], pch="-", cex=1)
mtext("k=2", side=2, at=0.7, las=2)

## k=3
knn_fit_3 <- train(klasse ~., data = data, method="knn", tuneGrid=data.frame(k=3))
res_knn_3 <- predict(knn_fit_3, newdata=data.frame(x=new_vec))
points(x=new_vec, y=rep(0.6, length(new_vec)), col=c("green","red")[res_knn_3], pch="-", cex=1)
mtext("k=3", side=2, at=0.6, las=2)

## k=4
knn_fit_4 <- train(klasse ~., data = data, method="knn", tuneGrid=data.frame(k=4))
res_knn_4 <- predict(knn_fit_4, newdata=data.frame(x=new_vec))
points(x=new_vec, y=rep(0.5, length(new_vec)), col=c("green","red")[res_knn_4], pch="-", cex=1)
mtext("k=4", side=2, at=0.5, las=2)

## k=5
knn_fit_5 <- train(klasse ~., data = data, method="knn", tuneGrid=data.frame(k=5))
res_knn_5 <- predict(knn_fit_5, newdata=data.frame(x=new_vec))
points(x=new_vec, y=rep(0.4, length(new_vec)), col=c("green","red")[res_knn_5], pch="-", cex=1)
mtext("k=5", side=2, at=0.4, las=2)

## k=6
knn_fit_6 <- train(klasse ~., data = data, method="knn", tuneGrid=data.frame(k=6))
res_knn_6 <- predict(knn_fit_6, newdata=data.frame(x=new_vec))
points(x=new_vec, y=rep(0.3, length(new_vec)), col=c("green","red")[res_knn_6], pch="-", cex=1)
mtext("k=6", side=2, at=0.3, las=2)

## k=7
knn_fit_7 <- train(klasse ~., data = data, method="knn", tuneGrid=data.frame(k=7))
res_knn_7 <- predict(knn_fit_7, newdata=data.frame(x=new_vec))
points(x=new_vec, y=rep(0.2, length(new_vec)), col=c("green","red")[res_knn_7], pch="-", cex=1)
mtext("k=7", side=2, at=0.2, las=2)


## k=8
knn_fit_8 <- train(klasse ~., data = data, method="knn", tuneGrid=data.frame(k=8))
res_knn_8 <- predict(knn_fit_8, newdata=data.frame(x=new_vec))
points(x=new_vec, y=rep(0.1, length(new_vec)), col=c("green","red")[res_knn_8], pch="-", cex=1)
mtext("k=8", side=2, at=0.1, las=2)

## k=9
knn_fit_9 <- train(klasse ~., data = data, method="knn", tuneGrid=data.frame(k=9))
res_knn_9 <- predict(knn_fit_9, newdata=data.frame(x=new_vec))
points(x=new_vec, y=rep(0, length(new_vec)), col=c("green","red")[res_knn_9], pch="-", cex=1)
mtext("k=9", side=2, at=0, las=2)

## k=10
knn_fit_10 <- train(klasse ~., data = data, method="knn", tuneGrid=data.frame(k=10))
res_knn_10 <- predict(knn_fit_10, newdata=data.frame(x=new_vec))
points(x=new_vec, y=rep(-0.1, length(new_vec)), col=c("green","red")[res_knn_10], pch="-", cex=1)
mtext("k=10", side=2, at=-0.1, las=2)

## k=11
knn_fit_11 <- train(klasse ~., data = data, method="knn", tuneGrid=data.frame(k=11))
res_knn_11 <- predict(knn_fit_11, newdata=data.frame(x=new_vec))
points(x=new_vec, y=rep(-0.2, length(new_vec)), col=c("green","red")[res_knn_11], pch="-", cex=1)
mtext("k=11", side=2, at=-0.2, las=2)

## k=12
knn_fit_12 <- train(klasse ~., data = data, method="knn", tuneGrid=data.frame(k=12))
res_knn_12 <- predict(knn_fit_12, newdata=data.frame(x=new_vec))
points(x=new_vec, y=rep(-0.3, length(new_vec)), col=c("green","red")[res_knn_12], pch="-", cex=1)
mtext("k=12", side=2, at=-0.3, las=2)

## k=13
knn_fit_13 <- train(klasse ~., data = data, method="knn", tuneGrid=data.frame(k=13))
res_knn_13 <- predict(knn_fit_13, newdata=data.frame(x=new_vec))
points(x=new_vec, y=rep(-0.4, length(new_vec)), col=c("green","red")[res_knn_13], pch="-", cex=1)
mtext("k=13", side=2, at=-0.4, las=2)
       
## KNN iris
############
set.seed(7)
data(iris)
dim(iris)
head(iris)
## Generierung von Trainings- und Test-Daten
rand <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
my.train <-  iris[rand, ]
my.test <-  iris[-rand, ]

## knn mit Trainingsdaten
iris.knn <- train(Species ~., data =my.train, method="knn", tuneGrid=data.frame(k=3))
## Vorhersage für die Test-Daten
pred <- predict(iris.knn, newdata=my.test)

confusionMatrix(dat=pred, reference = my.test$Species)


## KNN mit Standardpaket
#######################
library(class)
iris.knn.pred <-  knn(train=my.train[,1:4], test=my.test[,1:4],
                    cl=my.train$Species, k=3, prob=TRUE)
iris.knn.pred


## Arbeitsblatt 4:Aufgabe 1
#############################
#a)
library(ISLR)
?Smarket
data <- Smarket
str(data)   # Struktur
summary(data) 
## Verteilung der Klassen
table(data$Direction)  
## Volume ueber die Zeit
ts.plot(data$Volume, type = "l")  # Volume ueber die Zeit
#b)
train_index <- data$Year < 2005  
table(train_index)   
train <- data[train_index, c("Lag1", "Lag2", "Direction")] 
test <- data[!train_index, c("Lag1", "Lag2", "Direction")] 
#c)
library(caret)
knn1 <- train(Direction ~., data = train, method="knn",
              tuneGrid=data.frame(k=1))
knn.pred1_train <- predict(knn1, newdata=train) 
confusionMatrix(data=knn.pred1_train, reference=train$Direction)
# Alternative
table(knn.pred1_train, train$Direction)
#d)
knn.pred1 <- predict(knn1, newdata=test)
knn.pred1_prob <- predict(knn1, newdata=test, type = "prob")
## Wahrscheinlichkeiten für Klassenzuweisung
## e) 
## Konfusionsmatrix 
confusionMatrix(data=knn.pred1, reference=test$Direction)
#f)
set.seed(1) # bei 2 Nachbarn wird gewürfelt, wenn Uneinigkeit besteht: 
# durch seed geht würfeln immer gleich aus
knn <- train(Direction ~., data = train, method="knn",
             tuneGrid=data.frame(k=2:15))
knn
plot(knn)
knn.pred <- predict(knn, newdata=test)
knn.pred_prob <- predict(knn, newdata=test, type = "prob")

## Konfusionsmatrix 
confusionMatrix(data=knn.pred, reference=test$Direction)

## Kreuzvalidierung mit caret
################################
ctrl <- trainControl(method = "repeatedcv",
                     number =10,
                     repeats = 5)

knn_fit <-  train(Species ~., data =my.train,  method = "knn", trControl=ctrl, tuneGrid=data.frame(k=1:15))
knn_fit
plot(knn_fit)

train(Species ~., data =my.train,  method = "knn", trControl=ctrl, tuneLength = 10)

##LOOCV
ctrl2 <- trainControl(method = "cv",
                     number =dim(my.train)[1])

knn_fit_loocv <-  train(Species ~., data = my.train,  method = "knn", trControl=ctrl2, tuneGrid=data.frame(k=1:15))


## Weitere Optionen bei caret
################################

## Preprocessing
train(Species ~., data =my.train,  method = "knn", trControl=ctrl, tuneGrid=data.frame(k=1:13), preProcess = c("center", "scale"))
train(Species ~., data =my.train,  method = "knn", trControl=ctrl, tuneGrid=data.frame(k=1:13), preProcess = c("center", "scale", "pca"))

## optimierung mit andere Metrik
train(Species ~., data =my.train,  method = "knn", trControl=ctrl, tuneGrid=data.frame(k=1:13), metric="Kappa")

?preProcess

## Arbeitsblatt 4:Aufgabe 2
#############################
#a)
load("Daten/Churn1.rdata")
churn$IntlPlan <-  NULL
churn$VMailPlan <- NULL
boxplot(churn)
intrain <- createDataPartition(y=churn$Churn, p=0.8, list=F)
churn_train <- churn[intrain,]
churn_test <- churn[-intrain,]

knn <- train(Churn ~., data = churn_train, method="knn",
             tuneGrid=data.frame(k=1), 
             preProcess = c("center", "scale"))


pred_train <- predict(knn, newdata=churn_train)
mean(pred_train != churn_train$Churn)
pred_test <- predict(knn, newdata=churn_test)
mean(pred_test != churn_test$Churn)

# b) 
error_test <- error_training <- rep(NA,20)
set.seed(8)
for (k in 1:20){
  knn <- train(Churn ~., data = churn_train, method="knn",
               tuneGrid=data.frame(k=k), 
               preProcess = c("center", "scale"))
  pred_train <- predict(knn, newdata=churn_train)
  error_training[k] <- mean(pred_train != churn_train$Churn)
  pred_test <- predict(knn, newdata=churn_test)
  error_test[k] <- mean(pred_test != churn_test$Churn)
}
plot(x=1/(1:20),y=error_test, ylim = c(0,0.3),type='b', col='green',
     xlab="1/k",
     ylab="Fehlerrate", las=1)
lines(x=1/(1:20),y=error_training,col='red', type="b")
legend("topleft", legend=c("Testdaten", "Trainingsdaten"), 
       col=c("green", "red"), lty=1)
which.min(error_test)
#c)
x_fold <- 5
groups <- rep(1:x_fold, length.out=dim(churn)[1])
samp <- sample(x=groups, size=dim(churn)[1], replace=FALSE)
error_test <- error_training <- rep(NA,10)
for (k in 1:10){
  error_test_fold <- rep(NA, x_fold)
  error_train_fold <- rep(NA, x_fold)
  for (i in 1:x_fold){
    churn_train <- churn[samp!=i,]
    churn_test <- churn[samp==i,]
    knn <- train(Churn ~., data = churn_train, method="knn",
                 tuneGrid=data.frame(k=k), 
                 preProcess = c("center", "scale"))
    pred_train <- predict(knn, newdata=churn_train)
    error_train_fold[i] <- mean(pred_train != churn_train$Churn)
    pred_test <- predict(knn, newdata=churn_test)
    error_test_fold[i] <- mean(pred_test != churn_test$Churn)
  }
  error_training[k] <- mean(error_train_fold)
  error_test[k] <- mean(error_test_fold)
}
plot(x=1/(1:10),y=error_test, ylim = c(0,0.3),type='b', col='green', xlab="1/k", 
     ylab="Fehlerrate", las=1, 
     main=paste(x_fold, "-fach Kreuzvalidierung", sep=""))
lines(x=1/(1:10),y=error_training,col='red', type="b")
legend("topleft", legend=c("Testdaten", "Trainingsdaten"), 
       col=c("green", "red"), 
       lty=1)
which.min(error_test)

# d) 
ctrl <- trainControl(method = "repeatedcv",
                     number =10,
                     repeats = 3)
train_knn <- train(Churn ~ ., data=churn, method = "knn", 
                   trControl=ctrl, 
                   tuneGrid = data.frame(k=1:20),  
                   preProcess = c("center", "scale"))
plot(train_knn)

# e)
## Leave-one-out (dauert sehr, sehr lange)
ctrl2 <- trainControl(method = "cv",
                     number =dim(churn)[1])
train_knn2 <- train(Churn ~ ., data=churn, method = "knn", 
                   trControl=ctrl2, 
                   tuneGrid = data.frame(k=1:20),  
                   preProcess = c("center", "scale"))
plot(train_knn2)

# zu aufwendig

## Beispiel Diskriminanzanalyse
##############################
load("Daten/Studenten.Rdata")
head(Studenten)

## Aufteilung in Training und Testdaten
set.seed(25)
train_index <- createDataPartition(Studenten$sex, p = 0.7, list = FALSE)

train_set <- Studenten[train_index, ]
test_set <- Studenten[-train_index, ]

a <- tapply(train_set$height, INDEX=train_set$sex, FUN=function(x) length(x)/length(train_set$sex))
a
mu <- tapply(train_set$height, INDEX=train_set$sex, FUN=mean)
mu

var <- 1/(dim(train_set)[1]-2)*(sum((train_set$height[train_set$sex=="Female"]-mu[1])^2) + sum((train_set$height[train_set$sex=="Male"]-mu[2])^2))
var

## Diskriminanzfunktion
disq <- function(x, a, mu, var) x*mu/var-mu^2/(2*var)+log(a)

test_set[1,]

disq(x=test_set$height[1], a=a[1], mu=mu[1], var=var)

disq(x=test_set$height[1], a=a[2], mu=mu[2], var=var)

test_set$sex[1]

d_f <- disq(x=test_set$height, a=a[1], mu=mu[1], var=var)
d_m <- disq(x=test_set$height, a=a[2], mu=mu[2], var=var)

res <- ifelse(d_f>d_m, "Female", "Male")

confusionMatrix(data=as.factor(res), reference=test_set$sex)


Grösse <- seq(140, 210, 0.1)
plot(x=Grösse, y=dnorm(Grösse, mu[1],sqrt(var))*a[1], type="l", col="red", ylab="~A-posteriori-Wahrscheinlichkeit", lwd=2, las=1, xlab="Grösse [cm]", ylim=c(0,0.035))
lines(x=Grösse, y=dnorm(Grösse, mu[2],sqrt(var))*a[2], col="blue", lwd=2)
dnorm(160.3, mu[1],sqrt(var))*a[1]
dnorm(160.3, mu[2],sqrt(var))*a[2]
segments(x0=160.3, x1=160.3, y0=-5, y1=dnorm(160.3, mu[1],sqrt(var))*a[1], col="green", lwd=2)
text(y=0.002, x=160.3, label="Entscheidungsgrenze", col="darkgreen", cex=1.8)
legend("topright", legend=c("P(X|Frau)*P(Frau)", "P(X|Mann)*P(Mann)"), lwd=2, col=c("red", "blue"))


## Anpassen der a priori-Wahrscheinlichkeit
d_f2 <- disq(x=test_set$height, a=0.5, mu=mu[1], var=var)
d_m2 <- disq(x=test_set$height, a=0.5, mu=mu[2], var=var)

res2 <- ifelse(d_f2>d_m2, "Female", "Male")
confusionMatrix(data=as.factor(res2), reference=test_set$sex)



## LDA und QDA in R mit caret
####################
library(caret)
modelLookup("lda")

## Beispiel Geschlecht nach Grössen
ctrl <- trainControl(method = "repeatedcv",
                     number =10,
                     repeats = 5)
lda_height <- train(sex ~., data = train_set, method = "lda", trControl=ctrl)
lda_height

res_heigth = predict(lda_height, test_set)
table(res_heigth, test_set$sex)

## Anpassung der Prior
lda_height_prior <- train(sex ~., data = train_set, method = "lda", trControl=ctrl,
                          prior = c(0.5, 0.5))
res_heigth_prior = predict(lda_height_prior, test_set)
table(res_heigth_prior, test_set$sex)


lda_fit <- train(Species ~., data = my.train, method = "lda", trControl=ctrl)

lda_fit
res_lda = predict(lda_fit, my.test)

confusionMatrix(data=res_lda, reference=my.test$Species)

modelLookup("qda")
qda_fit <- train(Species ~., data = my.train, method = "qda", trControl=ctrl)

qda_fit 

res_qda = predict(qda_fit, my.test)

confusionMatrix(data=res_qda, reference=my.test$Species)

## Alternative
library(MASS)
res_lda <- lda(sex~height, data=train_set)
res_lda2 <- lda(sex~height, data=train_set, prior=c(0.5, 0.5))
pred_lda <- predict(res_lda, newdata=test_set)
str(pred_lda)
table(pred_lda$class, res)

## Aufgabe 3: Diskriminanzanalyse
######################
# a) 
load("Daten/Boston.RData")
boxplot(Boston)
set.seed(7)
index = createDataPartition(y=Boston$crime, p=0.5, list=FALSE)
train = Boston[index,]
test = Boston[-index,]

#b) 
lda.fit = train(crime ~ ., data=train, method="lda",
                trControl = trainControl(method = "cv"),  
                preProcess = c("center", "scale"))
pred.crime = predict(lda.fit, test)
confusionMatrix(data= pred.crime, reference = test$crime)



# c) 
qda.fit = train(crime ~ ., data=train, method="qda",
                trControl = trainControl(method = "cv"),
                preProcess = c("center", "scale"))
pred.crime2 = predict(qda.fit, test)
confusionMatrix(data= pred.crime2, reference = test$crime)
