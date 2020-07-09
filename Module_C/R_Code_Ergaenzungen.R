## Modul C: Data Mining mit Schwerpunkt auf Clustering und Klassifikation
###############################################################################
## R-Code Ergänzungen (Bootstrap, Lift Chart, ROC-Kurve, Support Vector Machine)
###############################################################################


## Anwendung PCA auf neue Daten
############################
library(caret)
rand <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
my.train <-  iris[rand, 1:4]
my.test <-  iris[-rand, 1:4]

res.pca <- prcomp(my.train, scale=F)
res.pca
head(my.test)
predict(res.pca, my.test)

## Imputation fehlende Werte
##################################
iris_miss <- iris
set.seed(17)
iris_miss[sample(1:dim(iris)[1], size=10, replace=F),4] <- NA
head(iris_miss)
summary(iris_miss)

library(caret)
knn_imp <- preProcess(iris_miss, method= "knnImpute")
iris_miss_imp <- predict(knn_imp, iris_miss)
head(iris_miss_imp)
plot(x=(iris[which(is.na(iris_miss$Petal.Width)),4]-mean(iris_miss$Petal.Width, na.rm=TRUE))/
       sd(iris_miss$Petal.Width, na.rm=TRUE),
     y=iris_miss_imp[which(is.na(iris_miss$Petal.Width)),4], 
     xlab="wahre Werte (skaliert)", ylab="imputierte Werte", pch=16)
abline(a=0, b=1, col="grey")

bag_imp <- preProcess(iris_miss, method= "bagImpute")
iris_miss_imp <- predict(bag_imp, iris_miss)
head(iris_miss_imp)
plot(x=iris[which(is.na(iris_miss$Petal.Width)),4],
     y=iris_miss_imp[which(is.na(iris_miss$Petal.Width)),4], 
     xlab="wahre Werte", ylab="imputierte Werte", pch=16)
abline(a=0, b=1, col="grey")

median_imp <- preProcess(iris_miss, method= "medianImpute")
iris_miss_imp <- predict(median_imp , iris_miss)
head(iris_miss_imp)
plot(x=iris[which(is.na(iris_miss$Petal.Width)),4],
     y=iris_miss_imp[which(is.na(iris_miss$Petal.Width)),4], 
     xlab="wahre Werte", ylab="imputierte Werte", pch=16)
abline(a=0, b=1, col="grey")
median(iris_miss$Petal.Width, na.rm=TRUE)


## mehrere fehlende Werte
iris_miss2<- iris

iris_miss2$Sepal.Length[1] <- NA
iris_miss2$Sepal.Width[1] <- NA
head(iris_miss2)

bag_imp2 <- preProcess(iris_miss2, method= "bagImpute")
iris_miss_imp2 <- predict(bag_imp2, iris_miss2)
head(iris_miss_imp2)

## ROC-Kurve
############################
## Beispiel Studenten (manuell)
load("Daten/Studenten.Rdata")
head(Studenten)

## cut-off 163
fun_cutoff <- function(x, cutoff) ifelse(x > cutoff, "Male", "Female") 
                                         
pred <- fun_cutoff(Studenten$height, cutoff=163)

confusionMatrix(data=as.factor(pred), reference=Studenten$sex)

pred <- fun_cutoff(Studenten$height, cutoff=185)

confusionMatrix(data=as.factor(pred), reference=Studenten$sex)

fun_roc <- function(pred, reference){
  tab <- array(0,dim=c(2,2),
        dimnames=list(c("Female", "Male"),
                      c("Female", "Male")))
  tab[1,1] <- sum(pred=="Female"& reference=="Female")
  tab[2,1] <- sum(pred=="Male" & reference=="Female")
  tab[1,2] <- sum(pred=="Female"& reference=="Male")
  tab[2,2] <- sum(pred=="Male"& reference=="Male")
  return(list(tpr=tab[1,1]/(sum(tab[,1])), fpr=1-tab[2,2]/(sum(tab[,2]))))
}


t1 <- fun_roc(fun_cutoff(Studenten$height, cutoff=163),reference=Studenten$sex )
plot(y=t1$tpr, x=t1$fpr, xlim=c(0,1), ylim=c(0,1), col="red", pch=16, ylab="tpr (Sensitivity)", xlab="fpr (1-Specificity)", las=1, asp=1,
     xaxs="i", yaxs="i")

t2 <- fun_roc(fun_cutoff(Studenten$height, cutoff=185),reference=Studenten$sex )
points(y=t2$tpr, x=t2$fpr, xlim=c(0,1), ylim=c(0,1), col="red", pch=16)

Range <- 140:220
dat <- as.data.frame(matrix(NA, nrow=length(Range), ncol=2, dimnames=list(NULL,c("tpr", "fpr"))))
for(i in 1:length(Range)){
  res <- fun_roc(fun_cutoff(Studenten$height, cutoff=Range[i]),reference=Studenten$sex )
  dat[i,] <- c(res$tpr, res$fpr)
}
lines(y=dat$tpr, x=dat$fpr)
abline(a=0, b=1, col="grey", lty=2)


## ROC in R
library(pROC)

load("Daten/Churn1.rdata")
set.seed(7)
library(caret)
intrain <- createDataPartition(y=churn$Churn, p=0.8, list=F)
churn_train <- churn[intrain,]
churn_test <- churn[-intrain,]

ctrl <- trainControl(method="oob") 
rf_fit <- train(Churn ~., data = churn_train, method = "rf", trControl=ctrl, tuneLength =3)
pred_test_rf <- predict(rf_fit, newdata=churn_test, type="prob")
head(pred_test_rf)
rf.roc <- roc(churn_test$Churn, pred_test_rf$'TRUE') 
rf.roc
rf.roc$auc
plot(rf.roc)

class_prediction <- factor(ifelse(pred_test_rf$'TRUE'>0.2, "TRUE", "FALSE"), levels=c("TRUE", "FALSE"))

confusionMatrix(class_prediction, churn_test$Churn)

ctrl <- trainControl(method="repeatedcv", number=10, repeats = 3) 
tree_fit <- train(Churn ~., data = churn_train, method = "rpart", trControl=ctrl, 
                  tuneLength =10)
pred_test_tree <- predict(tree_fit, newdata=churn_test, type="prob")

tree.roc <- roc(churn_test$Churn, pred_test_tree$'TRUE') 

## knn
knn_fit <- train(Churn ~., data = churn_train, method = "knn", trControl=ctrl, 
                 tuneLength  =10, preProcess = c("center", "scale"))

pred_test_knn <- predict(knn_fit, newdata=churn_test, type="prob")

knn.roc <- roc(churn_test$Churn, pred_test_knn$'TRUE') 


lda_fit <- train(Churn ~., data = churn_train, method = "lda", trControl=ctrl, 
                 tuneLength  =10, preProcess = c("center", "scale"))

pred_test_lda <- predict(lda_fit, newdata=churn_test, type="prob")


lda.roc <- roc(churn_test$Churn, pred_test_lda$'TRUE') 


qda_fit <- train(Churn ~., data = churn_train, method = "qda", trControl=ctrl, 
                 tuneLength  =10, preProcess = c("center", "scale"))

pred_test_qda <- predict(qda_fit, newdata=churn_test, type="prob")


qda.roc <- roc(churn_test$Churn, pred_test_qda$'TRUE') 

plot(tree.roc, col="green", las=1, asp=1, xlim=c(1,0), ylim=c(0,1), xaxs="i", yaxs="i")
lines(knn.roc, col="red")
lines(qda.roc, col="blue")
lines(lda.roc, col="lightblue")
lines(rf.roc, col="darkgreen")
legend("bottomright", legend = c(paste("Tree (AUC: ",round(tree.roc$auc,2), ")"),
                                 paste("knn (AUC: ",round(knn.roc$auc,2), ")"),
                                 paste("lda (AUC: ",round(lda.roc$auc,2), ")"),
                                 paste("qda (AUC: ",round(qda.roc$auc,2), ")"),
                                 paste("RandomForest (AUC: ",round(rf.roc$auc,2), ")")), 
       col=c("green", "red", "lightblue", "blue", "darkgreen"), lwd=1, cex=0.9)





## Lift Chart
###############
load("Daten/Churn1.rdata")
churn$Churn <- factor(churn$Churn, levels=c("TRUE", "FALSE"))
(tab <- table(churn$Churn))
tab[1]/sum(tab)
library(caret)
set.seed(13)
intrain <- createDataPartition(y=churn$Churn, p=0.75, list=F)
dat_train <- churn[intrain,]
dat_test <- churn[-intrain,]
ctrl <- trainControl(method="oob") 
rf_fit <- train(Churn ~., data = dat_train, method = "rf", trControl=ctrl, tuneLength =3)
pred <- predict(rf_fit, newdata=dat_test, type="prob")
dat_lift <- data.frame(pred=pred[,1], Response=dat_test$Churn==TRUE)
head(dat_lift, n=20)
dat_lift <- dat_lift[order(dat_lift$pred, decreasing = TRUE),]
head(dat_lift)
dat_lift$cumRes <- cumsum(dat_lift$Response)
head(dat_lift, n=100)
dat_lift$Anteile <- dat_lift$cumRes/(1:dim(dat_lift)[1])*100
dat_lift$Total <-  (1:dim(dat_lift)[1])/dim(dat_lift)[1]*100 
head(dat_lift, n=80)
# mittlere Erfolgsquote
(b <- sum(dat_lift$Response)/length(dat_lift$Response)*100)
par(mgp=c(2,1,0))
plot(Anteile~Total, data=dat_lift, type="l", las=1, main="Lift Chart: Churn", 
     ylim=c(0,100), ylab="Anteile %", xlab="Total %", col="darkgreen")
abline(h=b, lty=3)

## Beispiel erste 10%
dat_lift[which(dat_lift$Total>10)[1],]
segments(x0=10, x1=10, y0=-5, y1=97.61905, col="red")
segments(x0=-5, x1=10,y0=97.61905, y1=97.61905, col="red")
## Lift
dat_lift$Anteile[which(dat_lift$Total>10)[1]]/b

## Bootstrap
B <- 1000
boot_samp <- matrix(NA, ncol=B, nrow=dim(dat_test)[1])
basedat <- rep(NA, dim(dat_test)[1])
for (i in 1:B){
  index <- sample(dim(dat_test)[1], replace=T)
  pred <- predict(rf_fit, newdata = dat_test[index,], type="prob")[,1]
  dat_bot <- data.frame(pred=pred, 
                         Response=dat_test$Churn[index]==TRUE)
  dat_bot <- dat_bot[order(dat_bot$pred, decreasing = TRUE),]
  dat_bot$cumRes <- cumsum(dat_bot$Response)
  boot_samp[,i] <- dat_bot$cumRes[1:length(dat_bot$cumRes)]/(1:length(dat_bot$cumRes))*100
  basedat[i] <- sum(dat_bot$Response)/length(dat_bot$Response)*100
}

dim(boot_samp)
q_975 <- apply(boot_samp, MARGIN=1, FUN=function(x) quantile(x, probs=0.975))
q_025 <- apply(boot_samp, MARGIN=1, FUN=function(x) quantile(x, probs=0.025))
base_975 <- quantile(basedat, probs=0.975)
base_025 <- quantile(basedat, probs=0.025)

plot(Anteile~Total, data=dat_lift, type="l", las=1, main="Lift Chart: Churn", 
     ylim=c(0,100), ylab="Anteile %")
polygon(y=c(rep(base_975,2), rep(base_025,2)), 
        x=c(0,100,100,0), col="grey", border=NA)
abline(h=b, lty=3)
polygon(y=c(q_975, q_025[length(q_025):1]), 
        x=c(dat_lift$Total, dat_lift$Total[length(q_025):1]), col="green", border=NA)
lines(Anteile~Total, data=dat_lift, col="darkgreen")


## Support Vector Machine
##########################
set.seed (1)
x <- matrix (rnorm (20*2) , ncol =2)
y <- c(rep (-1,10) , rep (1 ,10) )
x[y==1 ,]= x[y==1,] + 1
dat <- data.frame(x=x, y=as.factor (y))
xtest <- matrix (rnorm (20*2) , ncol =2)
ytest <- sample (c(-1,1) , 20, rep=TRUE)
xtest[ytest ==1 ,]= xtest[ytest ==1,] + 1
testdat <- data.frame (x=xtest , y=as.factor(ytest))
library(ggplot2)
ggplot(dat, aes(x=x.1, y=x.2, colour=y))+
  geom_point()


## Support Vector Machine with Carot
library(caret)
modelLookup("svmLinear2")
x <- trainControl(method="repeatedcv",
                  number=10,
                  repeats=3)
model_svm <- train(y~., data=dat, method="svmLinear2", 
                   tuneGrid=data.frame(cost=c(0.001 , 0.01, 0.1, 1,5,10,100)),  
                   trControl=x)
model_svm
model_svm$finalModel
summary(model_svm$finalModel)

pred <- predict(model_svm, testdat)
confusionMatrix(data =pred , reference= testdat$y)

str(model_svm$finalModel)

dat_scale <- scale(dat[,1:2]) 
plot(x.2~x.1, data=dat_scale, col=as.factor(y), pch=16)
model_svm$finalModel$decision.values
points(model_svm$final$SV, pch=4, cex=1.5)
(cf <- coef(model_svm$finalModel))
abline(-cf[1]/cf[3], -cf[2]/cf[3], col = "red")


## Arbeitsblatt 5: Aufgabe 1
#a
library(ISLR)
library(caret)
data(OJ)
dim(OJ)
set.seed(1)
train <- createDataPartition(y=OJ$Purchase, p=0.8, list=F)
OJ.train <- OJ[train, ]
OJ.test <- OJ[-train, ]

# b)
x <- trainControl(method="none")
model_svm <- train(Purchase ~ ., data = OJ.train, method='svmLinear2', 
                   tuneGrid=data.frame(cost=c(0.01)),  trControl=x)
model_svm$finalModel

# c) 

train.pred <- predict(model_svm, OJ.train)
confusionMatrix(data=train.pred, reference = OJ.train$Purchase)
test.pred <- predict(model_svm, OJ.test)
confusionMatrix(data=test.pred, reference = OJ.test$Purchase)

# d) 
repeats <- 3
numbers <- 10
x <- trainControl(method="repeatedcv",
                  number=numbers,
                  repeats=repeats)
model_svm2 <- train(Purchase ~ ., data = OJ.train, method="svmLinear2", 
                    tuneGrid=data.frame(cost=10^seq(-2, 1, by = 0.25)),  
                    metric= "Accuracy", trControl=x)
model_svm2

# e) 
train.pred <- predict(model_svm2, OJ.train)
confusionMatrix(data=train.pred, reference = OJ.train$Purchase)
test.pred <- predict(model_svm2, OJ.test)
confusionMatrix(data=test.pred, reference = OJ.test$Purchase)

# f) 
x <- trainControl(method="repeatedcv",
                  number=numbers,
                  repeats=repeats, 
                  classProbs = TRUE)
model_svm3 <- train(Purchase ~ ., data = OJ.train, method="svmLinear2", 
                    tuneGrid=data.frame(cost=c(0.01)),  trControl=x)
pred_prob <- predict(model_svm3, OJ.test, type='prob')
library(pROC)
res.roc <- roc(OJ.test$Purchase, pred_prob$'CH')
res.roc
plot(res.roc)



## Demonstration Gaussian Kernel
library(manipulate)
library(e1071)
manipulate({
  svmfit=svm(y ~ .,data=dat, kernel="radial", 
             gamma=gamma, cost = cost)
  plot(svmfit , dat) #Plotting 
}, gamma = slider(0.1,10), cost=slider(0.1,10))

## Gaussian Kernel in R
x <- trainControl(method="repeatedcv",
                  number=10,
                  repeats=3)
modelLookup("svmRadial")

model_svm_radial <- train(y~., data=dat, method="svmRadial", tuneGrid=expand.grid(sigma=seq(0.5,5,0.5), C=c(0.01, 0.1, 0.5, 1, 1.5)),   trControl=x)
model_svm_radial
confusionMatrix(model_svm_radial , "none")


## Arbeitsblatt 5: Aufgabe 2
# a) 
set.seed(4)
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- as.factor(1 * (x1^2 - x2^2 > 0))

# b) 
plot(x=x1, y=x2, col=y)
library(ggplot2)
dat <- data.frame(x1 = x1, x2 = x2, y = y)
ggplot(dat, aes(x = x1, y = x2, col = y, shape = y)) + geom_point()


# c) 
x <- trainControl(method="none")
model_svm <- train(y ~ x1 + x2, data=dat, method='svmLinear2', 
                   tuneGrid=data.frame(cost=c(0.1)),  
                   trControl=x)
svm.pred <- predict(model_svm, dat)
confusionMatrix(data=svm.pred, reference=y)
svm.pred <- as.factor(svm.pred)
ggplot(dat, aes(x = x1, y = x2, col = svm.pred, shape = y)) + geom_point() +
  ggtitle("SVM with linear kernel")
plot(x=x1, y=x2, col=svm.pred, pch=as.numeric(y))

# d) 
x <- trainControl(method="repeatedcv",
                  number=10,
                  repeats=3)
modelLookup("svmRadial")

svm_radial <- train(y~., data=dat, method='svmRadial', 
                    tuneGrid=expand.grid(sigma=seq(0.5,5,0.5), C=c(0.01, 0.1, 0.5, 1, 1.5)),   
                    trControl=x)
svm_radial

svm.pred2 <- predict(svm_radial, dat)
confusionMatrix(data=svm.pred2, reference=y)

svm.pred2 <- as.factor(svm.pred2)
ggplot(dat, aes(x = x1, y = x2, col = svm.pred2, shape = y)) +
  geom_point() +
  ggtitle("SVM with radial kernel")
## Polynomial Kernel
modelLookup("svmPoly")

svm_poly <- train(y~., data=dat, method="svmPoly", tuneGrid=
                    expand.grid(degree=2, scale=c(0.1,  1,  2,  5, 10), 
                                C=c(0.1,  1, 5, 8, 10,15)), trControl=x)
svm_poly

svm.pred3 <- predict(svm_poly, dat)
confusionMatrix(data=svm.pred3, reference=y)

svm.pred3 <- as.factor(svm.pred3)
ggplot(dat, aes(x = x1, y = x2, col = svm.pred3, shape = y)) +
  geom_point() +
  ggtitle("SVM with polynomial kernel of degree 2")



## Mehr als 2 Klassen
set.seed(7)
data(iris)
dim(iris)
head(iris)
## Generierung von Trainings- und Test-Daten
rand <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
my.train <-  iris[rand, ]
my.test <-  iris[-rand, ]

x <- trainControl(method="repeatedcv",
                  number=10,
                  repeats=3)
model_3class <- train(Species ~ ., data = my.train, method="svmLinear", tuneGrid=data.frame(C=c(0.01, 0.1, 0.5, 1, 2,10)),  trControl=x)
                      
(ypred <- predict(model_3class, my.test))
confusionMatrix(data=ypred, reference = my.test$Species)


## Deep Learing in R
##########################
library(keras)
set.seed(8)
iris_scale <- scale(iris[,1:4])
rand <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
##benötigen Matrizen mit numerischen Werten
train <-  as.matrix(iris_scale[rand, 1:4])
test <-   as.matrix(iris_scale[-rand, 1:4])
train_labels1 <-  as.numeric(iris[rand, 5])-1
test_labels1 <-  as.numeric(iris[-rand, 5])-1

mix <- sample(1:dim(train)[1])
train <- train[mix,]
train_labels1 <- train_labels1[mix]

# One hot encode für Zielvariable 
train_labels <- to_categorical(train_labels1)
test_labels <- to_categorical(test_labels1)

head(train_labels)

## Architektur erstellen
model_iris <- keras_model_sequential()
layer_dense(model_iris, units = 8, input_shape = c(4), activation = 'relu') 
layer_dense(model_iris, units = 3, activation = 'softmax')

summary(model_iris)


model_iris <- compile(model_iris,
  optimizer = optimizer_adam(lr = 0.001), 
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)


history <- fit(model_iris,
  x=train, 
  y=train_labels, 
  epochs = 200,
  batch_size = 5, 
  validation_split = 0.2
)

# Plot the history
plot(history)

# Predict the classes for the test data
classes <- predict_classes(model_iris, test)

# Confusion matrix
confusionMatrix(data=as.factor(classes), reference=as.factor(test_labels1))

# Evaluate on test data and labels
score <- evaluate(model_iris, x=test, y=test_labels)

# Print the score
score

## Modell mit dropout
model_iris2 <- keras_model_sequential()
layer_dense(model_iris2, units = 8, input_shape = c(4), 
            activation = 'relu') 
layer_dropout(model_iris2, rate = 0.4)
layer_dense(model_iris2, units = 8, activation = 'relu') 
layer_dense(model_iris2, units = 3, activation = 'softmax')

summary(model_iris2)


model_iris2 <- compile(model_iris2,
                      optimizer = 'adam', 
                      loss = 'categorical_crossentropy',
                      metrics = c('accuracy')
)


history2 <- fit(model_iris2,
               x=train, 
               y=train_labels, 
               epochs = 200,
               batch_size = 5, 
               validation_split = 0.2
)

# Plot the history
plot(history2)

# Evaluate on test data and labels
score <- evaluate(model_iris2, x=test, y=test_labels)

# Print the score
score


## Fashion-MNIST
fashion_mnist <- dataset_fashion_mnist()
str(fashion_mnist)

?dataset_fashion_mnist

train_images <- fashion_mnist$train$x
train_labels <- fashion_mnist$train$y

test_images <- fashion_mnist$test$x
test_labels <- fashion_mnist$test$y

dim(train_images)

dim(train_labels)

# One Hot Encoding
train_y_keras <- to_categorical(train_labels)
test_y_keras <- to_categorical(test_labels)


class_names <- c('T-shirt/top',
                 'Trouser',
                 'Pullover',
                 'Dress',
                 'Coat', 
                 'Sandal',
                 'Shirt',
                 'Sneaker',
                 'Bag',
                 'Ankle boot')
class_names[train_labels[1:20]+1]

library(ggplot2)
library(tidyr)

image_1 <- as.data.frame(train_images[17, , ])
colnames(image_1) <- seq_len(ncol(image_1))
image_1$y <- seq_len(nrow(image_1))
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.integer(image_1$x)

ggplot(image_1, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")

## Skalierung der Werte
train_images <- train_images / 255
test_images <- test_images / 255

par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
  img <- train_images[i, , ]
  img <- t(apply(img, 2, rev)) 
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste(class_names[train_labels[i] + 1]))
}

## Modell erstellen
model <- keras_model_sequential()
layer_flatten(model, input_shape = c(28, 28))
layer_dense(model, units = 128, activation = "relu", name = "hidden1")
layer_dense(model, units = 10, activation = "softmax", name = "output")

compile(model, loss = "categorical_crossentropy", 
          optimizer = optimizer_adam(lr = 0.001), 
          metrics = c("accuracy"))

summary(model)

history <- fit(model, x=train_images, y=train_y_keras, epochs = 15, verbose = 2, batch_size = 10, validation_split = 0.2)

# Plot the history
plot(history)

# Predict the classes for the test data
classes <- predict_classes(model, test_images)

# Confusion matrix
confusionMatrix(data=as.factor(classes), reference=as.factor(test_labels))

# Evaluate on test data and labels
score <- evaluate(model, x=test_images, y=test_y_keras)

# Print the score
print(score)

par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
  img <- test_images[i, , ]
  img <- t(apply(img, 2, rev)) 
  # subtract 1 as labels go from 0 to 9
  predicted_label <- classes[i] #which.max(predictions[i, ]) - 1
  true_label <- test_labels[i]
  if (predicted_label == true_label) {
    color <- '#008800' 
  } else {
    color <- '#bb0000'
  }
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste0(class_names[predicted_label + 1], " (",
                      class_names[true_label + 1], ")"),
        col.main = color)
}



## CNN-Modell 
dim(train_images) <- c(nrow(train_images), 28, 28, 1)
dim(test_images) <- c(nrow(test_images), 28, 28, 1)
cnn <- keras_model_sequential()
layer_conv_2d(cnn, filters = 32, kernel_size = c(5,5), activation = 'relu',
                input_shape = c(28,28,1))
layer_max_pooling_2d(cnn, pool_size = c(2, 2)) 
layer_dropout(cnn, rate = 0.25) 
layer_conv_2d(cnn, filters = 64, kernel_size = c(3,3), activation = 'relu') 
layer_max_pooling_2d(cnn, pool_size = c(2, 2)) 
layer_dropout(cnn, rate = 0.25) 
layer_conv_2d(cnn, filters = 128, kernel_size = c(3,3), activation = 'relu')
layer_dropout(cnn, rate = 0.4) 
layer_flatten(cnn)
layer_dense(cnn, units = 128, activation = 'relu') 
layer_dropout(cnn, rate = 0.3) 
layer_dense(cnn, units = 10, activation = 'softmax')

compile(cnn, loss = "categorical_crossentropy", 
        optimizer = optimizer_adam(lr = 0.001), 
        metrics = c("accuracy"))

summary(cnn)

## dauert sehr, sehr Lange, auf einem PC allenfalls nicht ausführbar
history <- fit(cnn, x=train_images, y=train_y_keras, epochs = 5, verbose = 2, batch_size = 5, validation_split = 0.2)

