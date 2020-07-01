## Modul E: Data Mining mit Schwerpunkt auf Clustering und Klassifikation
###############################################################################
## R-Code Klassifikation
################################


## Klassifikationsbäume
###########################

data <- data.frame(x1=c(0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.5, 0.6, 0.8, 0.9), 
           x2=c(0.25, 0.7, 0.5, 0.1, 0.3, 0.1, 0.9, 0.6, 0.8, 0.3 ),
           klasse=c(rep("A", 6), rep("B",4)))

plot(x2~x1, data=data, pch=c(4, 1)[data$klasse], cex=2, las=1, ylim=c(0,1), xlim=c(0,1), xaxs="i", yaxs="i")
abline(v=0.45, lty=2)
lines(x = c(0.45,1), y = c(0.45,0.45), lty=2)
lines(x = c(0.8,0.8), y = c(0.45,0), lty=2)

## Visualusierung in R
library(caret)
library(rpart)
t1 <- train(klasse~., data=data,  method="rpart", trControl = trainControl(method = "none"), control = rpart.control(minsplit =2))
library(rattle)
fancyRpartPlot(t1$finalModel)

## Beispiel Gini-Index
plot(x2~x1, data=data, pch=c(4, 1)[data$klasse], cex=2, las=1, ylim=c(0,1), xlim=c(0,1))
(Gini_start <- 1-(0.6^2+0.4^2))
abline(v=0.45, lty=2)

(Gini_l <- 1-(1^2+0^2))
(Gini_r <- 1-((4/6)^2+(2/6)^2))
Gini_start-0.4*Gini_l-0.6*Gini_r
## anderer (schlechterer) Split
plot(x2~x1, data=data, pch=c(4, 1)[data$klasse], cex=2, las=1, ylim=c(0,1), xlim=c(0,1))
abline(h=0.4, lty=2)
(Gini_o <- 1-((3/5)^2+(2/5)^2))
(Gini_u <- 1-(4/5)^2+(1/5)^2)
Gini_start-0.5*Gini_o-0.5*Gini_u

## Klassifikationsbäume in R
###############################
library(caret)
modelLookup("rpart")

## Generierung von Trainings- und Test-Daten
rand <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
my.train <-  iris[rand, ]
my.test <-  iris[-rand, ]

## Klassifikationsbaum mit Trainingsdaten
ctrl <- trainControl(method = "none")
r.tree <- train(Species ~., data =my.train, method="rpart", trControl = ctrl, tuneGrid=data.frame(cp=0.01))
r.tree
r.tree$finalModel
plot(r.tree) # nur für Parametertuning
## Visualisierung des Baums
plot(r.tree$finalModel,margin=0.2, uniform=TRUE)
text(r.tree$finalModel)
## Alternative für schöner Visualisierung
library(rattle)
fancyRpartPlot(r.tree$finalModel)

## Vorhersage für die Test-Daten
pred <- predict(r.tree, newdata=my.test)
confusionMatrix(dat=pred, reference = my.test$Species)

## Alternative direkt mit rpart
library(rpart)
tree <- rpart(Species ~., data =my.train)
plot(tree, margin=0.2, uniform=TRUE)
text(tree, use.n=TRUE)

## Arbeitsblatt 5: Aufgabe 1
#############################
#a)
library(caret)
library(MASS)
head(crabs)
?crabs
## Klassifikationsbaum mit Trainingsdaten
r.tree <- train(sex ~., data=crabs, method='rpart',  tuneGrid=data.frame(cp=0.01))
plot(r.tree$finalModel, margin=0.2, uniform=TRUE)
text(r.tree$finalModel, use.n=TRUE)
## Optional
library(rattle)
fancyRpartPlot(r.tree$finalModel)

# b) 
pred_sex <-  predict(r.tree, data =crabs)
confusionMatrix(dat=pred_sex, reference = crabs$sex)

# c) 
r.tree$finalModel
plot(CL~RW, data=crabs, pch=19,col=crabs$sex, las=1)
legend("bottomright", legend = c("Male", "Female"), fill =  c(2,1))
## Erste Auftrennung im Streudiagramm 
abline(v=15.9)
# weitere Auftrennungen
lines(x = c(0, 15.9), y = c(35.9,35.9))
lines(x = c(13.1,13.1), y = c(0, 35.9))
lines(x = c(0,13.1), y = c(30.15,30.15))
lines(x = c(11.35, 11.35), y=c(0, 30.15))
lines(x = c(0,11.35), y = c(25.9,25.9)) 
lines(x = c(9.45,9.45), y = c(0,25.9)) 

# d)	
str(crabs)
# Die Variable 4 bis 8 sind nummerisch!
boxplot(crabs[,4:8])
pca <-  prcomp(crabs[,4:8], scale = FALSE) # Skalierung auch möglich
summary(pca)
plot(pca$x,pch=19,col=crabs$sex) 
pca

# e) 
crabs2 <- cbind(crabs,pca$x)
head(crabs2)
#Klassifikationsbaum mit dem gesamten Datensatz
r.tree2 <- train(sex ~., data=crabs2, method='rpart',  tuneGrid=data.frame(cp=0.01))
plot(r.tree2$finalModel, margin=0.2, uniform=TRUE)
text(r.tree2$finalModel, use.n=TRUE)
## Optional
library(rattle)
fancyRpartPlot(r.tree$finalModel)
##Streudiagram
plot(PC3~PC2, data=crabs2,pch=19,col=crabs$sex)
## Erste Auftrennung
abline(v=0.02025) 
## Zweite Auftrennung auf der linken Seite
abline(v=-0.382)
## Zweite Auftrennung auf der rechten Seite
lines(x = c(0.02025,4), y = c(-0.7436,-0.7436))
## Performance in den Trainingsdaten
pred_sex2 <- predict(r.tree2,data=crabs2)
confusionMatrix(dat=pred_sex2, reference = crabs$sex)


## Pruning Beispiel Titanic
######################
load("Daten/clean_titanic.Rdata")
head(clean_titanic)
dim(clean_titanic)
set.seed(19)
rand <- createDataPartition(clean_titanic$survived, p = 0.7, list = FALSE)
trai <- clean_titanic[rand,]
test <- clean_titanic[-rand,]
## vollständiger Baum
res <- train(survived~., data=trai,  method="rpart", trControl = trainControl(method = "none"), control = rpart.control(minsplit =2), tuneGrid=data.frame(cp=0))
plot(res$finalModel, margin=0.2, uniform=TRUE)
text(res$finalModel, use.n=TRUE, cex=0.5)
## Training- vs. Testfehler
p_trai <- predict(res, trai)
confusionMatrix(p_trai, reference = trai$survived)                                    
p_test <- predict(res, test)
confusionMatrix(p_test, reference = test$survived)
## Optimierung mittels Pruning
cl <- trainControl(method = "repeatedcv", number =10, repeats = 5)
res2 <- train(survived~., data=trai,  method="rpart", trControl = cl, control = rpart.control(minsplit =2), tuneGrid=data.frame(cp=seq(0,0.1,0.005)))
plot(res2)
plot(res2$finalModel, margin=0.2, uniform=TRUE)
text(res2$finalModel, use.n=TRUE)
p_trai2 <- predict(res2, trai)
confusionMatrix(p_trai2, reference = trai$survived)
p_test2 <- predict(res2, test)
confusionMatrix(p_test2, reference = test$survived)
         


## Random Forest in R
#############################
library(caret)
modelLookup("rf")
fitControl <- trainControl(method = "oob")
res_rf <- train(Species ~., data=iris, method="rf", 	trControl = fitControl, tuneLength=3)

res_rf
plot(res_rf)

res_rf$finalModel
set.seed(1)
res_rf2 <- train(Species ~., data=iris, method="rf", 	trControl = fitControl, tuneLength=3, ntree=5000)
res_rf2$finalModel

## Alternativen
library(randomForest)
set.seed(1)
iris.rf = randomForest(Species ~ ., data = iris, importance = TRUE, ntree=5000) 
iris.rf
library(party)
d_forest <- cforest(Species ~ ., data = iris)
d_forest 


## Importance
res_rf2 <- train(Species ~., data=iris, method="rf", 	trControl = fitControl, tuneLength=3, ntree=5000)
varImp(res_rf2)
res_rf3 <- train(Species ~., data=iris, method="rf", 	trControl = fitControl, tuneLength=3, ntree=5000, importance=TRUE)
varImp(res_rf3)
plot(varImp(res_rf3))

## Partial Dependence Plot
par(mfrow=c(3,1))
library(randomForest)
partialPlot(res_rf2$finalModel, iris, x.var=Petal.Width,which.class="setosa", main="Partial Dependence für setosa")
partialPlot(res_rf2$finalModel, iris, x.var=Petal.Width,which.class="versicolor", main="Partial Dependence für versicolor")
partialPlot(res_rf2$finalModel, iris, x.var=Petal.Width,which.class="virginica", main="Partial Dependence für virginica")
partialPlot(res_rf2$finalModel, iris, x.var=Sepal.Width,which.class="versicolor")


## Stratifiziertes Sampling
######################
library(imbalance) # für Datensatz Glas
head(glass0)
?glass0
table(glass0$Class)
res_unba <- train(Class ~., data=glass0, method="rf", 	trControl = fitControl, tuneLength=3)

res_unba$finalModel

res_strat <- train(Class ~., data=glass0, method="rf", 	trControl = fitControl, tuneLength=3, strata=glass0$Class, sampsize=c(70,70))

res_strat$finalModel


## Arbeitsblatt 5: Aufgabe 2
#############################
#a)
load("Daten/Churn1.rdata")
set.seed(7)
intrain <- createDataPartition(y=churn$Churn, p=0.8, list=F)
churn_train <- churn[intrain,]
churn_test <- churn[-intrain,]
library(caret)
ctrl <- trainControl(method="repeatedcv", number=10, repeats = 3) 
tree_fit <- train(Churn ~., data = churn_train, method = "rpart", trControl=ctrl, 
                  tuneLength =10)
tree_fit
pred_train_tree <- predict(tree_fit, newdata=churn_train)
confusionMatrix(data=pred_train_tree , reference=churn_train$Churn)
pred_test_tree <- predict(tree_fit, newdata=churn_test)
confusionMatrix(data=pred_test_tree , reference=churn_test$Churn)

## 1-knn 
knn_1 <- train(Churn ~., data = churn_train, method = "knn", trControl=ctrl, 
               tuneGrid=data.frame(k=1), preProcess = c("center", "scale"))
knn_1
## kategorielle Daten als dummy-Variablen (0/1), caret macht das automatisch
pred_train_knn1 <- predict(knn_1, newdata=churn_train)
confusionMatrix(data=pred_train_knn1, reference=churn_train$Churn)
pred_test_knn1 <- predict(knn_1, newdata=churn_test)
confusionMatrix(data=pred_test_knn1, reference=churn_test$Churn)
## knn
knn_fit <- train(Churn ~., data = churn_train, method = "knn", trControl=ctrl, 
                 tuneLength  =10, preProcess = c("center", "scale"))
knn_fit
pred_train_knn <- predict(knn_fit, newdata=churn_train)
confusionMatrix(data=pred_train_knn, reference=churn_train$Churn)
pred_test_knn <- predict(knn_fit, newdata=churn_test)
confusionMatrix(data=pred_test_knn, reference=churn_test$Churn) 
# b) 
ctrl <- trainControl(method="oob") 
rf_fit <- train(Churn ~., data = churn_train, method = "rf", trControl=ctrl, 
                tuneLength =3)

rf_fit
# Fehlerrate auf den Trainingsdaten
pred_train_rf <- predict(rf_fit, newdata=churn_train)
confusionMatrix(data=pred_train_rf, reference=churn_train$Churn)

# Fehlerrate auf den Testdaten
pred_test_rf <- predict(rf_fit, newdata=churn_test)
confusionMatrix(data=pred_test_rf, reference=churn_test$Churn) 

#c)
rf_fit$finalModel

# d)
varImp(rf_fit)

# e)
table(churn$Churn)
table(churn_train$Churn)
rf_strat <- train(Churn ~., data=churn_train, method="rf", 	trControl = fitControl, tuneLength=3, strata=churn_train$Churn , sampsize=c(350,350), ntree=1000)

rf_strat

# Fehlerrate auf den Trainingsdaten
pred_train_rf <- predict(rf_strat, newdata=churn_train)
confusionMatrix(data=pred_train_rf, reference=churn_train$Churn)

# Fehlerrate auf den Testdaten
pred_test_rf <- predict(rf_strat, newdata=churn_test)
confusionMatrix(data=pred_test_rf, reference=churn_test$Churn)



## Arbeitsblatt 5: Aufgabe 3
#############################
#a)
load("Daten/otto.rdata")
table(otto$target)
barplot(table(otto$target), las = 2)
table(otto$target) / sum(table(otto$target))

#b) 
set.seed(12)
intrain <- createDataPartition(y=otto$target, p=0.8, list=F)
otto_train <- otto[intrain,]
otto_test <- otto[-intrain,]
table(otto_train$target)/length(otto_train$target)
table(otto_test$target)/length(otto_test$target)
samp <- sample(1:length(otto$target), size=0.8*length(otto$target), replace=F)
samp_train <- otto[samp,]
samp_test <- otto[-samp,]
table(samp_train$target)/length(samp_train$target)
table(samp_test$target)/length(samp_test$target)


# c) 
ctrl <- trainControl(method="oob") 
rf_otto <- train(target ~., data=otto_train, method="rf", 	
                 trControl = fitControl, tuneLength=3, ntree=300)

rf_otto
# Evaluation auf den Testdaten
pred_otto <- predict(rf_otto, newdata=otto_test)
confusionMatrix(data=pred_otto, reference=otto_test$target)


# d) 
set.seed(11)
ctrl <- trainControl(method="oob") 
table(otto$target)
rf_otto_strat <- train(target ~., data=otto_train, method="rf", 
                       trControl = fitControl, tuneLength=3,  
                       strata=otto$target, sampsize=rep(96,9), ntree=1000)

rf_otto_strat
# Evaluation auf den Testdaten
pred_otto_strat <- predict(rf_otto_strat, newdata=otto_test)
confusionMatrix(data=pred_otto_strat, reference=otto_test$target)



