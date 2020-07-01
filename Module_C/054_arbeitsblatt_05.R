library(MASS)
library(caret)

r.tree <- train(sex ~., data=crabs, method='rpart', tuneGrid=data.frame(cp=0.01))
r.tree$finalModel

plot(r.tree$finalModel, margin=0.2, uniform=TRUE)
text(r.tree$finalModel, use.n=TRUE)


library(rattle)
fancyRpartPlot(r.tree$finalModel)
