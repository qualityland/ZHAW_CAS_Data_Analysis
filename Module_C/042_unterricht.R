data.path <- '/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_C/data/'
load(paste0(data.path, 'Studenten.RData'))


library(caret)
set.seed(25)
train_index <- createDataPartition(Studenten$sex, p = 0.7, list = FALSE)

train_set <- Studenten[train_index, ]
test_set <- Studenten[-train_index, ]

dim(test_set)
dim(train_set)


table(train_set$sex)
table(test_set$sex)

range(train_set$height)
range(test_set$height)

# raten
y_raten <- sample(c('Male', 'Female'), dim(test_set[1]), replace = TRUE)
mean(y_raten == test_set$sex)
