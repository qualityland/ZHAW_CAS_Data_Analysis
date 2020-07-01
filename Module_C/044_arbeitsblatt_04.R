
# Aufgabe 1 - Stock Market
library(ISLR)
data("Smarket")

# data.path <- '/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_C/data/'
# load(paste0(data.path, 'smarket.rdata'))


# a)
dim(Smarket)
summary(Smarket)
str(Smarket)
table(Smarket$Year)
ts.plot(Smarket$Today)

train_set <- Smarket[Smarket$Year < 2005,]
test_set <- Smarket[Smarket$Year == 2005, ]

head(train_set)
head(test_set)
