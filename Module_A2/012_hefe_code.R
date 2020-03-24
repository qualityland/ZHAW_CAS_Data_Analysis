library(readr)

# data set also in package 'HistData'
library(HistData)
YeastD.mat

Hefezellen <- read_csv("./Module_A2/data/Hefezellen.dat", col_names = FALSE)
hefe <- Hefezellen$X1
summary(hefe)
table(hefe)
barplot(table(hefe))

tab <- table(hefe)
names(tab)
plot(as.integer(names(tab)), as.integer(tab), type = "h")
lines(as.integer(names(tab)), as.integer(tab), type = "b", col = "red")

table(hefe)
prop.table(table(hefe))
sum(table(hefe))
table(hefe) / sum(table(hefe))
tab <- table(hefe)
k <- as.integer(names(tab))
k
hk <- as.integer(tab) # abs. Haeufigkeiten
hk
fk <- hk / sum(hk) # rel. Haeufigkeiten
sum(k * fk)

mean(hefe)

