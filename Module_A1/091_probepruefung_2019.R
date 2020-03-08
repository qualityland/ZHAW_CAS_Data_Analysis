library(dplyr)

# 3.1 FALSE
# 3.2 FALSE
# 3.3 TRUE
# 3.4 TRUE
# 3.5 FALSE
# 3.6 FALSE
# 3.7 FALSE
# 3.8 TRUE

load("./Module_A1/data/bikes.RData")

# 1.1
dim(bikes)
nrow(bikes)    # 8645 Zeilen
ncol(bikes)    # 17 Variablen

# 1.2
str(bikes)     # 7: season, yr, mnth, hr, holiday, weekday, weathersit
sum(sapply(bikes, is.factor))

# 1.3
# Verhaeltnisskala

# 1.4
# quantitativ-diskret

# 1.5
bikes$hr <- as.integer(as.character(bikes$hr))
str(bikes)
table(bikes$hr)

# 2.1
hist(bikes$casual, xlab = "Anzahl Gelegenheitsnutzer")
barplot(table(bikes$weathersit),
        xlab = "Weathersituation",
        ylab = "Anzahl Ausleihen")

# 2.2
hist(bikes$cnt)
# rechtsschief

# 2.3
boxplot(cnt ~ hr, data = bikes)
# zweigipflig mit Maximum um 8h und 17h

# 2.4
library(dplyr)
x <- bikes %>% 
  filter(dteday == "2011-08-14") %>% 
  select(cnt, hr)

plot(cnt ~ hr, data = x, type = "b")
# oder:
plot(x$cnt ~ x$hr)
lines(x$cnt ~ x$hr)

# 2.5
m <- bikes %>% 
  group_by(mnth) %>% 
  summarise(mean_cnt=mean(cnt))
barplot(mean_cnt ~ mnth,
        data = m,
        xlab = "Monat",
        ylab = "Durchschnittliche Anzahl Ausleihen")

# 2.6
cor(sqrt(bikes$cnt), bikes$hum)
# -0.316 also schwach negativ. Je hoeher die Luftfeuchtigkeit, umso weniger Ausleihen.
plot(sqrt(cnt) ~ hum, data = bikes)

# dasselbe robust:
library(robustbase)
# covMcd() benoetigt einen data frame als Parameter
x <- bikes %>% 
  mutate(scnt = sqrt(cnt)) %>% 
  select(scnt, hum)

covMcd(x, cor = TRUE)$cor
