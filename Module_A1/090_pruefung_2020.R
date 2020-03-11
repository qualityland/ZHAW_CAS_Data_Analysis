# Wednesday 11.03.2020
# Pruefung
library(dplyr)

load("./Module_A1/data/covid19.RData")
str(covid19)
dim(covid19)

table(covid19$Confirmed)
hist(covid19$Confirmed)
plot(covid19$Confirmed)
head(covid19)


plot(covid19$Country.Region)



boxplot(Deaths ~ Country.Region, data = covid19 %>%  filter(Deaths > 0),
        horizontal = TRUE, las = 1, log = "x", ylab = "", cex.axis = 0.6)


pairs(covid19[, c(7, 8, 6)])

cor(covid19$Confirmed, covid19$Deaths)
cor(covid19$Confirmed, covid19$Recovered)

covid19 %>% 
  group_by(Country.Region) %>% 
  summarise(n=n(Deaths))

covid19 %>% 
  group_by(Country.Region) %>% 
  summarise(Deaths, sum)
