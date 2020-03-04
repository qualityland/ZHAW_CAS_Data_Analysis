### Wednesday, 26.02.2020

# Slides:
#   datenmanipulation_beispiel.pdf

library(dplyr)

# 1a. load dataset
data(Cars93, package = "MASS")

# 1b. create local tibble
cars <- as_tibble(Cars93)

# 1c. find rows with Horsepower < 100
cars %>% 
  filter(Horsepower < 100) %>% 
  nrow()

# 1d. remove column Rev.per.mile
cars %>% 
  select(-Rev.per.mile) %>% 
  colnames()

# 1e. order by Cylinders (desc) and Passengers (asc)
cars %>% 
  select(Manufacturer, Model, Cylinders, Passengers, Horsepower, Origin) %>% # overview
  arrange(desc(Cylinders), Passengers)

# 1f. rename Horsepoer to PS
cars %>% 
  select(Manufacturer, Model, Cylinders, Passengers, Horsepower, Origin) %>% # overview
  arrange(desc(Cylinders), Passengers) %>% 
  rename(PS=Horsepower)

# 2a. calculate 'liter_km_city' based on MPG.city
cars %>% 
  mutate(liter_km_city = 235.2146 / MPG.city) %>% 
  arrange(desc(MPG.city)) %>% 
  select(Manufacturer, Model, MPG.city, liter_km_city)

# 2b. add column Price.Cat with 'cheap' (price < 0.25 quantile) or 'expensive'
cars %>% 
  mutate(Price.Cat = ifelse(Price <= quantile(Price, 0.25), "cheap", "expensive")) %>% 
  select(Manufacturer, Model, Price, Price.Cat) %>% 
  filter(Price <= 15 & Price >= 11)

# 2c. group by Price.Cat
cars %>% 
  mutate(Price.Cat = ifelse(Price <= quantile(Price, 0.25), "cheap", "expensive")) %>% 
  group_by(Price.Cat) %>% 
  select(Manufacturer, Model, Price, Price.Cat)

# 2d. calculate n(), mean(PS) and mean(liter_km_city) for each group
cars %>% 
  rename(PS=Horsepower) %>% 
  mutate(liter_km_city = 235.2146 / MPG.city) %>% 
  mutate(Price.Cat = ifelse(Price <= quantile(Price, 0.25), "cheap", "expensive")) %>% 
  group_by(Price.Cat) %>% 
  summarize(N=n(), mean(PS), mean(liter_km_city))
