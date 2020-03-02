### Wednesday, 26.02.2020

# Slides:
#   datenmanipulation.pdf
#   datenmanipulation_beispiel.pdf

############################# dplyr #############################

# Datenmanipulation (using dplyr)
library(dplyr)

# Keyboard shortcut for %>%:
# Cmd + Shift + M

iris <- as_tibble(iris)
iris
class(iris)

# slice() - choose rows
iris %>% slice(1)


# filter() - only rows matching conditions
iris %>% filter(Species == "setosa" & Petal.Width > 0.3)



# select() - columns / variables


# arrange() - sort
iris %>% arrange(desc(Species), Petal.Width, Petal.Length)


# mutate() - create new variables / columns


# group_by() - group data


# sumarize() - calculate statistics
# 



# Exercises "datenmanipulation_beispiel"
data(Cars93, package = "MASS")

# 1.
# 1a)
str(Cars93)
?MASS::Cars93

# 1b)
df <- as_data_frame(Cars93)
df

# 1c)
df %>%
  filter(Horsepower < 100) %>% 
  nrow

# 1d) Entfernen Sie die Variable Rev.per.mile.
df %>% 
  select(-Rev.per.mile) %>% 
  colnames

# 1e) Ordnen Sie den Datensatz absteigend nach Variable Cylinders und innerhalb
#     von Cylinders aufsteigend nach Passengers. Geben Sie anschließend nur
#     diese beiden Variablen aus.
df %>% 
  arrange(desc(Cylinders), Passengers) %>% 
  select(Cylinders, Passengers)


# 1f) Benennen Sie Variable Horsepower um. Der neue Name soll PS sein.
df %>% 
  rename(PS = Horsepower) %>% 
  colnames


# 2a) Berechnen Sie auf Variable MPG.city die Variable liter_km_city
#     dabei gilt: liter_km_city ~ 235.2146 / MPG.city
df %>% 
  mutate(liter_km_city = 235.2146 / MPG.city) %>% 
  select(liter_km_city, MPG.city)


# 2b) Erstellen Sie eine Variable 'Preiskategorie' die den Wert 'billig'
#     aufweist, wenn Variable Price kleiner dem 0.25er Quantil ist
#     (Hinweise: ?ifelse oder ?cut und ?quantile) und 'nicht billig' sonst.

# 2d) Berechnen Sie für billige und nicht billige Autos (Variable Preiskategorie)
#     folgende Werte:
#        die Gruppengröße
#        die mittlere PS-Anzahl (Variable PS)
#        den Durschnittsverbrauch in der Stadt (Variable liter_km_city)

df %>% 
  rename(PS = Horsepower) %>% 
  mutate(liter_km_city = 235.2146 / MPG.city) %>% 
  mutate(Preiskategorie = case_when(Price < quantile(df$Price)["25%"] ~ "billig",
                                    Price >= quantile(df$Price)["25%"] ~ "nicht billig")) %>% 
  group_by(Preiskategorie) %>% 
  summarise(count = n(),
            mean_PS = mean(PS),
            mean_l_km_city = mean(liter_km_city))
