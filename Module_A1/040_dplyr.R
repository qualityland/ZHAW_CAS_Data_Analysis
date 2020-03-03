### Wednesday, 26.02.2020

# Slides:
#   datenmanipulation.pdf
#   datenmanipulation_beispiel.pdf

############################# dplyr #############################

# Datenmanipulation (using dplyr)
library(dplyr)

# load iris dataset
data(iris)

# inspect data
head(iris)
str(iris)
summary(iris)

class(iris)

# make 'iris' a 'tibble
iris <- as_tibble(iris)
class(iris)
print(iris)



##########################  %>%  - the Pipe Operator ##########################
# Keyboard shortcut for Pipe '%>%':
# Cmd + Shift + M
iris %>% print



############################### slice() - choose Rows #########################

# first row
iris %>% slice(1)

# last row
iris %>% slice(n())                             # n() returns number of rows

# with Pipe (%>%)
iris %>% slice(1)
iris %>% slice(n())

# without Pipe (%>%)
slice(iris, 1)
slice(iris, n())

# choose multiple rows
iris %>% slice(c(1, 4, 10, 15, n()))



##############  filter() - choose Rows matching certain conditions ############

# filter by two conditions
iris %>% filter(Species == "setosa" & Petal.Width > 0.3)



################ arrange() - sort Data by one or more Columns #################

# sort by one Columns
iris %>% arrange(Sepal.Length)

# descendant by Species, then ascendant by Petal Width and Length
iris %>% arrange(desc(Species), Petal.Width, Petal.Length)



####################### select() - only certain Columns #######################

# select two Columns
iris %>% select(Sepal.Length, Species)

# select a sequence of Columns
iris %>% select(Petal.Length:Species)

# select all Columns except the ones with leading minus (-)
iris %>% select(-Petal.Width, -Sepal.Length)

# functions to use within select():
#       starts_with()
#       ends_with()
#       contains()
#       matches()
#       num_range()

# select all Columns beginning with 'Petal'
iris %>% select(starts_with("Petal"))

# select all Columns containing 'width'
iris %>% select(contains("width"))

# select all Columns NOT containing 'width'
iris %>% select(-contains("width"))



######################## rename() - rename Columns ########################

# rename a Column
iris %>% rename(Species2=Species)

# select() can also be used
# but returns only the specified Columns
iris %>% select(Species2=Species, Petal.Width)



####################### distinct() - filters unique Rows ######################

# 150 Rows
iris %>% dim

# but only 149 distinct Rows
iris %>% distinct %>% dim

# distinct Values of a Variable
iris %>% distinct(Species)



######################## mutate() - add new Columns ###########################

# add logical Column 'is_sertosa'
iris %>% mutate(is_setosa = Species == "setosa")

# case_when()
# add Column 'sepal2' with value short/long depending on Sepal.Length
iris %>%
  mutate(sepal2 = case_when(Sepal.Length < 5 ~ "short",
                            Sepal.Length >= 5 ~ "long")) %>%
  select(Species, Sepal.Length, sepal2)

# ifelse()
iris %>%
  mutate(sepal2 = ifelse(Sepal.Length < 5, "short", "long")) %>% 
  select(Species, Sepal.Length, sepal2)

######################## transmute() - only new Columns #######################

# transmute returns only specified Columns
# here: is_setosa, Species
iris %>% transmute(is_setosa = Species == "setosa", Species)

# newly created Columns can be used in the same statement
iris %>%
  select(starts_with("Sepal"), Species) %>%
  arrange(Sepal.Width) %>%
  mutate(logicalV = Species == "setosa", numV = as.numeric(logicalV))

  

########################### group_by() - group Data ###########################

# group by 'Species'
# summarise: - count for each Species
#            - min/max Sepal.Length
iris %>%
  group_by(Species) %>%
  summarise(
    count = n(),
    min_Sepal.Length = min(Sepal.Length),
    max_Sepal.Length = max(Sepal.Length)
  )

# group by and show first two records
iris %>%
  group_by(Species) %>%
  slice(1:2)

###################### sumarize() - calculate statistics ######################

# summarize_all()
# summarize 'mean' of all Columns
# returns NA for Species (not numeric/logical)
iris %>%
  group_by(Species) %>% 
  summarize_all(mean)

# summarize_at()
# iris %>% 
#   group_by(Species) %>% 
#   summarize_at(Species, mean)


# summarize_if()
iris %>%
  summarize_if(is.numeric, mean)

iris %>%
  group_by(Species) %>%
  summarize_if(is.numeric, list(mean, median)) %>%
  select(starts_with("Sepal"))


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
