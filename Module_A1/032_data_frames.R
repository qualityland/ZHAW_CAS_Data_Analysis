### Wednesday, 26.02.2020

# Slides:
#   datentypen.pdf
#   datentypen_beispiel.pdf

############################# Data Frames #############################


nn <- c("Bernhard", "Matthias", "Angelika")

df1 <- data.frame(
  namen = nn,
  geschl = c("m", "m", "w"),
  groesse = c(185, 182, 165)
)

df1
str(df1)
df1[c(1, 3), ]
df1$namen
df1[df1$groesse <= 180, ]



############################# Exercises: Data Frames ##########################

# 8a. Erzeugen Sie einen Data Frame df aus Cars93 mit den folgenden Variablen:
#    Manufacturer, Type, Price und Luggage.room.
data(Cars93, package = "MASS")

df <- Cars93[ , c("Manufacturer", "Type", "Price", "Luggage.room")]
df

# or
vars <- c("Manufacturer", "Type", "Price", "Luggage.room")
df <- Cars93[, vars]

# 8b. Betrachten Sie die Struktur von df.
str(df)

# 8c. Berechnen Sie die Anzahl der Zeilen und Spalten von df.
nrow(df)
ncol(df)
# or
dim(df)[1]  # rows
dim(df)[2]  # columns

# 8d. Extrahieren Sie aus df alle Zeilen, bei denen Volkswagen der
#    Hersteller (Variable Manufacturer) ist und weisen Sie das
#    Ergebnis einem Objekt df2 zu.
df2 <- df[df$Manufacturer == "Volkswagen", ]
df2

# 8e. Extrahieren Sie auf zwei Arten Variable Price von df2.
df2$Price
df2[, c("Price")]
df2[, "Price"]
df2[, 3]
df2[, -c(1, 2, 4)]
df2[, c(FALSE, FALSE, TRUE, FALSE)]

# f. Geben Sie die Variablennamen von df2 zurück.
colnames(df2)


# 9a. Verwenden Sie das Objekt df aus 8a)
df <- Cars93[ , c("Manufacturer", "Type", "Price", "Luggage.room")]

# 9b. Fügen Sie einen logischen Vektor index hinzu, der genau dann TRUE ist,
#     wenn Luggage.room nicht NA ist und interpretieren Sie das Ergebnis.
df$index <- !is.na(df$Luggage.room)
df[df$index,]

# 9c. Erstellen Sie eine Berechnung, deren Ergebnis Infinum (Inf) ist und
#     überprüfen Sie das Ergebnis.
is.infinite(5/0)

