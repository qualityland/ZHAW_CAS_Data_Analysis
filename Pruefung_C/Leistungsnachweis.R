# UCI Heart failure clinical records Data Set
# URL: https://archive.ics.uci.edu/ml/datasets/Heart+failure+clinical+records
# Dataset URL: https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv

df <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv')

pairs(df)
summary(df)
table(df$sex)

# Altersverteilung
hist(df$age)
# Geschlechteraufteilung
round(mean(df$sex), digits = 2)
# Anämie [%]
round(mean(df$anaemia), digits = 2)
# Diabetiker [%]
round(mean(df$diabetes), digits = 2)
# Bluthochdruck-Patienten [%]
round(mean(df$high_blood_pressure), digits = 2)
# Raucher [%]
round(mean(df$smoking), digits = 2)
# 
par(mfrow = c(2, 3))
hist(df$age, xlab = 'Alter [Jahre]', xlim = c(35, 100), main = 'Altersverteilung')
hist(df$ejection_fraction, xlab = 'Auswurf-Fraktion [%]', main = 'Herzleistung')
hist(df$serum_creatinine, xlab = 'Creatinin [mg/dl]', main = 'Serum-Creatinin')
hist(df$serum_sodium, xlab = 'Na [mEq/l]', main = 'Serum-Natrium')
hist(df$platelets, xlab = expression(paste('[kiloplatelets/', mu, 'l]')), main = 'Thromozyten')
hist(df$creatinine_phosphokinase, xlab = expression(paste('CK im Blut [', mu, 'g/l]')), main = 'Creatin-Kinase')


