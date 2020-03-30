##
## CAS Datenanalyse:
## Modul 'Statistische Grundlagen der Datenanalyse'
##
## HT3: Intermezzo Coronavirus
## **********

## Auf der Website
## "https://de.wikipedia.org/wiki/COVID-19-F�lle_in_der_Schweiz_und_Liechtenstein"
## gibt es Informationen �ber die Verbreitung des Coronaviruses in der Schweiz
## und Lichtenstein. Ich habe mir mal die Fallzahlen pro Kanton vom 14. M�rz
## angeschaut und mich gefragt, welcher Kanton ist am meisten betroffen.

## - Die h�chste Fall Zahl mit 262 hat das Tessin. OK
## - Ist aber der Kanton Genf mit 107 F�llen fast gleich stark betroffen wie
##   der Kanton Basel-Stadt mit 119 F�llen ?




## A. F�lle pro 10'000 Einwohnern
##    ***************************
## Wir k�nnten doch das mal ansehen und die Fallzahlen pro Kanton pro
## 10'000 Einwohnern anschauen.


## Ich habe mir mal die Fallzahlen pro Kanton vom 14. M�rz heruntergeladen
## sowie die jeweilige Wohnbev�lkerung in Einheiten von 10'000.

load("Daten4ModulA2/Coronavirus200314.rda")  ## --> Covid19

## Fallzahlen pro Kanton und 10'000 Einwohnern darstellen

plot(Covid19$Faelle/Covid19$Einwohnerzahl, type="h", lwd=6, col="blue", lend=2)
Covid19$rAF�lle <- Covid19$Faelle/Covid19$Einwohnerzahl

plot(Covid19$rAF�lle, type="h", lwd=6, col="blue", lend=2, xaxt="n",
     ylab="Aanzahl F�lle pro 10'000 Einwohnern",
     xlab="Kantone plus Liechtensteig")
axis(side=1, at=1:nrow(Covid19), labels=paste(Covid19$Kanton))



## Mit Vertrauensintervallen
## *************************
poisson.test(x=Covid19$Faelle[1], T=Covid19$Einwohnerzahl[1])

hh <- poisson.test(x=Covid19$Faelle[1], T=Covid19$Einwohnerzahl[1])
str(hh)


## Also automatisieren:

h.res <- matrix(NA, nrow=nrow(Covid19), ncol=2)
for(i in 1:nrow(Covid19)){
    hh <- poisson.test(x=Covid19$Faelle[i], T=Covid19$Einwohnerzahl[i])
    h.res[i,] <- hh$conf.int
}


## Einzeichnen
segments(1:nrow(Covid19), h.res[,1], 1:nrow(Covid19), h.res[,2],
         col="gray", lwd=2)







## --------------------------------------------------------------------------