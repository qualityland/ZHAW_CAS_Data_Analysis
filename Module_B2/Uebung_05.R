###
### ML_Uebung_05
###
zr.plot.resid <- function( res, zr.name = '')
{
  library(car)
  par(mfrow = c(2,2))
  plot( res, main = paste("Residuen von ", zr.name ,sep =""))
  qqPlot( res, main = 'qqPlot')
  acf( res , na.action = na.pass, ylim = c(-1,1), main  ='ACF')
  pacf( res, na.action = na.pass, ylim = c(-1,1), main = 'PACF')
  par(mfrow = c(1,1))
}


setwd('/Users/hofc/idp/Public/Lehre/CAS/CAS_18_8/Daten')
voc2 <- read.table( 'voc2.dat',sep = ';', header = T)

ts.voc <- ts(voc2[, c('t','rf' , 'O3')])

pairs( data.frame( ts.voc))

###
### Regression
###

lm.voc2 <- lm ( O3 ~., data = ts.voc)
summary( lm.voc2 )

zr.plot.resid( lm.voc2$residuals)

## Residuen zeigen eine Korrelation
burg.fit  <- ar.burg( lm.voc2$residuals)
zr.plot.resid( burg.fit$resid )
alpha <- burg.fit$ar

ts.voc.stern <- ts.voc - alpha*lag( ts.voc, -1)
head( ts.voc.stern) ### Achtung Colnames
colnames( ts.voc.stern ) <- colnames( ts.voc)
head( ts.voc.stern)
###
### oder
### 
ts.voc.t.stern <- ts.voc[,'t']- alpha * lag(ts.voc[,'t'], -1)
ts.voc.rf.stern <- ts.voc[,'rf']- alpha * lag(ts.voc[,'rf'], -1)
ts.voc.O3.stern <- ts.voc[,'O3']- alpha * lag(ts.voc[,'O3'], -1)
ts.voc.stern <- data.frame(O3 = ts.voc.O3.stern, rf = ts.voc.rf.stern, t =ts.voc.t.stern)
head( ts.voc.stern)

### Regression mit transformierten Daten
lm.voc2.stern <- lm ( O3 ~., data = ts.voc.stern)
summary( lm.voc2.stern)
zr.plot.resid( lm.voc2.stern$residuals )

beta0.hat <- coef(lm.voc2.stern)[1] / (1-alpha)
se.beta0.hat <- coef(summary( lm.voc2.stern ) )[1,2] / (1-alpha)

beta0.hat 
se.beta0.hat

coef(summary( lm.voc2 ))[1,1:2]

###
### GLS
###
library(nlme)
ts.time <- time( ts.voc[, 'O3'])    

gls.voc <- gls( O3 ~. , data = ts.voc,
                correlation = corARMA(form = ~ ts.time, p = 1))
summary( gls.voc)
zr.plot.resid( gls.voc$residuals )

lm.voc2

lm.voc2.stern; beta0.hat 
