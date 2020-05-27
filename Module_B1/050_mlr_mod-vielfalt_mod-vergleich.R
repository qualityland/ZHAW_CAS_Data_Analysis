
# Daten laden
data.path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B1/data/"
iq <- read.csv(paste0(data.path, 'IQ.csv'), header = TRUE, sep = ';', stringsAsFactors = TRUE)
str(iq)

# Modell mit binaerer Variable (Gender)
fit.iq <- lm(FSIQ ~ Gender + MRI_Count, data = iq)
summary(fit.iq)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)  
# (Intercept) -2.787e+01  5.835e+01  -0.478   0.6359  
# GenderMale  -1.160e+01  9.663e+00  -1.200   0.2382  
# MRI_Count    1.620e-04  6.739e-05   2.404   0.0216 *
  

# Modell mit allen Variablen (incl. binaerer Variable)
fit.iq2 <- lm(FSIQ ~ Gender + MRI_Count + Height + Weight, data = iq)
summary(fit.iq2)
# Call:
# lm(formula = FSIQ ~ Gender + MRI_Count + Height + Weight, data = iq)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -33.527 -16.574  -0.823  16.957  43.780 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  1.318e+02  9.241e+01   1.426   0.1632   
# GenderMale   2.599e+00  1.118e+01   0.233   0.8175   
# MRI_Count    1.999e-04  6.645e-05   3.008   0.0050 **
#   Height      -2.767e+00  1.447e+00  -1.912   0.0646 . 
# Weight      -7.538e-02  2.202e-01  -0.342   0.7343   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 21.6 on 33 degrees of freedom
# Multiple R-squared:  0.2661,	Adjusted R-squared:  0.1772 
# F-statistic: 2.992 on 4 and 33 DF,  p-value: 0.03269


# kategorielle Variablen mit mehr als 2 Auspraegungen
data.path <- "/Users/schmis12/wrk/studio/ZHAW_CAS_Data_Analysis/Module_B1/data/"
ga <- read.table(paste0(data.path, "Softdrink.dat"), header = TRUE)

fit.ga <- lm(Zeit ~ Menge + Distanz + Ort, data = ga)
coef(fit.ga)

dummy.coef(fit.ga)

summary(fit.ga)

# Testen geschachtelter Modelle
# Hat die kategorielle Variable Ort einen signifikaten Einfluss?
fit.voll <- lm(Zeit ~ Menge + Distanz + Ort, data = ga)
fit.red <- lm(Zeit ~ Menge + Distanz, data = ga)
# Vergleich der Modelle
anova(fit.voll, fit.red)

# Analysis of Variance Table
# 
# Model 1: Zeit ~ Menge + Distanz + Ort
# Model 2: Zeit ~ Menge + Distanz
#   Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
# 1     19 169.45                              
# 2     22 233.73 -3   -64.281 2.4025 0.09946 .
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Spezialfall:
# Gibt es irgendeine Beziehung zwischen den erklärenden Variablen
# und der Zielgrösse? (Globaler F-Test)
fit.voll <- lm(Zeit ~ Menge + Distanz + Ort, data = ga)
fit.red <- lm(Zeit ~ 1, data = ga)
anova(fit.voll, fit.red)
# Analysis of Variance Table
# 
# Model 1: Zeit ~ Menge + Distanz + Ort
# Model 2: Zeit ~ 1
#   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1     19  169.5                                  
# 2     24 5784.5 -5   -5615.1 125.92 6.919e-14 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# entspricht dem globlen F-Test
# am Ende des summary() Reports
summary(fit.voll)


fit.ga <- lm(Zeit ~ Menge + Distanz + Ort, data = ga)
drop1(fit.ga, test = "F")
