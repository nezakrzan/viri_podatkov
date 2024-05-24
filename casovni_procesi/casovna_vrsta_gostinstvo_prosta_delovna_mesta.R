# knjiznice
library(readxl)
library(astsa)
library(car)
library(splines)

# uvoz podatkov
# imena posameznih sheet-ov
# excel_sheets("Podatki za seminarske naloge.xlsx")

# Prosta in zasedena delovna mest
data1 = read_excel("Podatki za seminarske naloge.xlsx",
                   sheet = "Prosta in zasedena delovna mest")
# Indeks prihodka po dejavnostih
data2 = read_excel("Podatki za seminarske naloge.xlsx",
                   sheet = "Indeks prihodka po dejavnostih")

zasedena_1 = data1[,"Zasedena_1+_I GOSTINSTVO"]
prosta_1 = data1[,"Prosta_1+_I GOSTINSTVO"]

nastanitvene_dejavnosti = data2[,"Gostinske nastanitvene dejavnosti"]
strezba_jedi_pjac = data2[, "Dejavnost strežbe jedi in pijač"]


# casovni interval 2010M1-2024M1
ts_nastanitve = ts(nastanitvene_dejavnosti, start = c(2010,1), frequency = 12)
ts_strezba = ts(strezba_jedi_pjac, start = c(2010,1), frequency = 12)

# preveriva ce je zadnje obdobje res 2024M1
# end(ts_nastanitve)
# end(ts_strezba)

# casovni interval 2028Q1-2023Q4
ts_zasedena = ts(zasedena_1, start = c(2008, 1), frequency = 4)
ts_prosta = ts(prosta_1, start = c(2008, 1), frequency = 4)

# preveriva ce je zadnje obdobje res 2023Q4
# end(ts_zasedena)
# end(ts_prosta)

################################################################################
#                     Gostinstvo prosta delovna mesta 1+
################################################################################
# Časovna vrsta za prosta delovna mesta v gostinstvu, kjer je zaposlena vsaj 1 oseba.

# prikaz casovne vrste
plot.ts(ts_prosta, type = "o", pch = 16, cex=0.6, 
        xlab = "leto", ylab = "stevilo delovnih mest",  xaxt="n")
axis(1, at=c(2008, 2011, 2014, 2017, 2020, 2023), 
     labels=c("2008", "2011", "2014", "2017", "2020", "2023"))

# gladilnik
gladilnik <- lowess(time(ts_prosta),ts_prosta)
points(gladilnik$x, gladilnik$y, type="l", col = "blue")

############################### transformacija #################################

time = 1 + frequency(ts_prosta) * (time(ts_prosta) - start(ts_prosta)[1])

model_prosta1 = lm(ts_prosta~ time)
model_prosta2 = lm(ts_prosta~ time + I(time^2))
model_prosta3 = lm(ts_prosta~ time + I(time^2) + I(time^3))
model_prosta4 = lm(ts_prosta~ ns(time, df = 8))

anova(model_prosta1, model_prosta2, model_prosta3, model_prosta4)

# plot(time, ts_prosta, type = "o", cex=0.5)
# points(time, model_prosta4$fitted.values, type="l", col = "red")

# Box-Cox za 'Gostinstvo prosta delovna mesta 1+'.
boxCox(model_prosta4)
# Lambda = 0 --> log transformacija

ts_prosta_log = log(ts_prosta) 

################################# ACF in PACF #################################
# Avtokorelogram in parcialni avtokorelogram za 'Gostinstvo prosta delovna mesta 1+'.

acf2(ts_prosta_log, main = "", max.lag = 63)
# trend - vrednosti avtokorelacijskih koeficientov počasi padajo
# morda prisotna sezonskot?
# pacf izgleda okej
# torej verjetno samo AR model? 

############################### diferenciranje ###############################
# ker je trend rabimo diferencirati
# 1. stopnje - linearni trend
par(mfrow=c(1,2))
plot(ts_prosta_log, main="osnovna", xlab="leto", ylab="stevilo delovnih mest")
plot(diff(ts_prosta_log, lag=1), main="lag=1 - diferenciranje 1. stopnje", xlab="leto", ylab="stevilo delovnih mest")
#   - na levi strani drugega grafa(de leta 2020) bi lahko rekli da je pričakovana vrednost enaka 0
#   - tudi variabilnost na levi strani izgleda končna
#   - na desni strani se še vednomopazi padec,ki se je zgodil v letu 2020

par(mfrow=c(1,1))
acf(diff(ts_prosta_log, lag=1))
# vecji koeficienti avtokorelacije pri 1.5 lag in 3 lag
# je to morda nekaj kar se vedno malo bolj vpliva na avtokorelacijo, sicer je statisticno neznacilno

################################### model ###################################
# model za diferenciranje
mod.dif <- arima(ts_prosta_log, order = c(0, 1, 0), method = "CSS-ML")
mod.dif$aic

# diagnostika ostankov modela - isto kot zgoraj
par(mfrow=c(1,3))
plot(ts(mod.dif$res), ylab = "mod.dif$resid");
acf(mod.dif$res, na.action = na.pass, main = "")
pacf(mod.dif$res, na.action = na.pass, main = "")

# H0: avtokorelacijski koeficienti (ACF) časovne vrste enake nič
Box.test(mod.dif$res, lag = 20, type = c("Ljung-Box"), fitdf = 2)
# na podlagi zgornjega testa  lahko za ostanke tega modela privzamemo beli šum

library(astsa)
# test značilnosti 
# H0: posamezen parameter je enak 0
# če so parametri statistično neznačilni potem je to verjetno preparametrizacija
ocene <- sarima(ts_prosta_log, p = 0, d = 1, q = 0, details = TRUE)
ocene$ttable 
# ni stat. značilno

################################### napovedi ###################################
# ucna in testna
temp.ucna <- window(ts_prosta_log, start = c(2008,1), end = c(2019,4))
temp.test <- window(ts_prosta_log, start = c(2020,1), end = c(2023,4))

par(mfrow=c(1,1))
# n.ahead = 4 leta * 4 kvartali
sarima.for(temp.ucna, n.ahead = 4*4, p=0, d=1, q=0, plot.all = TRUE)
points(temp.test, type = "o")
# zelo slabe napovedi, skoraj prepricani smo da model ne bi zaznal padca v letu 2020

sarima.for(ts_prosta_log, n.ahead = 4*4, p=0, d=1, q=0, plot.all = TRUE)
#points(temp.test, type = "o")
# isto kr neki

