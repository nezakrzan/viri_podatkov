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
#                     Dejavnost strežbe jedi in pijač
################################################################################



############################### diferenciranje ###############################
# Kaj se zgodi če diferenciram?
# 1. stopnje - linearni trend
par(mfrow=c(1,2))
plot(ts_strezba, main="osnovna", xlab="leto", ylab="indeks prihodka")
plot(diff(ts_strezba, lag=1), main="lag=1 - diferenciranje 1. stopnje", xlab="leto", 
     ylab="indeks prihodka")
#   - na levi strani drugega grafa(de leta 2020) bi lahko rekli da je pričakovana vrednost enaka 0
#   - tudi variabilnost na levi strani izgleda končna
#   - na desni strani se še vednomopazi padec,ki se je zgodil v letu 2020

par(mfrow=c(1,1))
acf2(diff(ts_strezba, lag=1))
# vecji koeficienti avtokorelacije pri 0.4 lag in 1.4 lag ter en se pojavi pri 1, 2, 3,...
# torej definitivno je neka sezonskost oziroma nekaj kar se vedno vpliva na avtokorelacijo...
# je to morda nekaj kar se vedno malo bolj vpliva na avtokorelacijo
# je to potem mogoce d=1 in D=1?

par(mfrow=c(1,2,))
plot(diff(ts_strezba, lag=1), main="lag=1 - diferenciranje 1. stopnje", xlab="leto", 
     ylab="indeks prihodka")
plot(diff(diff(ts_strezba, lag=1), lag=12), main="lag=1 in lag=12", xlab="leto", 
     ylab="indeks prihodka")

par(mfrow=c(1,1))
acf2(diff(diff(ts_strezba, lag=1), lag=12))
# acf: nekaj statistično značilnih koeficientov še vedno, torej bomo verjetno sikali primeren q oz. MA(q) model...
#      ne opazim več statistično značilnih koeficientov, ki bi se periodično ponavljali
# pacf: na odlohih 1, 2, 3 in 4 vidimo koeficiente parciallne avtokorelacije, ki poačsi padajo, 
#       na začetku so statistično značilni

# kaj pa ce diferenciramo samo sezonskost, torej D=1?
par(mfrow=c(1,2,))
plot(diff(ts_strezba, lag=1), main="lag=1 - diferenciranje 1. stopnje", xlab="leto", 
     ylab="indeks prihodka")
plot(diff(ts_strezba, lag=12), main="lag=12 - diferenciranje sezonskosti", xlab="leto", 
     ylab="indeks prihodka")
# graf časovne vrste na levi strani izgleda bolje pri diferenciranju 1. stopnje, saj je varianca videti bolj konstantna

par(mfrow=c(1,1))
acf2(diff(ts_strezba, lag=12))
# acf: še vedno veliko koeficientov avtokorelacije statistično značilnih, 
#      torej bi moral biti q nekje 8, 
#      morda celo več, saj se kasneje ponovno pojavijo statistično značilni koeficienti
# pacf: tukaj nimamo statistično značilnih koeficientov, ki bi se pijavljali na določeno periodo, kot so se pri lag=1 in lag=12 skupaj

################################### model ###################################
# preizkušam več različnih modelov

# diferneciranje 1. reda
mod.dif <- arima(ts_strezba, order = c(0, 1, 0), 
                 seasonal = list(order = c(0, 0, 0), 
                                 period = frequency(ts_strezba)))
mod.dif$aic

# diagnostika ostankov modela
par(mfrow=c(1,3))
plot(ts(mod.dif$res), ylab = "mod.dif$resid");
acf(mod.dif$res, na.action = na.pass, main = "")
pacf(mod.dif$res, na.action = na.pass, main = "")

# H0: avtokorelacijski koeficienti (ACF) časovne vrste enake nič
Box.test(mod.dif$res, lag = 20, type = c("Ljung-Box"), fitdf = 2)
# na podlagi zgornjega testa  ne moremo za ostanke  modela privzeti belega šuma

# test značilnosti 
# H0: posamezen parameter je enak 0
# če so parametri statistično neznačilni potem je to verjetno preparametrizacija
ocene <- sarima(ts_strezba, p = 0, d = 1, q = 0, details = TRUE)
ocene$ttable 
# je stat. značilno

# diferneciranje trenda in sezonskosti 
mod.dif2 <- arima(ts_strezba, order = c(0, 1, 0), 
                 seasonal = list(order = c(0, 1, 0), 
                                 period = frequency(ts_strezba)))
mod.dif2$aic # manjši
mod.dif$aic

# H0: avtokorelacijski koeficienti (ACF) časovne vrste enake nič
Box.test(mod.dif2$res, lag = 20, type = c("Ljung-Box"), fitdf = 2)
# na podlagi zgornjega testa  ne moremo za ostanke  modela privzeti belega šuma
ocene <- sarima(ts_strezba, p = 8, d = 1, q = 1, D=1, S=12, details = TRUE)

get.best.arma <- function(x.ts, maxord = c(1,1), metoda = "CSS-ML"){
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) {
    for (q in 0:maxord[2]) {
      fit <- arima(x.ts, order = c(p, 0, q), method = metoda)
      if (fit$aic<best.aic) {
        best.aic <- fit$aic
        best.fit <- fit
      }
    }
  }
  list(best.fit)
}

(best.y.arma <- get.best.arma(diff(ts_strezba, lag=1),
                             maxord = c(10, 2),metoda = "CSS-ML"))
ocene1 <- sarima(ts_strezba, p = 4, d = 1, q = 2, details = TRUE)

(best.y.arma2 <- get.best.arma(diff(ts_strezba, lag=12),
                             maxord = c(10, 2),metoda = "CSS-ML"))
ocene2 <- sarima(ts_strezba, p = 8, d = 0, q = 2, D=1, S=12,details = TRUE) #??

(best.y.arma3 <- get.best.arma(diff(diff(ts_strezba, lag=1),12),
                             maxord = c(10, 2),metoda = "CSS-ML"))
ocene3 <- sarima(ts_strezba, p = 3, d = 1, q = 2, D=1, S=12,details = TRUE) #??
# to nc ne pride okej :(

ocene4 <- sarima(ts_strezba, p = 0, d = 1, q = 0, P=1, D=1, Q=4, S=12, details = TRUE)
arima(ts_strezba, order = c(0, 1, 0), 
      seasonal = list(order = c(0, 1, 4), 
                      period = frequency(ts_strezba_trans)))

################################### napovedi ###################################
# probam...
# ucna in testna
temp.ucna <- window(ts_strezba, start = c(2008,1), end = c(2019,4))
temp.test <- window(ts_strezba, start = c(2020,1), end = c(2023,4))

par(mfrow=c(1,1))
# n.ahead = 4 leta * 4 kvartali
sarima.for(temp.ucna, n.ahead = 4*4, p=3, d=1, q=2, D=1, S=12, plot.all = TRUE)
points(temp.test, type = "o")
# zelo slabe napovedi, skoraj prepricani smo da model ne bi zaznal padca v letu 2020

sarima.for(temp.ucna, n.ahead = 4*4, p=8, d=0, q=2, D=1, S=12, plot.all = TRUE)
points(temp.test, type = "o")
# isto kr neki

# na celotni mnozici
par(mfrow=c(1,1))
# n.ahead = 4 leta * 4 kvartali
sarima.for(ts_strezba, n.ahead = 4*4, p=3, d=1, q=2, D=1, S=12, plot.all = TRUE)
sarima.for(ts_strezba, n.ahead = 4*4, p=8, d=0, q=2, D=1, S=12, plot.all = TRUE)

############################### transformacija #################################
# kaj pa ce vseen probamo transformirat
rez = boxCox(model_str1)
lambda = rez$x[which.max(rez$y)]
# poskusim lambda=1.64

# transformiramo
ts_strezba_trans <- ts_strezba^lambda

# Časovna vrsta indeksa prihodka za Dejavnost strežbe jedi in pijač.
par(mfrow=c(1,2))
plot.ts(ts_strezba_trans, type = "o", pch = 16, cex=0.6, 
        xlab = "leto", ylab = "indeks prihodka", xaxt="n", main="transformirana")
axis(1, at=c(2008, 2011, 2014, 2017, 2020, 2023), 
     labels=c("2008", "2011", "2014", "2017", "2020", "2023"))
# gladilnik
gladilnik2 <- lowess(time(ts_strezba_trans),ts_strezba_trans)
points(gladilnik2$x, gladilnik2$y, type="l", col = "blue")

plot.ts(ts_strezba, type = "o", pch = 16, cex=0.6, 
        xlab = "leto", ylab = "indeks prihodka", xaxt="n", main="osnovna")
axis(1, at=c(2008, 2011, 2014, 2017, 2020, 2023), 
     labels=c("2008", "2011", "2014", "2017", "2020", "2023"))
# gladilnik
gladilnik2 <- lowess(time(ts_strezba),ts_strezba)
points(gladilnik2$x, gladilnik2$y, type="l", col = "blue")
# dokaj podoben graf se vedno, edin y skala drugacna...

################################# ACF in PACF #################################
# Avtokorelogram in parcialni avtokorelogram za 'Dejavnost strežbe jedi in pijač'.
par(mfrow=c(1,2))
acf2(ts_strezba, main = "osnovna")
acf2(ts_strezba_trans, main = "transformirana")
# dokaj podobno

# Kaj se zgodi če diferenciram?
# 1. stopnje - linearni trend
par(mfrow=c(1,2))
plot(ts_strezba_trans, main="transponirana", xlab="leto", ylab="indeks prihodka")
plot(diff(ts_strezba_trans, lag=1), main="lag=1 - diferenciranje 1. stopnje", xlab="leto", 
     ylab="indeks prihodka")

par(mfrow=c(1,1))
acf2(diff(ts_strezba_trans, lag=1))
# acf: tudi tukaj imamo na odlogi 1, 2, 3, 4... avtokorelacijske koeficiente statistično značilne, ki počasi padajo
# pacf: pri lag=1 stat značilen
# ali je to mogoče za določitev Q in P

ocene_trans <- sarima(ts_strezba_trans, p = 0, d = 1, q = 0, P=0, D=0, Q=4, S=12, details = TRUE)
arima(ts_strezba_trans, order = c(0, 1, 0), 
      seasonal = list(order = c(0, 0, 4), 
                      period = frequency(ts_strezba_trans)))
# p - vrednosti niso v redu, nimamo stat. neznačilno

ocene1_trans <- sarima(ts_strezba_trans, p = 0, d = 1, q = 0, P=0, D=1, Q=4, S=12, details = TRUE)
arima(ts_strezba_trans, order = c(0, 1, 0), 
      seasonal = list(order = c(0, 1, 4), 
                      period = frequency(ts_strezba_trans)))
# ni okej...

(best.y.arma_trans <- get.best.arma(diff(ts_strezba_trans, lag=1),
                               maxord = c(13, 1),metoda = "CSS-ML"))
ocene2_trans <- sarima(ts_strezba_trans, p = 12, d = 1, q = 0, P=0, D=0, Q=0, S=12, details = TRUE)
# tud ni okej...
# ampak je pa vsaj acf of residuals brez stat značilnih...
