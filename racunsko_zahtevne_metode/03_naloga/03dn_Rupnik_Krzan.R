set.seed(2024)
# ========= simulacija podatkov ============
n = 150 # velikost vzorca

generiranje_podatkov <- function(beta0, beta1, beta2, hetero, eksp) {
  # generiranje vrednosti za x1 in x2 # x1 <- rnorm(n, 50, 3)
  # x2 <- rnorm(n, 200, 8)
  x1 <- runif(n, 20, 60)
  x2 <- runif(n, 2, 10)
  epsilon <- rnorm(n, 0,x1*hetero) # dodava heteroskedasticnost
  # izračun napovedne spremenljivke za model (parametri = vhodni argumenti) 
  # z ^eksp dodava nelinearni del
  y <- beta0 + beta1 * x1 + beta2 * x2^eksp + epsilon
  # podatke spravimo v podatkovni okvir in vrnemo kot rezultat funkcije
  data.frame(x1 = x1, x2 = x2, y = y) 
}

# izracun vrednosti vzorca
vzorec06_08 = generiranje_podatkov(100, 3, 2, 0.6, 0.8)
vzorec06_14 = generiranje_podatkov(100, 3, 2, 0.6, 1.4)
vzorec1_08 = generiranje_podatkov(100, 3, 2, 1, 0.8)
vzorec1_14 = generiranje_podatkov(100, 3, 2, 1, 1.4)
vzorec12_08 = generiranje_podatkov(100, 3, 2, 1.2, 0.8)
vzorec12_14 = generiranje_podatkov(100, 3, 2, 1.2, 1.4)

# vzorce zdruziva v data frame
vzorec_skupaj = data.frame("alpha" = rep(c(0.6, 1, 1.2), each=2*n),
                           "gamma" = rep(rep(c(0.8, 1.4), each=n),3),
                           "vred" =  rbind(vzorec06_08, vzorec06_14, 
                                      vzorec1_08, vzorec1_14,
                                      vzorec12_08, vzorec12_14))

# preimenujeva zadnje tri stolpce
colnames(vzorec_skupaj)[3:5] = c("x1", "x2", "y")

# modeli (brez transformacije)
model06_08 = lm(y ~ x1 + x2, vzorec06_08)
model06_14 = lm(y ~ x1 + x2, vzorec06_14)
model1_08 = lm(y ~ x1 + x2, vzorec1_08)
model1_14 = lm(y ~ x1 + x2, vzorec1_14)
model12_08 = lm(y ~ x1 + x2, vzorec12_08)
model12_14 = lm(y ~ x1 + x2, vzorec12_14)

# primer grafa ostankov
par(mfrow=c(1,2))
plot(model06_08, which = c(1,3))

# ==================== primerjava IZ (brez transformacije) =================

# klasicna analiza
par(mfrow=c(2,2))
plot(model06_08)
par(mfrow=c(1,1))
summary(model06_08)
confint(model06_08)
orgEst = summary(model06_08)$coef

# __________ bootstrap __________
m = 1000

alpha_v = c(0.6, 0.6, 1, 1, 1.2, 1.2)
gamma_v = c(0.8, 1.4, 0.8, 1.4, 0.8, 1.4)

res = matrix(NA, nrow = m*length(alpha_v), ncol = 5)
colnames(res) = c("alpha", "gamma", "int", "x1", "x2")
set.seed(2024)


ind_res=1 # indeks za pisanje v res
for(kateri in 1:length(alpha_v)){
  for(i in 1:m){
    # n velikost vzorcev x1 in x2
    ind = sample(n, replace = TRUE)
    # izbereva podatke za trenutni alpha in gamma
    podatki = vzorec_skupaj %>% 
      filter(alpha == alpha_v[kateri] & gamma == gamma_v[kateri]) %>%
      select(c("x1", "x2", "y")) # izlociva le stolpce x1, x2, y
    
    iFitLm = lm(y ~ x1 + x2, data=podatki[ind,])
    iEst = summary(iFitLm)$coef
    
    res[ind_res, 1:2] = c(alpha_v[kateri], gamma_v[kateri])
    res[ind_res, 3:5] = iEst[,1]
    
    ind_res = ind_res + 1
  }
}

# primer izracuna IZ s quantile
res_izbrani = data.frame(res) %>% 
  filter(alpha == 0.6 & gamma == 0.8) %>%
  select(c("int", "x1", "x2"))

alpha = 0.05
percCI = t(apply(res_izbrani[,1:3], 2, quantile, probs = c(alpha/2, 1-alpha/2)))

