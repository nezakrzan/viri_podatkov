set.seed(2024)

library(tidyr)
library(dplyr)
library(patchwork)
library(glue)
library(ggplot2)
library(car)


# =============================== faktorji ====================================
alpha.v = c(0.6, 1, 1.2)
gamma.v = c(0.8, 1.4)
n.v = c(20, 200, 500)

# bootstrap vzorci
m = 1000

useOld = TRUE # ne uporabljal starega rezultata

# ============================ simulacija podatkov ============================
# funkcija za generiranje podatkov
generiranje_podatkov  = function(beta0, beta1, beta2, alpha, gamma, n){
  # generiranje vrednosti za x1 in x2
  x1  = runif(n, 20, 60)
  x2  = runif(n, 2, 10)
  
  # generiranje napake
  epsilon  = rnorm(n, 0,x1*alpha) # dodava heteroskedasticnost
  
  # izračun napovedne spremenljivke za model (parametri = vhodni argumenti) 
  y  = beta0 + beta1 * x1 + beta2 * x2^gamma + epsilon # z ^gamma dodava nelinearni del
  
  # podatke spravimo v podatkovni okvir in vrnemo kot rezultat funkcije
  data.frame(x1 = x1, x2 = x2, y = y) 
}

# ============================ generiranje podatkov ============================
podatki = data.frame(alpha = numeric(), gamma = numeric(), velikost.vzorca = numeric(),
                     x1 = numeric(), x2 = numeric(), y = numeric())

if(useOld&&file.exists("podatki.RDS")){
  res = readRDS("podatki.RDS")
}else{
  for (alpha in unique(alpha.v)){
    for (gamma in unique(gamma.v)){
      for (n in unique(n.v)){
        # generiram podatke
        data = generiranje_podatkov(beta0=100, beta1=3, beta2=2, alpha=alpha, gamma=gamma, n=n)
      
        # shranim podatke
        podatki = rbind(podatki, data.frame(alpha = c(rep(alpha, n)),
                                            gamma=c(rep(gamma, n)), 
                                            velikost.vzorca = c(rep(n, n)),
                                            x1 = data$x1,
                                            x2 = data$x2,
                                            y = data$y))
      }
    }
  }
  saveRDS(object = podatki, file="podatki.RDS")
}

podatki = readRDS("podatki.RDS")
podatki = data.frame(podatki)

# -------------------------- graficni prikazi ----------------------------------
# izracun vrednosti vzorca
n = 500 # velikost vzorca
vzorec06_08 = generiranje_podatkov(100, 3, 2, 0.6, 0.8, n)
vzorec06_14 = generiranje_podatkov(100, 3, 2, 0.6, 1.4, n)
vzorec1_08 = generiranje_podatkov(100, 3, 2, 1, 0.8, n)
vzorec1_14 = generiranje_podatkov(100, 3, 2, 1, 1.4, n)
vzorec12_08 = generiranje_podatkov(100, 3, 2, 1.2, 0.8, n)
vzorec12_14 = generiranje_podatkov(100, 3, 2, 1.2, 1.4, n)

# modeli (brez transformacije)
model06_08 = lm(y ~ x1 + x2, vzorec06_08)
model06_14 = lm(y ~ x1 + x2, vzorec06_14)
model1_08 = lm(y ~ x1 + x2, vzorec1_08)
model1_14 = lm(y ~ x1 + x2, vzorec1_14)
model12_08 = lm(y ~ x1 + x2, vzorec12_08)
model12_14 = lm(y ~ x1 + x2, vzorec12_14)

# modeli (s transformacijo)
model06_08_log = lm(log(y) ~ x1 + x2, vzorec06_08)
model06_14_log = lm(log(y) ~ x1 + x2, vzorec06_14)

# Grafi ostankov pri paramatrih alpha=0,6 in gamma=0,8.
# par(mfrow=c(1,2))
# plot(model06_08, which = c(1,3))

# Grafi ostankov pri paramatrih alpha=1,2 in gamma=1,4.
# par(mfrow=c(1,2))
# plot(model12_14, which = c(1,3))

# Grafi ostankov transformiranih podatkov pri paramatrih alpha=0,6 in gamma=0,8.
# modeli (brez transformacije)
model06_08_log = lm(log(y) ~ x1 + x2, vzorec06_08)
model06_14_log = lm(log(y) ~ x1 + x2, vzorec06_14)
# par(mfrow=c(1,2))
# plot(model06_08_log, which = c(1,3))

# Grafi ostankov transformiranih podatkov pri paramatrih alpha=0,6 in gamma=1,4.
# par(mfrow=c(1,2))
# plot(model06_14_log, which = c(1,3))

# ====================== primerjava IZ (brez transformacije) ===================

# ------------------------------- klasicni test --------------------------------
intervali.zaupanja.org = data.frame(alpha = numeric(), gamma = numeric(), velikost.vzorca = numeric(),
                                    int.coef = numeric(), x1.coef = numeric(), x2.coef = numeric(),
                                    int.lower = numeric(), int.upper = numeric(),
                                    x1.lower = numeric(), x1.upper = numeric(), 
                                    x2.lower = numeric(), x2.upper = numeric())

if(useOld&&file.exists("intervali.zaupanja.org.RDS")){
  res = readRDS("intervali.zaupanja.org.RDS")
}else{
  for (alpha in unique(alpha.v)){
    for (gamma in unique(gamma.v)){
      for (n in unique(n.v)){
        # podatki
        data = podatki[c(podatki$alpha==alpha & podatki$gamma==gamma & podatki$velikost.vzorca==n),
                       c("x1", "x2", "y")]
        
        # model
        model = lm(y ~ x1 + x2, data)
        
        # koeficienti
        int = summary(model)$coef[1,1]
        x1 = summary(model)$coef[2,1]
        x2 = summary(model)$coef[3,1]
  
        
        # intervali zaupanja
        int.iz = confint(model)[1,]
        x1.iz = confint(model)[2,]
        x2.iz = confint(model)[3,]
        
        # shranjevanje
        intervali.zaupanja.org = rbind(intervali.zaupanja.org, 
                                       data.frame(alpha = alpha, gamma = gamma, velikost.vzorca = n,
                                                  int.coef = int, x1.coef = x1, x2.coef = x2,
                                                  int.lower = int.iz[1], int.upper = int.iz[2],
                                                  x1.lower = x1.iz[1], x1.upper = x1.iz[2],
                                                  x2.lower = x2.iz[1], x2.upper = x2.iz[2]))
      }
    }
  }
  saveRDS(object = intervali.zaupanja.org, file="intervali.zaupanja.org.RDS")
}

intervali.zaupanja.org = readRDS("intervali.zaupanja.org.RDS")
intervali.zaupanja.org = data.frame(intervali.zaupanja.org)

# --------------------------------- bootstrap ----------------------------------
settings = expand.grid(alpha = rev(alpha.v),
                       gamma = rev(gamma.v),
                       n=rev(n.v))


res = matrix(NA, nrow = nrow(settings) * m, ncol = 6)
colnames(res) = c("alpha", "gamma", "velikost.vzorca", "int", "x1", "x2")

if(useOld&&file.exists("bootstrap.RDS")){
  res = readRDS("bootstrap.RDS")
}else{
  ind_res = 1 # indeks za rezultate
  for(i in 1:nrow(settings)){
    #i = 1
    for(j in 1:m){ #bootstrap
      ind = sample(settings$n[i], replace = T)
      data = podatki[c(podatki$alpha==settings$alpha[i] & podatki$gamma==settings$gamma[i] & podatki$velikost.vzorca==settings$n[i]),
                     c("x1", "x2", "y")]
  
      # lm model
      iFitLm = lm(y ~ x1 + x2, data=data[ind,])
      iEst = summary(iFitLm)$coef
      
      # shranimo rezultate
      res[ind_res, 1:3] = c(settings$alpha[i], settings$gamma[i], settings$n[i])
      res[ind_res, 4:6] = iEst[,1]
      
      ind_res = ind_res + 1
    }
  }
  # shranjevanje
  saveRDS(object = res, file="bootstrap.RDS")
}

res = readRDS("bootstrap.RDS")
rezultati = data.frame(res)

# za shranjevanje intervalov zaupanja
intervali.zaupanja  = data.frame(alpha = numeric(), gamma = numeric(), velikost.vzorca = numeric(),
                                 int.lower = numeric(), int.upper = numeric(),
                                 x1.lower = numeric(), x1.upper = numeric(), 
                                 x2.lower = numeric(), x2.upper = numeric())
alpha.iz = 0.05
df = data.frame(res)

# izračun intervalov zaupanja za vsako kombinacijo parametrov
if(useOld&&file.exists("intervali.zaupanja.RDS")){
  res = readRDS("intervali.zaupanja.RDS")
}else{
  for (alpha in unique(rezultati$alpha)){
    for (gamma in unique(rezultati$gamma)){
      for (n in unique(rezultati$velikost.vzorca)){
        # filtriraj podatke za trenutno kombinacijo parametrov
        subset.res = df[c(df$alpha==alpha & df$gamma==gamma & df$velikost.vzorca==n),]
        
        # izračun intervalov zaupanja
        int.iz  = quantile(subset.res$int, probs = c(alpha.iz/2, 1-alpha.iz/2))
        x1.iz  = quantile(subset.res$x1, probs = c(alpha.iz/2, 1-alpha.iz/2))
        x2.iz  = quantile(subset.res$x2, probs = c(alpha.iz/2, 1-alpha.iz/2))
        
        # shranjevanje 
        intervali.zaupanja = rbind(intervali.zaupanja, 
                                   data.frame(alpha = alpha, gamma = gamma, velikost.vzorca = n,
                                              int.lower = int.iz[1], int.upper = int.iz[2],
                                              x1.lower = x1.iz[1], x1.upper = x1.iz[2],
                                              x2.lower = x2.iz[1], x2.upper = x2.iz[2]))
      }
    }
  }
  
  saveRDS(object = intervali.zaupanja, file="intervali.zaupanja.RDS") 
}

intervali.zaupanja = readRDS("intervali.zaupanja.RDS")
intervali.zaupanja = data.frame(intervali.zaupanja)

# -------------------------- graficni prikazi ----------------------------------
# priprava podatkov za prikaze
IZ_org = readRDS("intervali.zaupanja.org.RDS") %>% 
  select(-c("int.coef", "x1.coef", "x2.coef"))
IZ_bootstrap = readRDS("intervali.zaupanja.RDS")
IZ_skupaj = rbind(IZ_org, IZ_bootstrap)
IZ_skupaj = cbind(IZ_skupaj, rep(c("klasična analiza(linearna regresija)", "bootstrap"), each=nrow(IZ_org)))
colnames(IZ_skupaj)[10] = "metoda"
# povprečja koeficientov
IZ_org_koef = readRDS("intervali.zaupanja.org.RDS")
IZ_bootstrap_koef = readRDS("bootstrap.RDS")
IZ_bootstrap_koef = data.frame(IZ_bootstrap_koef)
koef.mean.org = IZ_org_koef %>% group_by(velikost.vzorca, alpha, gamma) %>%
  summarise(mean(int.coef), mean(x1.coef), mean(x2.coef))
koef.mean.org = cbind(koef.mean.org, rep(c("klasična analiza(linearna regresija)"), nrow(koef.mean.org)))
colnames(koef.mean.org) = c("velikost.vzorca", "alpha", "gamma", "mean.int", "mean.x1", "mean.x2", "metoda")
koef.mean.boot = IZ_bootstrap_koef %>% group_by(velikost.vzorca, alpha, gamma) %>%
  summarise(mean(int), mean(x1), mean(x2))
koef.mean.boot = cbind(koef.mean.boot, rep(c("bootstrap"), nrow(koef.mean.boot)))
colnames(koef.mean.boot) = c("velikost.vzorca", "alpha", "gamma", "mean.int", "mean.x1", "mean.x2", "metoda")
IZ_skupaj_coef = rbind(koef.mean.org, koef.mean.boot)
IZ_vse = full_join(IZ_skupaj, IZ_skupaj_coef, by = c("velikost.vzorca", "alpha", "gamma", "metoda"))

# Grafi intervalov zaupanja za prosti koeficient(Intercept) - podatki brez transformacije.
pod_int = IZ_vse %>% select(c("alpha", "gamma", "velikost.vzorca", "int.lower", "int.upper", "mean.int", "metoda"))
pod_int$velikost.vzorca = factor(pod_int$velikost.vzorca)
ggplot(pod_int, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_errorbar(aes(ymin = int.lower, ymax = int.upper), width=0.5, position = position_dodge2(reverse = TRUE, 0.3)) + 
  geom_point(aes(y = mean.int), position = position_dodge2(reverse = TRUE, 0.5), shape = 16, size = 1) +
  facet_grid(glue('alpha*" = {alpha}"') ~ glue('gamma*" = {gamma}"'), 
             labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ") +
  theme(legend.position = "bottom") +
  theme_minimal()

# Grafi intervalov zaupanja za koeficient pri x1(beta1) - podatki brez transformacije.
pod_x1 = IZ_vse %>% select(c("alpha", "gamma", "velikost.vzorca", "x1.lower", "x1.upper", "mean.x1","metoda"))
pod_x1$velikost.vzorca = factor(pod_x1$velikost.vzorca)
ggplot(pod_x1, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_errorbar(aes(ymin = x1.lower, ymax = x1.upper), width=0.5, position = position_dodge2(reverse = TRUE, 0.3)) + 
  geom_point(aes(y = mean.x1), position = position_dodge2(reverse = TRUE, 0.5), shape = 16, size = 1) +
  facet_grid(glue('alpha*" = {alpha}"') ~ glue('gamma*" = {gamma}"'), 
             labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ") +
  theme(legend.position = "bottom") +
  theme_minimal()

# Grafi intervalov zaupanja za koeficient pri x2(beta2) - podatki brez transformacije.
pod_x2 = IZ_vse %>% select(c("alpha", "gamma", "velikost.vzorca", "x2.lower", "x2.upper", "mean.x2","metoda"))
pod_x2$velikost.vzorca = factor(pod_x1$velikost.vzorca)
ggplot(pod_x2, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_errorbar(aes(ymin = x2.lower, ymax = x2.upper), width=0.5, position = position_dodge2(reverse = TRUE, 0.3)) + 
  geom_point(aes(y = mean.x2), position = position_dodge2(reverse = TRUE, 0.5), shape = 16, size = 1) +
  facet_grid(glue('alpha*" = {alpha}"') ~ glue('gamma*" = {gamma}"'), 
             labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ") +
  theme(legend.position = "bottom") +
  theme_minimal()

# ====================== primerjava IZ (s transformacijo) ===================

# ------------------------------- klasicni test --------------------------------
intervali.zaupanja.org = data.frame(alpha = numeric(), gamma = numeric(), velikost.vzorca = numeric(),
                                    int.coef = numeric(), x1.coef = numeric(), x2.coef = numeric(),
                                    int.lower = numeric(), int.upper = numeric(),
                                    x1.lower = numeric(), x1.upper = numeric(), 
                                    x2.lower = numeric(), x2.upper = numeric())

if(useOld&&file.exists("intervali.zaupanja.org.transf.RDS")){
  res = readRDS("intervali.zaupanja.org.transf.RDS")
}else{
  for (alpha in unique(alpha.v)){
    for (gamma in unique(gamma.v)){
      for (n in unique(n.v)){
        # podatki
        data = podatki[c(podatki$alpha==alpha & podatki$gamma==gamma & podatki$velikost.vzorca==n),
                       c("x1", "x2", "y")]
        
        # model
        model = lm(log(y) ~ x1 + x2, data)
        
        # koeficienti
        int = summary(model)$coef[1,1]
        x1 = summary(model)$coef[2,1]
        x2 = summary(model)$coef[3,1]
        
        # intervali zaupanja
        int.iz = confint(model)[1,]
        x1.iz = confint(model)[2,]
        x2.iz = confint(model)[3,]
        
        # shranjevanje
        intervali.zaupanja.org = rbind(intervali.zaupanja.org, 
                                       data.frame(alpha = alpha, gamma = gamma, velikost.vzorca = n,
                                                  int.coef = int, x1.coef = x1, x2.coef = x2,
                                                  int.lower = int.iz[1], int.upper = int.iz[2],
                                                  x1.lower = x1.iz[1], x1.upper = x1.iz[2],
                                                  x2.lower = x2.iz[1], x2.upper = x2.iz[2]))
      }
    }
  }
  saveRDS(object = intervali.zaupanja.org, file="intervali.zaupanja.org.transf.RDS")
}

intervali.zaupanja.org = readRDS("intervali.zaupanja.org.RDS")
intervali.zaupanja.org = data.frame(intervali.zaupanja.org)

# --------------------------------- bootstrap ----------------------------------
settings = expand.grid(alpha = rev(alpha.v), 
                       gamma = rev(gamma.v), 
                       n=rev(n.v))


res = matrix(NA, nrow = nrow(settings) * m, ncol = 6)
colnames(res) = c("alpha", "gamma", "velikost.vzorca", "int", "x1", "x2")

if(useOld&&file.exists("bootstrap.transformirano.RDS")){
  res = readRDS("bootstrap.transformirano.RDS")
}else{
  ind_res = 1 # indeks za rezultate
  for(i in 1:nrow(settings)){
    #i = 1
    for(j in 1:m){ #bootstrap
      #j = 1
      ind = sample(settings$n[i], replace = T)
      # data = podatki %>% filter(alpha == settings$alpha[i] & gamma == settings$gamma[i] & velikost.vzorca == settings$n[i]) %>%
      #   select(c("x1", "x2", "y"))
      data = podatki[c(podatki$alpha==settings$alpha[i] & podatki$gamma==settings$gamma[i] & podatki$velikost.vzorca==settings$n[i]),
                     c("x1", "x2", "y")]
      
      # lm model
      iFitLm = lm(log(y) ~ x1 + x2, data=data[ind,])
      iEst = summary(iFitLm)$coef
      
      # shranimo rezultate
      res[ind_res, 1:3] = c(settings$alpha[i], settings$gamma[i], settings$n[i])
      res[ind_res, 4:6] = iEst[,1]
      
      ind_res = ind_res + 1
    }
  }
  # shranjevanje
  saveRDS(object = res, file="bootstrap.transformirano.RDS")
}

res = readRDS("bootstrap.transformirano.RDS")
rezultati = data.frame(res)

# za shranjevanje intervalov zaupanja
intervali.zaupanja  = data.frame(alpha = numeric(), gamma = numeric(), velikost.vzorca = numeric(),
                                 int.lower = numeric(), int.upper = numeric(),
                                 x1.lower = numeric(), x1.upper = numeric(), 
                                 x2.lower = numeric(), x2.upper = numeric())
alpha.iz = 0.05
df = data.frame(res)

# izračun intervalov zaupanja za vsako kombinacijo parametrov
if(useOld&&file.exists("intervali.zaupanja.transf.RDS")){
  res = readRDS("intervali.zaupanja.transf.RDS")
}else{
  for (alpha in unique(rezultati$alpha)){
    for (gamma in unique(rezultati$gamma)){
      for (n in unique(rezultati$velikost.vzorca)){
        # filtriraj podatke za trenutno kombinacijo parametrov
        # subset.res  = data.frame(res) %>% filter(alpha == alpha & gamma == gamma & velikost.vzorca == n) %>% 
        #   select(c("int", "x1", "x2"))
        subset.res = df[c(df$alpha==alpha & df$gamma==gamma & df$velikost.vzorca==n),]
        
        # izračun intervalov zaupanja
        int.iz  = quantile(subset.res$int, probs = c(alpha.iz/2, 1-alpha.iz/2))
        x1.iz  = quantile(subset.res$x1, probs = c(alpha.iz/2, 1-alpha.iz/2))
        x2.iz  = quantile(subset.res$x2, probs = c(alpha.iz/2, 1-alpha.iz/2))
        
        # shranjevanje 
        intervali.zaupanja = rbind(intervali.zaupanja, 
                                   data.frame(alpha = alpha, gamma = gamma, velikost.vzorca = n,
                                              int.lower = int.iz[1], int.upper = int.iz[2],
                                              x1.lower = x1.iz[1], x1.upper = x1.iz[2],
                                              x2.lower = x2.iz[1], x2.upper = x2.iz[2]))
      }
    }
  }
  saveRDS(object = intervali.zaupanja, file="intervali.zaupanja.transf.RDS") 
}

intervali.zaupanja = readRDS("intervali.zaupanja.transf.RDS")
intervali.zaupanja = data.frame(intervali.zaupanja)

# -------------------------- graficni prikazi ----------------------------------
# priprava podatkov za prikaze
IZ_org_transf = readRDS("intervali.zaupanja.org.transf.RDS") %>% 
  select(-c("int.coef", "x1.coef", "x2.coef"))
IZ_bootstrap_transf = readRDS("intervali.zaupanja.transf.RDS")
IZ_skupaj_transf = rbind(IZ_org_transf, IZ_bootstrap_transf)
IZ_skupaj_transf = cbind(IZ_skupaj_transf, rep(c("klasična analiza(linearna regresija)", "bootstrap"), each=nrow(IZ_org)))
colnames(IZ_skupaj_transf)[10] = "metoda"
# povprečja koeficientov
IZ_org_koef_trans = readRDS("intervali.zaupanja.org.transf.RDS")
IZ_bootstrap_koef_trans  = readRDS("bootstrap.transformirano.RDS")
IZ_bootstrap_koef_trans  = data.frame(IZ_bootstrap_koef_trans)
koef.mean.org_trans  = IZ_org_koef_trans  %>% group_by(velikost.vzorca, alpha, gamma) %>%
  summarise(mean(int.coef), mean(x1.coef), mean(x2.coef))
koef.mean.org_trans  = cbind(koef.mean.org_trans, rep(c("klasična analiza(linearna regresija)"), nrow(koef.mean.org_trans)))
colnames(koef.mean.org_trans) = c("velikost.vzorca", "alpha", "gamma", "mean.int", "mean.x1", "mean.x2", "metoda")
koef.mean.boot_trans = IZ_bootstrap_koef_trans  %>% group_by(velikost.vzorca, alpha, gamma) %>%
  summarise(mean(int), mean(x1), mean(x2))
koef.mean.boot_trans = cbind(koef.mean.boot_trans, rep(c("bootstrap"), nrow(koef.mean.boot_trans)))
colnames(koef.mean.boot_trans) = c("velikost.vzorca", "alpha", "gamma", "mean.int", "mean.x1", "mean.x2", "metoda")
IZ_skupaj_coef_trans = rbind(koef.mean.org_trans, koef.mean.boot_trans)
IZ_vse_trans = full_join(IZ_skupaj_transf, IZ_skupaj_coef_trans, by = c("velikost.vzorca", "alpha", "gamma", "metoda"))

# Grafi intervalov zaupanja za prosti koeficient(Intercept) - transformirani podatki.
pod_int = IZ_vse_trans %>% select(c("alpha", "gamma", "velikost.vzorca", "int.lower", "int.upper", "mean.int","metoda"))
pod_int$velikost.vzorca = factor(pod_int$velikost.vzorca)
ggplot(pod_int, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_errorbar(aes(ymin = int.lower, ymax = int.upper), width=0.5, position = position_dodge2(reverse = T, 0.3)) + 
  geom_point(aes(y = mean.int), position = position_dodge2(reverse = TRUE, 0.5), shape = 16, size = 1) +
  facet_grid(glue('alpha*" = {alpha}"') ~ glue('gamma*" = {gamma}"'), 
             labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ") +
  theme(legend.position = "bottom") +
  theme_minimal()

# Grafi intervalov zaupanja za koeficient pri x1(beta1) - transformirani podatki.
pod_x1 = IZ_vse_trans %>% select(c("alpha", "gamma", "velikost.vzorca", "x1.lower", "x1.upper", "mean.x1","metoda"))
pod_x1$velikost.vzorca = factor(pod_x1$velikost.vzorca)
ggplot(pod_x1, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_errorbar(aes(ymin = x1.lower, ymax = x1.upper), width=0.5, position = position_dodge2(reverse = TRUE, 0.3)) + 
  geom_point(aes(y = mean.x1), position = position_dodge2(reverse = TRUE, 0.5), shape = 16, size = 1) +
  facet_grid(glue('alpha*" = {alpha}"') ~ glue('gamma*" = {gamma}"'), 
             labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ") +
  theme(legend.position = "bottom") +
  theme_minimal()

# Grafi intervalov zaupanja za koeficient pri x2(beta2) - transformirani podatki.
pod_x2 = IZ_vse_trans %>% select(c("alpha", "gamma", "velikost.vzorca", "x2.lower", "x2.upper", "mean.x2","metoda"))
pod_x2$velikost.vzorca = factor(pod_x1$velikost.vzorca)
ggplot(pod_x2, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_errorbar(aes(ymin = x2.lower, ymax = x2.upper), width=0.5, position = position_dodge2(reverse = TRUE, 0.3)) + 
  geom_point(aes(y = mean.x2), position = position_dodge2(reverse = TRUE, 0.5), shape = 16, size = 1) +
  facet_grid(glue('alpha*" = {alpha}"') ~ glue('gamma*" = {gamma}"'), 
             labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ") +
  theme(legend.position = "bottom") +
  theme_minimal()

# ============================= Primerjava =====================================
# -------------------------- graficni prikazi ----------------------------------
# priprava podatkov
sirine_intervalov = IZ_vse %>%
  mutate(SI_int = abs(int.upper) - abs(int.lower), SI_x1 = abs(x1.upper) - abs(x1.lower), SI_x2 = abs(x2.upper) - x2.lower) %>%
  select(alpha, gamma, velikost.vzorca, SI_int, SI_x1, SI_x2, metoda)
sirine_intervalov = cbind(sirine_intervalov, podatki = rep(c("klasična analiza(linearna regresija)", "bootstrap"), each=nrow(IZ_org)))
sirine_intervalov$velikost.vzorca = factor(sirine_intervalov$velikost.vzorca)
sirine_intervalov_trans = IZ_vse_trans %>%
  mutate(SI_int = abs(int.upper) - abs(int.lower), SI_x1 = abs(x1.upper) - abs(x1.lower), SI_x2 = abs(x2.upper) - abs(x2.lower)) %>%
  select(alpha, gamma, velikost.vzorca, SI_int, SI_x1, SI_x2, metoda)
sirine_intervalov_trans = cbind(sirine_intervalov_trans, podatki = rep(c("klasična analiza(l.r.) - transformirani podatki", "bootstrap - transformirani podatki"), each=nrow(IZ_org_transf)))
sirine_intervalov_trans$velikost.vzorca = factor(sirine_intervalov_trans$velikost.vzorca)

# Širina intervalov zaupanja za koeficient pri prostem členu(intercept).
g1 = ggplot(sirine_intervalov, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_point(aes(y = SI_int), shape = 16, size = 1.2) +
  geom_line(aes(y = SI_int)) +
  facet_grid(glue('alpha*" = {alpha}"') ~ glue('gamma*" = {gamma}"'), 
             labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ",
       title = "ne transformirani podatki") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 10)) +
  theme_minimal()
g2 = ggplot(sirine_intervalov_trans, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_point(aes(y = SI_int), shape = 16, size = 1.2) +
  geom_line(aes(y = SI_int)) +
  facet_grid(glue('alpha*" = {alpha}"') ~ glue('gamma*" = {gamma}"'), 
             labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ",
       title = "transformirani podatki") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 10)) +
  theme_minimal()
combined_plot = g1 + g2 + plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
combined_plot

# Širina intervalov zaupanja za koeficient pri koeficientu x1(beta1).
g1 = ggplot(sirine_intervalov, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_point(aes(y = SI_x1), shape = 16, size = 1.2) +
  geom_line(aes(y = SI_x1)) +
  facet_grid(glue('alpha*" = {alpha}"') ~ glue('gamma*" = {gamma}"'), 
             labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ",
       title = "ne transformirani podatki") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 10)) +
  theme_minimal()
g2 = ggplot(sirine_intervalov_trans, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_point(aes(y = SI_x1), shape = 16, size = 1.2) +
  geom_line(aes(y = SI_x1)) +
  facet_grid(glue('alpha*" = {alpha}"') ~ glue('gamma*" = {gamma}"'), 
             labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ",
       title = "transformirani podatki") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 10)) +
  theme_minimal()
combined_plot = g1 + g2 + plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
combined_plot

# Širina intervalov zaupanja za koeficient pri koeficientu x2(beta2).
g1 = ggplot(sirine_intervalov, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_point(aes(y = SI_x2), shape = 16, size = 1.2) +
  geom_line(aes(y = SI_x2)) +
  facet_grid(glue('alpha*" = {alpha}"') ~ glue('gamma*" = {gamma}"'), 
             labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ",
       title = "ne transformirani podatki") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 10)) +
  theme_minimal()
g2 = ggplot(sirine_intervalov_trans, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_point(aes(y = SI_x1), shape = 16, size = 1.2) +
  geom_line(aes(y = SI_x1)) +
  facet_grid(glue('alpha*" = {alpha}"') ~ glue('gamma*" = {gamma}"'), 
             labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ",
       title = "transformirani podatki") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 10)) +
  theme_minimal()
combined_plot = g1 + g2 + plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
combined_plot