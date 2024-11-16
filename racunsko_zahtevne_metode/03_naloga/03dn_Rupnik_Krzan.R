set.seed(2024)

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

# generiranje podatkov
alpha.v = c(0.6, 1, 1.2)
gamma.v = c(0.8, 1.4)
n.v = c(20, 200, 500)

podatki = data.frame(alpha = numeric(), gamma = numeric(), velikost.vzorca = numeric(),
                     x1 = numeric(), x2 = numeric(), y = numeric())

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


# ====================== primerjava IZ (brez transformacije) ===================

# ------------------------------- klasicni test --------------------------------
podatki = readRDS("podatki.RDS")
intervali.zaupanja.org = data.frame(alpha = numeric(), gamma = numeric(), velikost.vzorca = numeric(),
                                    int.coef = numeric(), x1.coef = numeric(), x2.coef = numeric(),
                                    int.lower = numeric(), int.upper = numeric(),
                                    x1.lower = numeric(), x1.upper = numeric(), 
                                    x2.lower = numeric(), x2.upper = numeric())

for (alpha in unique(alpha.v)){
  for (gamma in unique(gamma.v)){
    for (n in unique(n.v)){
      # podatki
      #data = generiranje_podatkov(beta0=100, beta1=3, beta2=2, alpha=alpha, gamma=gamma, n=n)
      data = podatki %>% filter(alpha == alpha & gamma == gamma & velikost.vzorca == n ) %>%
        select(c("x1", "x2", "y"))
      
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
                                                int.coef = int, x1.coef = summary(model)$coef[1,1], x2.coef = x2,
                                                int.lower = int.iz[1], int.upper = int.iz[2],
                                                x1.lower = x1.iz[1], x1.upper = x1.iz[2],
                                                x2.lower = x2.iz[1], x2.upper = x2.iz[2]))
    }
  }
}

saveRDS(object = intervali.zaupanja.org, file="intervali.zaupanja.org.RDS")

# --------------------------------- bootstrap ----------------------------------
# bootstrap vzorci
m = 1000

alpha.v = c(0.6, 1, 1.2)
gamma.v = c(0.8, 1.4)
n.v = c(20, 200, 500)

settings = expand.grid(i=1:length(alpha.v), 
                       alpha = rev(alpha.v), 
                       gamma = rev(gamma.v), 
                       n=rev(n.v))


res = matrix(NA, nrow = nrow(settings) * m, ncol = 6)
colnames(res) = c("alpha", "gamma", "velikost.vzorca", "int", "x1", "x2")
set.seed(2024)

ind_res = 1 # indeks za rezultate
for(i in 1:nrow(settings)){
  #i = 1
  for(j in 1:m){ #bootstrap
    #j = 1
    ind = sample(settings$n[i], replace = T)
    data = podatki %>% filter(alpha == settings$alpha[i] & gamma == settings$gamma[i] & velikost.vzorca == settings$n[i]) %>%
      select(c("x1", "x2", "y"))

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

res = readRDS("bootstrap.RDS")
rezultati = data.frame(res)

# za shranjevanje intervalov zaupanja
intervali.zaupanja  = data.frame(alpha = numeric(), gamma = numeric(), velikost.vzorca = numeric(),
                                 int.lower = numeric(), int.upper = numeric(),
                                 x1.lower = numeric(), x1.upper = numeric(), 
                                 x2.lower = numeric(), x2.upper = numeric())
alpha.iz = 0.05

# izračun intervalov zaupanja za vsako kombinacijo parametrov
for (alpha in unique(rezultati$alpha)){
  for (gamma in unique(rezultati$gamma)){
    for (n in unique(rezultati$velikost.vzorca)){
      # filtriraj podatke za trenutno kombinacijo parametrov
      subset.res  = data.frame(res) %>% filter(alpha == alpha & gamma == gamma & velikost.vzorca == n) %>% 
        select(c("int", "x1", "x2"))
      
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
  
# ====================== primerjava IZ (s transformacijo) ===================

# ------------------------------- klasicni test --------------------------------
alpha.v = c(0.6, 1, 1.2)
gamma.v = c(0.8, 1.4)
n.v = c(20, 200, 500)

intervali.zaupanja.org = data.frame(alpha = numeric(), gamma = numeric(), velikost.vzorca = numeric(),
                                    int.coef = numeric(), x1.coef = numeric(), x2.coef = numeric(),
                                    int.lower = numeric(), int.upper = numeric(),
                                    x1.lower = numeric(), x1.upper = numeric(), 
                                    x2.lower = numeric(), x2.upper = numeric())

for (alpha in unique(alpha.v)){
  for (gamma in unique(gamma.v)){
    for (n in unique(n.v)){
      # podatki
      data = podatki %>% filter(alpha == alpha & gamma == gamma & velikost.vzorca == n ) %>%
        select(c("x1", "x2", "y"))
      
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
                                                int.coef = int, x1.coef = summary(model)$coef[1,1], x2.coef = x2,
                                                int.lower = int.iz[1], int.upper = int.iz[2],
                                                x1.lower = x1.iz[1], x1.upper = x1.iz[2],
                                                x2.lower = x2.iz[1], x2.upper = x2.iz[2]))
    }
  }
}

saveRDS(object = intervali.zaupanja.org, file="intervali.zaupanja.org.transf.RDS")

# --------------------------------- bootstrap ----------------------------------
# bootstrap vzorci
m = 1000

alpha.v = c(0.6, 1, 1.2)
gamma.v = c(0.8, 1.4)
n.v = c(20, 200, 500)

settings = expand.grid(i=1:length(alpha.v), 
                       alpha = rev(alpha.v), 
                       gamma = rev(gamma.v), 
                       n=rev(n.v))


res = matrix(NA, nrow = nrow(settings) * m, ncol = 6)
colnames(res) = c("alpha", "gamma", "velikost.vzorca", "int", "x1", "x2")
set.seed(2024)

ind_res = 1 # indeks za rezultate
for(i in 1:nrow(settings)){
  #i = 1
  for(j in 1:m){ #bootstrap
    #j = 1
    ind = sample(settings$n[i], replace = T)
    data = podatki %>% filter(alpha == settings$alpha[i] & gamma == settings$gamma[i] & velikost.vzorca == settings$n[i]) %>%
      select(c("x1", "x2", "y"))
    
    # lm model
    iFitLm = lm(log(y) ~ x1 + x2, data=podatki[ind,])
    iEst = summary(iFitLm)$coef
    
    # shranimo rezultate
    res[ind_res, 1:3] = c(settings$alpha[i], settings$gamma[i], settings$n[i])
    res[ind_res, 4:6] = iEst[,1]
    
    ind_res = ind_res + 1
  }
}
# shranjevanje
saveRDS(object = res, file="bootstrap.transformirano.RDS")

res = readRDS("bootstrap.transformirano.RDS")
rezultati = data.frame(res)

# za shranjevanje intervalov zaupanja
intervali.zaupanja  = data.frame(alpha = numeric(), gamma = numeric(), velikost.vzorca = numeric(),
                                 int.lower = numeric(), int.upper = numeric(),
                                 x1.lower = numeric(), x1.upper = numeric(), 
                                 x2.lower = numeric(), x2.upper = numeric())
alpha.iz = 0.05

# izračun intervalov zaupanja za vsako kombinacijo parametrov
for (alpha in unique(rezultati$alpha)){
  for (gamma in unique(rezultati$gamma)){
    for (n in unique(rezultati$velikost.vzorca)){
      # filtriraj podatke za trenutno kombinacijo parametrov
      subset.res  = data.frame(res) %>% filter(alpha == alpha & gamma == gamma & velikost.vzorca == n) %>% 
        select(c("int", "x1", "x2"))
      
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






