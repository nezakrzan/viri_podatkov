set.seed(2024)

library(tidyr)
library(dplyr)

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

podatki = readRDS("podatki.RDS")
podatki = data.frame(podatki)

# ====================== primerjava IZ (brez transformacije) ===================

# ------------------------------- klasicni test --------------------------------
intervali.zaupanja.org = data.frame(alpha = numeric(), gamma = numeric(), velikost.vzorca = numeric(),
                                    int.coef = numeric(), x1.coef = numeric(), x2.coef = numeric(),
                                    int.lower = numeric(), int.upper = numeric(),
                                    x1.lower = numeric(), x1.upper = numeric(), 
                                    x2.lower = numeric(), x2.upper = numeric())

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

intervali.zaupanja.org = readRDS("intervali.zaupanja.org.RDS")
intervali.zaupanja.org = data.frame(intervali.zaupanja.org)

# --------------------------------- bootstrap ----------------------------------
# bootstrap vzorci
m = 1000

alpha.v = c(0.6, 1, 1.2)
gamma.v = c(0.8, 1.4)
n.v = c(20, 200, 500)

settings = expand.grid(alpha = rev(alpha.v),
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
    # data = podatki %>% filter(alpha == settings$alpha[i] & gamma == settings$gamma[i] & velikost.vzorca == settings$n[i]) %>%
    #   select(c("x1", "x2", "y"))
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

saveRDS(object = intervali.zaupanja, file="intervali.zaupanja.RDS") 
 
intervali.zaupanja = readRDS("intervali.zaupanja.RDS")
intervali.zaupanja = data.frame(intervali.zaupanja)

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
      # data = podatki %>% filter(alpha == alpha & gamma == gamma & velikost.vzorca == n ) %>%
      #   select(c("x1", "x2", "y"))
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

intervali.zaupanja.org = readRDS("intervali.zaupanja.org.RDS")
intervali.zaupanja.org = data.frame(intervali.zaupanja.org)

# --------------------------------- bootstrap ----------------------------------
m = 1000

alpha.v = c(0.6, 1, 1.2)
gamma.v = c(0.8, 1.4)
n.v = c(20, 200, 500)

settings = expand.grid(alpha = rev(alpha.v), 
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

intervali.zaupanja = readRDS("intervali.zaupanja.transf.RDS")
intervali.zaupanja = data.frame(intervali.zaupanja)

# ================================== pokritje =================================
set.seed(2024)
alpha.v = c(0.6, 1, 1.2)
gamma.v = c(0.8, 1.4)
n.v = c(20, 200, 500)
m = 1000

# -------------------------- brez transformacije -------------------------------
df = readRDS("podatki.RDS")
df = data.frame(df)

int.org = 100
x1.org = 3
x2.org = 2

alpha.iz = 0.05
pokritje = data.frame(alpha = numeric(), gamma = numeric(), velikost.vzorca = numeric(),
                      int.in = logical(), x1.in = logical(), x2.in = logical())

for(i in 1:1000){ 
  res = matrix(NA, nrow = m, ncol = 6)
  for (alpha in unique(alpha.v)){
    # alpha = 0.6
    for (gamma in unique(gamma.v)){
      # gamma = 0.8
      for (n in unique(n.v)){
        # n = 20
        # izberem podatke
        subset.res = df[c(df$alpha==alpha & df$gamma==gamma & df$velikost.vzorca==n),]
        
        for(j in 1:m){ #bootstrap
          #j = 1
          ind = sample(n, replace = T)
          
          # lm model
          iFitLm = lm(y ~ x1 + x2, data=subset.res[ind,])
          iEst = summary(iFitLm)$coef
          
          # shranimo rezultate
          res[j, 1:3] = c(alpha, gamma, n)
          res[j, 4:6] = iEst[,1]
        }
        
        res = data.frame(res)
        
        # izračun intervalov zaupanja
        int.iz  = quantile(res$X4, probs = c(alpha.iz/2, 1-alpha.iz/2))
        x1.iz  = quantile(res$X5, probs = c(alpha.iz/2, 1-alpha.iz/2))
        x2.iz  = quantile(res$X6, probs = c(alpha.iz/2, 1-alpha.iz/2))
        
        # ali so pravi parametri v IZ
        int.in = (int.iz[1] <= int.org) & (int.org <= int.iz[2])
        x1.in = (x1.iz[1] <= x1.org) & (x1.org <= int.iz[2])
        x2.in = (x2.iz[1] <= x2.org) & (x2.org <= int.iz[2])
        
        pokritje = rbind(pokritje, 
                         data.frame(alpha = alpha, gamma = gamma, velikost.vzorca = n,
                                    int.in = int.in, x1.in = x1.in, x2.in = x2.in))
      }
    }
  }
  if(i%%10==0) cat("Iteration ", i, "/", 1000, "complete! \n")
}

saveRDS(pokritje, file="pokritje.RDS") 


pokritje = readRDS("pokritje.RDS")
df = data.frame(pokritje)

pokritje.odst = data.frame(alpha = numeric(), gamma = numeric(), velikost.vzorca = numeric(), 
                           int.pokritje = numeric(), x1.pokritje = numeric(), x2.pokritje = numeric())

for (alpha in unique(pokritje$alpha)){
  for (gamma in unique(pokritje$gamma)){
    for (n in unique(pokritje$velikost.vzorca)){
      # izberemo podatke
      subset.res = df[c(df$alpha==alpha & df$gamma==gamma & df$velikost.vzorca==n),]
      
      # izračunamo povprečje
      int.pokritje = round(mean(subset.res$int.in) * 100, 2)
      x1.pokritje = mean(subset.res$x1.in)
      x2.pokritje = mean(subset.res$x2.in)
      
      pokritje.odst = rbind(pokritje.odst, 
                            data.frame(alpha = alpha, gamma = gamma, velikost.vzorca = n, 
                                       int.pokritje = int.pokritje, x1.pokritje = x1.pokritje, x2.pokritje = x2.pokritje))
    }
  }
}

# -------------------------- s transformacijo -------------------------------
df = readRDS("podatki.RDS")
df = data.frame(df)

int.org = 100
x1.org = 3
x2.org = 2

alpha.iz = 0.05
pokritje = data.frame(alpha = numeric(), gamma = numeric(), velikost.vzorca = numeric(),
                      int.in = logical(), x1.in = logical(), x2.in = logical())

for(i in 1:1000){ 
  res = matrix(NA, nrow = m, ncol = 6)
  for (alpha in unique(alpha.v)){
    # alpha = 0.6
    for (gamma in unique(gamma.v)){
      # gamma = 0.8
      for (n in unique(n.v)){
        # n = 20
        # izberem podatke
        subset.res = df[c(df$alpha==alpha & df$gamma==gamma & df$velikost.vzorca==n),]
        
        for(j in 1:m){ #bootstrap
          #j = 1
          ind = sample(n, replace = T)
          
          # lm model
          iFitLm = lm(y ~ x1 + x2, data=subset.res[ind,])
          iEst = summary(iFitLm)$coef
          
          # shranimo rezultate
          res[j, 1:3] = c(alpha, gamma, n)
          res[j, 4:6] = iEst[,1]
        }
        
        res = data.frame(res)
        
        # izračun intervalov zaupanja
        int.iz  = quantile(res$X4, probs = c(alpha.iz/2, 1-alpha.iz/2))
        x1.iz  = quantile(res$X5, probs = c(alpha.iz/2, 1-alpha.iz/2))
        x2.iz  = quantile(res$X6, probs = c(alpha.iz/2, 1-alpha.iz/2))
        
        # ali so pravi parametri v IZ
        int.in = (int.iz[1] <= int.org) & (int.org <= int.iz[2])
        x1.in = (x1.iz[1] <= x1.org) & (x1.org <= int.iz[2])
        x2.in = (x2.iz[1] <= x2.org) & (x2.org <= int.iz[2])
        
        pokritje = rbind(pokritje, 
                         data.frame(alpha = alpha, gamma = gamma, velikost.vzorca = n,
                                    int.in = int.in, x1.in = x1.in, x2.in = x2.in))
      }
    }
  }
  if(i%%10==0) cat("Iteration ", i, "/", 1000, "complete! \n")
}

saveRDS(pokritje, file="pokritje.RDS") 


pokritje = readRDS("pokritje.RDS")
df = data.frame(pokritje)

pokritje.odst = data.frame(alpha = numeric(), gamma = numeric(), velikost.vzorca = numeric(), 
                           int.pokritje = numeric(), x1.pokritje = numeric(), x2.pokritje = numeric())

for (alpha in unique(pokritje$alpha)){
  for (gamma in unique(pokritje$gamma)){
    for (n in unique(pokritje$velikost.vzorca)){
      # izberemo podatke
      subset.res = df[c(df$alpha==alpha & df$gamma==gamma & df$velikost.vzorca==n),]
      
      # izračunamo povprečje
      int.pokritje = round(mean(subset.res$int.in) * 100, 2)
      x1.pokritje = mean(subset.res$x1.in)
      x2.pokritje = mean(subset.res$x2.in)
      
      pokritje.odst = rbind(pokritje.odst, 
                            data.frame(alpha = alpha, gamma = gamma, velikost.vzorca = n, 
                                       int.pokritje = int.pokritje, x1.pokritje = x1.pokritje, x2.pokritje = x2.pokritje))
    }
  }
}
