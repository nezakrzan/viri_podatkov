set.seed(2024)

# =============================== faktorji ====================================
alpha.v = c(0.6, 1, 1.2)
n.v = c(20, 200, 500)
st.ponovitev = 1000

# bootstrap vzorci
m = 1000

useOld = T # ne uporabljal starega rezultata

# ============================ simulacija podatkov ============================
# funkcija za generiranje podatkov
generiranje_podatkov  = function(beta0, beta1, beta2, alpha, n){
  # generiranje vrednosti za x1 in x2
  x1  = runif(n, 20, 60)
  x2  = runif(n, 2, 10)
  
  # generiranje napake
  epsilon  = rnorm(n, 0, x1^alpha) # dodava heteroskedasticnost
  
  # izračun napovedne spremenljivke za model (parametri = vhodni argumenti) 
  y  = beta0 + beta1 * x1 + beta2 * x2 + epsilon
  
  # podatke spravimo v podatkovni okvir in vrnemo kot rezultat funkcije
  data.frame(x1 = x1, x2 = x2, y = y) 
}

# ============================ funkcija za bootstrap ============================
# funkcija za bootstrap
bootstrap = function(m, podatki, alpha, n){
  # shranjevanje rezultatov
  res = matrix(NA, nrow = m, ncol = 5)
  colnames(res) = c("alpha", "velikost.vzorca", "int", "x1", "x2")
  ind_res = 1
  for(j in 1:m){ # bootstrap
    ind = sample(n, replace = T)
    
    # lm model
    iFitLm = lm(y ~ x1 + x2, data=podatki[ind,])
    iEst = summary(iFitLm)$coef
    
    # shranimo rezultate
    res[ind_res, 1:2] = c(alpha, n)
    res[ind_res, 3:5] = iEst[,1]
    
    ind_res = ind_res + 1
  }
  return(res)
}

# ================================ simulacija ==================================
settings = expand.grid(i = 1:st.ponovitev, alpha = rev(alpha.v), n=rev(n.v))
# m = 10
alpha.iz = 0.05

if(useOld&&file.exists("bootstrapV2.RDS")){
  rezultati_bootstrap = readRDS("bootstrapV2.RDS") 
}else{
  library(foreach)
  library(doParallel)
  library(doRNG)
  # parallel computing
  nc = detectCores()-1 
  cl = makeCluster(nc, outfile="log_boot") # shranjujemo konzolo
  registerDoParallel(cl)
  
  set.seed(2024)
  
  res2 = foreach(i = 1:nrow(settings), .combine=rbind) %dorng%{ 
    # generiranje podatkov
    alpha = settings$alpha[i]
    n = settings$n[i]
    data = generiranje_podatkov(beta0=100, beta1=3, beta2=2, alpha=alpha, n=n)

    ## bootstrap
    res = bootstrap(m, data, alpha, n)
    
    # izračun intervalov zaupanja
    int.iz  = quantile(res[,3], probs = c(alpha.iz/2, 1-alpha.iz/2))
    x1.iz  = quantile(res[,4], probs = c(alpha.iz/2, 1-alpha.iz/2))
    x2.iz  = quantile(res[,5], probs = c(alpha.iz/2, 1-alpha.iz/2))
    
    res.temp = c(alpha, n, mean(res[,3]), mean(res[,4]), mean(res[,5]),
                 int.iz[[1]], int.iz[[2]],
                 x1.iz[[1]], x1.iz[[2]],
                 x2.iz[[1]], x2.iz[[2]])
    
    # kje se nahaja zanka
    if(i%%100==0) cat("Iteration ", i, "/", nrow(settings), "complete! \n")
    
    return(res.temp)
    
  }
  rezultati = data.frame(alpha = res2[,1], velikost.vzorca = res2[,2], 
                         int.coef = res2[,3], x1.coef = res2[,4], x2.coef = res2[,5],
                         int.lower = res2[,6], int.upper = res2[,7],
                         x1.lower = res2[,8], x1.upper = res2[,9],
                         x2.lower = res2[,10], x2.upper = res2[,11])
  # Shranjevanje rezultatov
  saveRDS(object = rezultati, file = "bootstrapV2.RDS")
  
  stopCluster(cl)
}

rezultati_bootstrap = readRDS("bootstrapV2.RDS")
rownames(rezultati_bootstrap) = 1:9000 # to je samo lepotno

if(useOld&&file.exists("klasicna_metodaV2.RDS")){
  rezultati_klasicna = readRDS("klasicna_metodaV2.RDS") 
}else{
  library(foreach)
  library(doParallel)
  library(doRNG)
  # parallel computing
  nc = detectCores()-1 
  cl = makeCluster(nc, outfile="log_klasicna_metoda") # shranjujemo konzolo
  registerDoParallel(cl)
  
  set.seed(2024)
  
  res2 = foreach(i = 1:nrow(settings), .combine=rbind) %dorng%{ 
    # generiranje podatkov
    alpha = settings$alpha[i]
    n = settings$n[i]
    data = generiranje_podatkov(beta0=100, beta1=3, beta2=2, alpha=alpha, n=n)
    
    ## klasicna metoda
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
    # shranjevanj
    intervali.zaupanja = data.frame(alpha = alpha, velikost.vzorca = n,
                                              int.coef = int, x1.coef = x1, x2.coef = x2,
                                              int.lower = int.iz[1], int.upper = int.iz[2],
                                              x1.lower = x1.iz[1], x1.upper = x1.iz[2],
                                              x2.lower = x2.iz[1], x2.upper = x2.iz[2])
    
    
    # kje se nahaja zanka
    if(i%%100==0) cat("Iteration ", i, "/", nrow(settings), "complete! \n")
    
    return(intervali.zaupanja)
    
  }
  intervali.zaupanja.org = data.frame(alpha = res2[,1], velikost.vzorca = res2[,2], 
                                      int.coef = res2[,3], x1.coef = res2[,4], x2.coef = res2[,5],
                                      int.lower = res2[,6], int.upper = res2[,7], 
                                      x1.lower = res2[,8], x1.upper = res2[,9],
                                      x2.lower = res2[,10], x2.upper = res2[,11])
  # Shranjevanje rezultatov
  saveRDS(object = intervali.zaupanja.org, file = "klasicna_metodaV2.RDS")
  
  stopCluster(cl)
}

rezultati_klasicna = readRDS("klasicna_metodaV2.RDS")