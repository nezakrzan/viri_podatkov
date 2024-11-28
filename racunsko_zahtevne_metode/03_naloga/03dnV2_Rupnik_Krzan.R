set.seed(2024)

# =============================== faktorji ====================================
alpha.v = c(0.6, 1, 1.2)
n.v = c(20, 200, 500)
st.ponovitev = 1000

# bootstrap vzorci
m = 1000

useOld = FALSE # ne uporabljal starega rezultata

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
m = 10

if(useOld&&file.exists(c("bootstrapV2.RDS", "intervali.zaupanja.orgV2.RDS"))){
  rezultati = readRDS(c("bootstrapV2.RDS", "intervali.zaupanja.orgV2.RDS")) 
}else{
  intervali.zaupanja.org = data.frame(alpha = numeric(), gamma = numeric(), velikost.vzorca = numeric(),
                                      int.coef = numeric(), x1.coef = numeric(), x2.coef = numeric(),
                                      int.lower = numeric(), int.upper = numeric(),
                                      x1.lower = numeric(), x1.upper = numeric(), 
                                      x2.lower = numeric(), x2.upper = numeric())
  rezultati = matrix(ncol = 5)
  colnames(rezultati) = c("alpha", "velikost.vzorca", "int", "x1", "x2")
  for(i in 1:nrow(settings)){
    # i = 2
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
    intervali.zaupanja.org = rbind(intervali.zaupanja.org, 
                                   data.frame(alpha = alpha, velikost.vzorca = n,
                                              int.coef = int, x1.coef = x1, x2.coef = x2,
                                              int.lower = int.iz[1], int.upper = int.iz[2],
                                              x1.lower = x1.iz[1], x1.upper = x1.iz[2],
                                              x2.lower = x2.iz[1], x2.upper = x2.iz[2]))

    ## botstrap
    res = bootstrap(m, data, alpha, n)
    # shranjevanje
    rezultati = rbind(rezultati, res)
    
    # kje se nahaja zanka
    if(i%%100==0) cat("Iteration ", i, "/", nrow(settings), "complete! \n")
  }
  # shranjevanje
  rezultati <- rezultati[-1, ]
  saveRDS(object = rezultati, file="bootstrapV2.RDS")
  saveRDS(object = intervali.zaupanja.org, file="intervali.zaupanja.orgV2.RDS")
}



