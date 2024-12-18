library(ggplot2)
# library(rgl)
# source("plot3d.R")

set.seed(2024)

# funkcija gostote
gostota = function(x, y){
  if(x > 0 & y > 0){
    vred = x^2 * y^2 * exp(-x) * exp(-y) * exp(-x*y)
    return(vred)
  } else {
    return(0)
  }
}

# generiranje tock
predlagane = function(x_stari, y_stari){
  xP = rnorm(1, x_stari, 1)
  yP = rnorm(1, y_stari, 1)
  return(c(xP, yP))
}

# gostota porazdelitve (pogojno)
g = function(x_stari, y_stari, x_novi, y_novi){
  return(dnorm(x_novi, mean=x_stari, sd=1)*dnorm(y_novi, mean=y_stari, sd=1))
}


# Metropolis-Hastings algorithm
MH_algorithm = function(n, burnIn, step, x0, y0){
  set.seed(2024)
  res = matrix(NA, nrow=n, ncol=2)
  
  for(i in 1:(n*step + burnIn)){
    prop = predlagane(x0, y0)
    xP = prop[1]
    yP = prop[2]
    
    alpha = gostota(xP, yP)*g(xP, yP, x0, y0)/(gostota(x0, y0)*g(x0, y0, xP, yP))
    alpha = min(alpha, 1)
    
    if(runif(1)<alpha){
      x0 = xP
      y0 = yP
    } 
    if(i > burnIn & (i- burnIn)%%step==0){
      j = (i- burnIn)/step
      res[j,] = c(x0, y0)
    }
    if(i %% 100000 == 0){
      print(paste(round(i/(n*step + burnIn), 3), "%", sep=""))
    }
  }
  return(res)
}

# primer generiranja vrednosti
n = 1000
burnIn = 100
step = 100

test = MH_algorithm(n, burnIn, step, 2, 2)


# graficni prikazi
plot(test, asp=1, col=alpha("black", 0.1), pch=19, cex=0.6)
plot(test[1:300,1], type="l")
plot(test[1:300,2], type="l")

acf(test[,1])
acf(test[,2])

X = data.frame(test)

ggplot(X, aes(x = X1, y = X2, asp=1))  +
  geom_point() + coord_fixed() +
  geom_density_2d() + 
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha=0.5)


# hist3d(X, nclass = 20, scale = 200)

# ocenjena verjetnost, da sta x in y manjsa od 1 (ena ponovitev-velik vzorec)
n = 1000000
burnIn = 100
step = 100

if(file.exists("vzorec_velik.RDS")){
  vzorec_velik = readRDS("vzorec_velik.RDS") 
} else {
  set.seed(2024)
  vzorec_velik = MH_algorithm(n, burnIn, step, 2, 2)
  saveRDS(object = vzorec_velik, file = "vzorec_velik.RDS")
}

manjsiOd1 = function(vrednosti){
  res = mean(vrednosti[,1]<=1 & vrednosti[,2]<=1)
  return(res)
}

trueProp = manjsiOd1(vzorec_velik) # 0.091%

# risanje
# X = data.frame(vzorec_velik)
# 
# graf = ggplot(X, aes(x = X1, y = X2, asp=1))  +
#   geom_point() + coord_fixed() +
#   geom_density_2d() + 
#   stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha=0.5) 

# podatki za tabelo
skupine = function(data, kateri){
  res = matrix(NA, nrow = length(data), ncol=1)
  ind1 = which(data<1)
  ind2 = which(1 <= data & data < 2)
  ind3 = which(2 <= data & data < 3)
  ind4 = which(3 <= data)
  res[ind1] = paste(kateri,"<1", sep = "")
  res[ind2] = paste("1<=",kateri,"<2", sep = "")
  res[ind3] = paste("2<=",kateri,"<3", sep = "")
  res[ind4] = paste("3<=", kateri, sep = "")
  return(res)
}

if(file.exists("skupine.RDS")){
  df_skupine = readRDS("skupine.RDS") 
} else {
  set.seed(2024)
  df_skupine =  data.frame(x = skupine(vzorec_velik[,1], "X"),
                           y = skupine(vzorec_velik[,2], "Y"))
  saveRDS(object = df_skupine, file = "skupine.RDS")
}

table(df_skupine$x, df_skupine$y)


# ______________ simulacija (vzorci velikosti 100)

# porazdelitev
simulacija_porazdelitev = function(n){
  res = c()
  for(i in 1:n){
    indeksi = sample(1:nrow(vzorec_velik), 100)
    vzorec = vzorec_velik[indeksi,]
    delez = mean(vzorec[,1]<=1 & vzorec[,2]<=1)
    res[i] = delez
  }
  return(res)
}

# za 10000 vzorcev velikosti 100
porazdelitev = simulacija_porazdelitev(10000)
# normalna porazdelitev s parametroma
round(c(mean(porazdelitev), sd(porazdelitev)),5)

if(file.exists("parametri.RDS")){
  parametri = readRDS("parametri.RDS") 
} else {
  set.seed(2024)
  parametri = matrix(NA, nrow=10, ncol=2)
  for(i in 1:10){
    porazdelitev = simulacija_porazdelitev(10000)
    parametri[i,] = round(c(mean(porazdelitev), sd(porazdelitev)),4)
  }
  saveRDS(object = parametri, file = "parametri.RDS")
}

parametri = readRDS("parametri.RDS") 

# pokritost

simulacija_pokritost = function(st_ponovitev, st_vzorcev){
  
  library(foreach)
  library(doParallel)
  library(doRNG)
  # parallel computing
  nc = detectCores()-1 
  cl = makeCluster(nc) # shranjujemo konzolo
  clusterExport(cl, c("simulacija_porazdelitev", "vzorec_velik", "trueProp"))
  registerDoParallel(cl)
  
  res2 = foreach(i = 1:st_ponovitev, .combine=rbind, .export=c("simulacija_porazdelitev")) %dorng%{ 
  # for(i in 1:st_ponovitev){
    vred = simulacija_porazdelitev(st_vzorcev)
    spodnja = mean(vred) - 1.96*sd(vred)/sqrt(st_vzorcev)
    zgornja = mean(vred) + 1.96*sd(vred)/sqrt(st_vzorcev)
    if(spodnja <= trueProp && trueProp <= zgornja){
      # res = c(res, 1)
      return(1)
    } else {
      # res = c(res, 0)
      return(0)
    }
  }
  return(mean(res2))
}

# simulacija_pokritost(100, 1000)


if(file.exists("pokritost.RDS")){
  pokritost = readRDS("pokritost.RDS") 
} else {
  set.seed(2024)
  pokritost = simulacija_pokritost(1000, 1000)
  saveRDS(object = pokritost, file = "pokritost.RDS")
}

pokritost = readRDS("pokritost.RDS") 
