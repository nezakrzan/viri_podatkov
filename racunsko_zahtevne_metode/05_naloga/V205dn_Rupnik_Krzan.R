library(ggplot2)
library(gridExtra)
set.seed(2024)

# =============================== FUNKCIJE =====================================
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

# ============================== ALGORITEM =====================================
# Metropolis-Hastings algorithm
MH_algorithm = function(n, burnIn, step, x0, y0){
  set.seed(2024)
  res = matrix(NA, nrow=n, ncol=2)
  
  for(i in 1:(n*step + burnIn)){
    prop = predlagane(x0, y0)
    xP = prop[1]
    yP = prop[2]
    
    alpha = gostota(xP, yP)*g(x0, y0, xP, yP)/(gostota(x0, y0)*g(xP, yP, x0, y0))
    alpha = min(alpha, 1)
    
    if(runif(1) < alpha){
      x0 = xP
      y0 = yP
    } 
    if(i > burnIn & (i- burnIn)%%step==0){
      j = (i- burnIn)/step
      res[j,] = c(x0, y0)
    }
    #if(i %% 100000 == 0){
    #print(paste(round(i/(n*step + burnIn), 3), "%", sep=""))
    #}
  }
  return(res)
}

# ================================ GENERIRANJE =================================
useOld = T
n = 10000
x0 = 4
y0 = 4
# ------------------------------- test funkcije --------------------------------
burnIn = 1
step = 1
if(useOld&&file.exists("V2_MH_algorithm_test.RDS")){
  test = readRDS("V2_MH_algorithm_test.RDS") 
}else{
  test = MH_algorithm(n, burnIn, step, x0, y0)
  saveRDS(object = test, file = "V2_MH_algorithm_test.RDS")
}

# Graf gibanja prvih 150, 500 in 1500 vrednosti za X in Y."
vzorec_test = readRDS("V2_MH_algorithm_test.RDS")
par(mfrow=c(3,2))
plot(vzorec_test[1:150,1], type="l", ylab="vrednosti X", main="Prvih 150 vrednosti")
plot(vzorec_test[1:150,2], type="l", ylab="vrednosti Y", main=" ")
plot(vzorec_test[1:500,1], type="l", ylab="vrednosti X", main="Prvih 500 vrednosti")
plot(vzorec_test[1:500,2], type="l", ylab="vrednosti Y", main=" ")
plot(vzorec_test[1:1500,1], type="l", ylab="vrednosti X", main="Prvih 1500 vrednosti")
plot(vzorec_test[1:1500,2], type="l", ylab="vrednosti Y", main=" ")

# --------------------------------- step ---------------------------------------
burnIn = 100
step = 10
if(useOld&&file.exists("V2_MH_algorithm_step1.RDS")){
  test = readRDS("V2_MH_algorithm_step1.RDS") 
}else{
  test = MH_algorithm(n, burnIn, step, x0, y0)
  saveRDS(object = test, file = "V2_MH_algorithm_step1.RDS")
}

burnIn = 100
step = 100
if(useOld&&file.exists("V2_MH_algorithm_step2.RDS")){
  test = readRDS("V2_MH_algorithm_step2.RDS") 
}else{
  test = MH_algorithm(n, burnIn, step, x0, y0)
  saveRDS(object = test, file = "V2_MH_algorithm_step2.RDS")
}

# "Graf avtokorelacije in križne avtokorelacije za X in Y pri step=10 in step=100."
vzorec1 = readRDS("V2_MH_algorithm_step1.RDS")
vzorec = readRDS("V2_MH_algorithm_step2.RDS")
par(mfrow=c(2,3))
acf(vzorec1[,1], main="step=10", ylab="koef. avtokorelacije X")
acf(vzorec1[,2], main=" ", ylab="koef. avtokorelacije Y")
ccf(vzorec1[,1], vzorec1[,2], main=" ", ylab="krizna avtokorelacija")
acf(vzorec[,1], main="step=100", ylab="koef. avtokorelacije X")
acf(vzorec[,2], main=" ", ylab="koef. avtokorelacije Y")
ccf(vzorec[,1], vzorec[,2], main=" ", ylab="krizna avtokorelacija")


# ------------------------------ prikaz vzorca ---------------------------------
X = data.frame(vzorec)

ggplot(X, aes(x = X1, y = X2, asp=1))  +
  geom_point() + coord_fixed() +
  geom_density_2d() + 
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha=0.5) +
  theme_minimal()

# ======================= VERJETNOST P(x < 1 in y < 1) =========================
# Vrednosti za vzorec velikosti 10000 z označenim območjem [0,1]x[0,1].
ggplot(X, aes(x = X1, y = X2, asp = 1)) +
  geom_point() +
  coord_fixed() +
  geom_density_2d() +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.5) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
            fill = NA, color = "red", linewidth = 0.7) +  
  theme_minimal()

# verjetnost P(x < 1 in y < 1)
manjsiOd1 = function(vrednosti){
  res = mean(vrednosti[,1]<=1 & vrednosti[,2]<=1)
  return(res)
}

# ------------------------- generiranje populacije -----------------------------
# ocenjena verjetnost, da sta x < 1 in y < 1 (ena ponovitev-populacija)
n = 1000000
burnIn = 100
step = 100

if(file.exists("V2_vzorec_velik.RDS")){
  vzorec_velik = readRDS("V2_vzorec_velik.RDS") 
} else {
  set.seed(2024)
  vzorec_velik = MH_algorithm(n, burnIn, step, x0, y0)
  saveRDS(object = vzorec_velik, file = "V2_vzorec_velik.RDS")
}

# ---------------------------- racunanje verjetnosti ---------------------------
# verjetnost P(x < 1 in y < 1)
manjsiOd1 = function(vrednosti){
  res = mean(vrednosti[,1]<1 & vrednosti[,2]<1)
  return(res)
}

# ocenjena verjetnost
populacija = readRDS("V2_vzorec_velik.RDS") 
trueProp = manjsiOd1(populacija) # 0.091%

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

if(file.exists("V2_skupine.RDS")){
  df_skupine = readRDS("V2_skupine.RDS") 
} else {
  set.seed(2024)
  df_skupine =  data.frame(x = skupine(vzorec_velik[,1], "X"),
                           y = skupine(vzorec_velik[,2], "Y"))
  saveRDS(object = df_skupine, file = "V2_skupine.RDS")
}

# tabela
df_skupine = readRDS("V2_skupine.RDS") 
df_skupine$x = factor(df_skupine$x, levels = c("X<1", "1<=X<2", "2<=X<3", "3<=X"),
                      labels = c("X<1", "1<=X<2", "2<=X<3", "3<=X"))
df_skupine$y = factor(df_skupine$y, levels = c("Y<1", "1<=Y<2", "2<=Y<3", "3<=Y"),
                      labels = c("Y<1", "1<=Y<2", "2<=Y<3", "3<=Y"))
tabela = table(df_skupine$x, df_skupine$y)
tabela = (tabela / 1000000) * 100
tabela = apply(round(tabela, 2), c(1, 2), function(x) paste0(x, "\\%"))
colnames(tabela) = c("$Y < 1$", "$1 \\leq Y < 2$", "$2 \\leq Y < 3$", "$3 \\leq Y$")
rownames(tabela) = c("$X < 1$", "$1 \\leq X < 2$", "$2 \\leq X < 3$", "$3 \\leq X$")
kable(tabela, align = "c", 
      caption = "Tabela razporeditev vrednosti glede na X in Y v populaciji.",
      format = "latex", escape = FALSE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))


# -------------------- simulacija (vzorci velikosti 100)------------------------
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

# Histogram porazdelitve verjetnosti P(X<1 in Y<1)
ggplot(data.frame(x = porazdelitev), aes(x = x)) + 
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black", alpha = 0.7) + 
  labs(title = " ", x = " ", y = "Frequency") +
  theme_minimal()

# -------------------- pokritost (vzorci velikosti 100)------------------------
# simulacija_pokritost = function(st_ponovitev, st_vzorcev){
#   library(foreach)
#   library(doParallel)
#   library(doRNG)
#   # parallel computing
#   nc = detectCores()-1 
#   cl = makeCluster(nc) # shranjujemo konzolo
#   clusterExport(cl, c("simulacija_porazdelitev", "vzorec_velik", "trueProp"))
#   registerDoParallel(cl)
#   
#   res2 = foreach(i = 1:st_ponovitev, .combine=rbind, .export=c("simulacija_porazdelitev")) %dorng%{ 
#     # for(i in 1:st_ponovitev){
#     vred = simulacija_porazdelitev(st_vzorcev)
#     spodnja = mean(vred) - 1.96*sd(vred)/sqrt(st_vzorcev)
#     zgornja = mean(vred) + 1.96*sd(vred)/sqrt(st_vzorcev)
#     if(spodnja <= trueProp && trueProp <= zgornja){
#       # res = c(res, 1)
#       return(1)
#     } else {
#       # res = c(res, 0)
#       return(0)
#     }
#   }
#   return(mean(res2))
# }
# 
# if(file.exists("pokritost.RDS")){
#   pokritost = readRDS("pokritost.RDS") 
# } else {
#   set.seed(2024)
#   pokritost = simulacija_pokritost(1000, 1000)
#   saveRDS(object = pokritost, file = "pokritost.RDS")
# }
# 
# pokritost = readRDS("pokritost.RDS") 

# dodajanje indikatorja, če sta x in y manjša od 1
populacija = readRDS("V2_vzorec_velik.RDS") 
ind = ifelse(populacija[,1] < 1 & populacija[,2] < 1, 1, 0)
populacija = cbind(populacija, ind)

# prava verjetnost na populaciji
trueProp = manjsiOd1(populacija)

sample.size = 10000  # velikost vzorca
n.sim = 10000  # število simulacij
coverage.count = 0

for (i in 1:n.sim) {
  # Naključno izberemo vzorec velikosti 1000 iz populacije
  sample = sample(ind, size = sample.size, replace = TRUE)
  
  # Izračunamo delež v vzorcu
  sample_prop = mean(sample)
  
  # Izračunamo interval zaupanja za proporcijo
  test_result = prop.test(sum(sample), sample.size, p = sample_prop, conf.level = 0.95)
  
  # Preverimo, če pravi delež leži v intervalu zaupanja
  if (test_result$conf.int[1] <= trueProp && test_result$conf.int[2] >= trueProp) {
    coverage.count = coverage.count + 1
  }
}

# Pokritost: delež simulacij, kjer pravi delež leži v intervalu zaupanja
coverage = coverage.count / n.sim
coverage









