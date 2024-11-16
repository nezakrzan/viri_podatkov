library(MASS)

set.seed(2024)

stevilo.spremenljivk = 5
stevilo.neinformativnih.sprem = 4
velikost.skupin = 100
stevilo.skupin = 4
diff = 2
cor = 0.1

# njegov komentar:
# Navodila pravijo, da morate v osnovi generirati podatke iz bivariatne normalne porazdelitve. 
# Torej tisto, kar določa razlike med skupinami, morata biti dve spremenljivki.
# Sicer morate potem preučevati dodajanje nepomembnih spremenljivk, a tega dejavnika vsaj jasno ne vidim tu.
# Tudi če gledam kodo se mi zdi, da je bil cilj čim bom "uporabiti" kodo iz vaj, kot pa res narediti svojo simulacijo.

# Moja ideja:
# zgenerirava podatke iz bivar mult normalne(karkoli že je to) za določeno število spremenljivk in to pustiva skos fiksno?
# potem pa dodajava neinformativne spremenljivke - torej da mava enkar podatke s 5 informativnimi spremenljivkami in 3 neinformativnimi, potem naslednjič s 5 informativnimi in 6 neinformativnimi
# pač da je število informativnih skos enako, neinformativne pa spreminjava

# njegov drugi komentar:
# Dobro bi bilo, če bi preostale dejavnike (poleg v naprej določenega) izbrala tako, da bi 
# vsaj kakšen tak, da bi pri eni strani pričakovali, da bo boljša ena metoda, pri drugi strani 
# (min-max vrednosti) pa druga (oz. da bosta na eni strani enakovredni).
# Recimo, metoda voditeljev predpostavlja okrogle skupine (korelacije 0) z enake variance med skupinami in 
# spremenljivkami znotraj skupin,  Mclust pa ne.

# Moja ideja:
# torej rabva med skupinami neko korelacijo? Seprav rabva za sigmo po diagonali med neko korelacijo? Al? madonis sm zmedena hahah

# generiram podatke za informativne spremenljivke
generiranje.podatkov = function(
    velikost.skupin, stevilo.skupin, stevilo.spremenljivk = 5, stevilo.neinformativnih.sprem, diff, cor){
  ## informativne spremenljivke
  # povprecja
  Mu = diag(stevilo.spremenljivk)*diff
  # sigma
  st.spremenljivk = stevilo.spremenljivk+stevilo.neinformativnih.sprem
  Sigma = matrix(cor, ncol=st.spremenljivk, nrow=st.spremenljivk)
  diag(Sigma) = 1
  
  ## neinformativne spremenljivke
  # generiranje neinformativnih spremenljivk
  non.infor = matrix(0, nrow=stevilo.spremenljivk, ncol=stevilo.neinformativnih.sprem)
  # zdruzimo
  M = cbind(Mu, non.infor)
 
  # generamo podatke
  X = NULL 
  for(i in 1:stevilo.skupin){
    iX = mvrnorm(n=velikost.skupin, mu = M[i,], Sigma = Sigma)
    X = rbind(X,iX)
  } 
  
  # dodava se skupine
  X = cbind(X, skupina=rep(1:stevilo.skupin, each=velikost.skupin))
  
  return(X)
}

data.primer1 = generiranje.podatkov(velikost.skupin, stevilo.skupin, stevilo.spremenljivk = 5, 
                     stevilo.neinformativnih.sprem, diff, cor)
pairs(data.primer1[,1:5], col=data.primer1[,10])




