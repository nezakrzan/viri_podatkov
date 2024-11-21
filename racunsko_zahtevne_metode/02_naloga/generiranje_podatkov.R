library(MASS)

set.seed(2024)

stevilo.neinformativnih.sprem = 1
velikost.skupin = 100
stevilo.skupin = 4
diff = 2
cor = 0

# generiram podatke za informativne spremenljivke
generiranje.podatkov = function(
    velikost.skupin, stevilo.skupin, stevilo.neinformativnih.sprem, diff, cor){
  ## informativne spremenljivke
  # povprecja
  Mu = diag(stevilo.skupin)*diff
  # sigma
  st.spremenljivk = stevilo.skupin+stevilo.neinformativnih.sprem
  Sigma = matrix(cor, ncol=st.spremenljivk, nrow=st.spremenljivk)
  diag(Sigma) = 1
  
  ## neinformativne spremenljivke
  # generiranje neinformativnih spremenljivk
  non.infor = matrix(0, nrow=stevilo.skupin, ncol=stevilo.neinformativnih.sprem)
  # zdruzimo
  M = cbind(Mu, non.infor)
 
  # generamo podatke
  X = NULL 
  for(i in 1:stevilo.skupin){
    iX = MASS::mvrnorm(n=velikost.skupin, mu = M[i,], Sigma = Sigma)
    X = rbind(X,iX)
  } 
  
  # dodava se skupine
  X = cbind(X, skupina=rep(1:stevilo.skupin, each=velikost.skupin))
  
  return(X)
}



