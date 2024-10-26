set.seed(2024)

# potrebne knjiznice


########################## generiranje podatkov #############################
stevilo.skupin.v = c(4, 8, 10)
velikost.skupin.v = c(20, 100, 200)
stevilo.spremenljivk.v = c(8, 16, 32)
diff.v = c(0.5, 1, 2, 10)

settings = expand.grid(stevilo.spremenljivk = rev(stevilo.spremenljivk.v), 
                       velikost.skupin = rev(velikost.skupin.v), 
                       stevilo.skupin=rev(stevilo.skupin.v),
                       diff = rev(diff.v))

# funkcija za generiranje podatkov
generiranje.podatkov = function(stevilo.spremenljivk, velikost.skupin, stevilo.skupin, diff){
  # generiranje povprečij
  M = diag(stevilo.skupin)*diff 
  
  # neinformativne spremenljivke - same 0
  M = cbind(M, matrix(0, nrow=stevilo.skupin, ncol=stevilo.spremenljivk-stevilo.skupin)) 
  S = diag(stevilo.spremenljivk)
  
  X = NULL 
  # generamo podatke za vsako skupino posebaj
  for(i in 1:stevilo.skupin){
    iX = MASS::mvrnorm(n=velikost.skupin, mu = M[i,], Sigma = S)
    X = rbind(X,iX)
  }
  
  # dodamo se skupino
  X = cbind(X, skupina=rep(1:stevilo.skupin,each=velikost.skupin)) # clu = skupina
  return(X)
}




