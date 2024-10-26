set.seed(2024)

# potrebne knjiznice


########################## generiranje podatkov #############################
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

################################## simulacija ##################################
m = 5 # st. ponovitev

stevilo.skupin.v = c(4, 8, 10)
velikost.skupin.v = c(20, 100, 200)
stevilo.spremenljivk.v = c(12, 24, 36)
diff.v = c(0.5, 1, 2, 10)

stevilo.zacetnih.skupin = c(1, 4, 10, 100)

settings = expand.grid(stevilo.spremenljivk = rev(stevilo.spremenljivk.v), 
                       velikost.skupin = rev(velikost.skupin.v), 
                       stevilo.skupin=rev(stevilo.skupin.v),
                       diff = rev(diff.v))

useOld = FALSE # ne uporabljal starega rezultata

if(useOld&&file.exists("simulacija.RDS")){
  res = readRDS("simulacija.RDS")
}else{
  # potrebne knjiznice
  library(foreach)
  library(doParallel)
  library(doRNG)
  
  # parallel computing
  nc = detectCores()-1 
  cl = makeCluster(nc, outfile="logSimulacija") # shranjujemo konzolo
  registerDoParallel(cl)
  
  set.seed(2024)
  timePar = system.time({ # za time
    res = foreach(i = 1:nrow(settings), .combine=rbind) %dorng% { 
      # izberemo faktorje
      stevilo.spremenljivk = settings$stevilo.spremenljivk[i]
      stevilo.skupin = settings$stevilo.skupin[i]
      velikost.skupin = settings$velikost.skupin[i]
      diff = settings$diff[i]
      
      # za rezultate
      res = data.frame(settings[rep(i,m*length(stevilo.zacetnih.skupin)),], id=NA, nRep=NA, ari=NA, wss=NA, pwss=NA) 
      
      # pazi da bodo pravi podatki v pravi vrstici
      ind = 0
      
      for(j in 1:m){ 
        # generiramo podatke
        data = generiranje.podatkov(stevilo.spremenljivk= stevilo.spremenljivk, 
                                    velikost.skupin = velikost.skupin,
                                    stevilo.skupin = stevilo.skupin, 
                                    diff = diff) 
        # transformacija v data.frame
        data = as.data.frame(data)
        
        # najprej sm nardila sam za eno metodo
        for(nRep in stevilo.zacetnih.skupin){ 
          # metoda kmeans
          kmeans.res = kmeans(data[,1:stevilo.spremenljivk], centers=stevilo.skupin, nstart=nRep) 
          
          # zapis v pravo vrstico
          ind = ind+1 
          res$id[ind] = i*m*10 + j
          res$nRep[ind] = nRep
          
          # mere
          res$ari[ind] = blockmodeling::crand(data$skupina, kmeans.res$cluster) #prava vrednost in vrednost iz rezultatov
          res$wss[ind] = kmeans.res$tot.withinss
          res$pwss[ind] = kmeans.res$tot.withinss/kmeans.res$totss
        }
      }
      cat("Setting ",i,"/",nrow(settings), " done.\t", format(Sys.time()) ,"\n", sep="")
      return(res)
    }
  })
  attr(res, "time") = timePar
  cat("Time needed in min:", timePar[3]/60, "\n")
  saveRDS(res, "simulacija.RDS")
  stopCluster(cl)
}



