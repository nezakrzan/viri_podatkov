set.seed(2024)

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
m = 1000 # st. ponovitev

stevilo.skupin.v = c(4, 8, 10)
velikost.skupin.v = c(20, 100, 200)
stevilo.spremenljivk.v = c(12, 24, 36)
diff.v = c(1, 2, 10)

# stevilo.zacetnih.skupin = c(1, 4, 10, 100)

# parametri za test
m = 2
# stevilo.zacetnih.skupin = c(1,4) 
# stevilo.spremenljivk.v = 12
# velikost.skupin.v = 100
# stevilo.skupin.v = 8
# diff.v = 2
# stevilo.zacetnih.skupin = 10
# data = generiranje.podatkov(stevilo.spremenljivk, velikost.skupin, stevilo.skupin, diff)


settings = expand.grid(stevilo.spremenljivk = rev(stevilo.spremenljivk.v), 
                       velikost.skupin = rev(velikost.skupin.v), 
                       stevilo.skupin=rev(stevilo.skupin.v),
                       diff = rev(diff.v))


useOld = FALSE # ne uporabljal starega rezultata

if(useOld&&file.exists("simulacija.RDS")){
  res = readRDS("simulacija.RDS")
}else{
  # potrebne knjiznice
  # neke tezave z mclust zato je tkole zdej
  # potrebne.knjiznice <- c("foreach", "doParallel", "doRNG", "mclust", "blockmodeling")
  # nove.knjiznice <- potrebne.knjiznice[!(potrebne.knjiznice %in% installed.packages()[,"Package"])]
  # if (length(nove.knjiznice)) install.packages(nove.knjiznice)
  # lapply(potrebne.knjiznice, library, character.only = TRUE)
  
  library(foreach)
  library(doParallel)
  library(doRNG)
  
  # parallel computing
  nc = detectCores()-1 
  cl = makeCluster(nc, outfile="logSimulacija") # shranjujemo konzolo
  # nalozimo ustrezne pakete za vsak cluster worker
  clusterEvalQ(cl, { 
    library(mclust)
  })
  registerDoParallel(cl)
  
  set.seed(2024)
  timePar = system.time({ # za time
    res = foreach(i = 1:nrow(settings), .combine=rbind) %dorng% { 
      # i = 1
      # izberemo faktorje
      stevilo.spremenljivk = settings$stevilo.spremenljivk[i]
      stevilo.skupin = settings$stevilo.skupin[i]
      velikost.skupin = settings$velikost.skupin[i]
      diff = settings$diff[i]
      
      # za rezultate
      res = data.frame(settings, #id=NA, nRep=NA, 
                       ari.kmeans=NA, wss.kmeans=NA, pwss.kmeans=NA,
                       ari.mclust=NA, wss.mclust=NA, pwss.mclust=NA) 
      
      # # pazi da bodo pravi podatki v pravi vrstici
      # ind = 0
      
      for(j in 1:m){ 
        # j = 1
        # generiramo podatke
        data = generiranje.podatkov(stevilo.spremenljivk= stevilo.spremenljivk, 
                                    velikost.skupin = velikost.skupin,
                                    stevilo.skupin = stevilo.skupin, 
                                    diff = diff) 
        # transformacija v data.frame
        data = as.data.frame(data)
        # skaliramo podatke za metodo razvrscanje na polagi modelov
        # data.scale <- scale(data[,1:stevilo.spremenljivk])
        
        # metoda kmeans
        kmeans.res = kmeans(data[,1:stevilo.spremenljivk], centers=stevilo.skupin) #, nstart=nRep) 
        res$ari.kmeans[j] = blockmodeling::crand(data$skupina, kmeans.res$cluster) #ari
        res$wss.kmeans[j] = kmeans.res$tot.withinss #wss
        res$pwss.kmeans[j] = kmeans.res$tot.withinss/kmeans.res$totss #pwss
        
        # razvrscanje na polagi modelov
        mclust.res = mclust::Mclust(data[,1:stevilo.spremenljivk], G = stevilo.skupin) # kle nevem cist tocn, k zgori je za nstart kao zacetno stevilo skupin ane in pol sm mal googlala sam kle pa nevem cit tocn kaj nastavt...
        res$ari.mclust[j] = blockmodeling::crand(data$skupina, mclust.res$classification) #ari
        # wss
        wss = sum(sapply(1:stevilo.skupin, function(k){
          sum(rowSums((
            data[,1:stevilo.spremenljivk][mclust.res$classification == k, ] - colMeans(data[,1:stevilo.spremenljivk][mclust.res$classification == k, ]))^2))
        }))
        res$wss.mclust[j] = wss
        # pwss
        res$pwss.mclust[j] = wss / sum((data[,1:stevilo.spremenljivk] - colMeans(data[,1:stevilo.spremenljivk]))^2)
        
        # dala sm vn ta nstart ker to ni to kar sm jst misla da je in zdej niti ne vem tocno vec kaj je neki da hitrejs skonvergira, uglavnem lani tega nismo delal....
        # sm pa pustla zakomentirano za vsak slucaj ce ti ugotovis, sam men se zdi to bw ker ma samo kmeans to ostale metode nimajo....
        
        # for(nRep in stevilo.zacetnih.skupin){ 
        #   # nRep = 4
        #   # zapis v pravo vrstico
        #   ind = ind+1 
        #   res$id[ind] = i*m*10 + j
        #   res$nRep[ind] = nRep
        #   
        #   # metoda kmeans
        #   kmeans.res = kmeans(data[,1:stevilo.spremenljivk], centers=stevilo.skupin, nstart=nRep) 
        #   res$ari.kmeans[ind] = blockmodeling::crand(data$skupina, kmeans.res$cluster) #ari
        #   res$wss.kmeans[ind] = kmeans.res$tot.withinss #wss
        #   res$pwss.kmeans[ind] = kmeans.res$tot.withinss/kmeans.res$totss #pwss
        #   
        #   # razvrscanje na polagi modelov
        #   mclust.res = mclust::Mclust(data.scale, G = stevilo.skupin) # kle nevem cist tocn, k zgori je za nstart kao zacetno stevilo skupin ane in pol sm mal googlala sam kle pa nevem cit tocn kaj nastavt...
        #   res$ari.mclust[ind] = blockmodeling::crand(data$skupina, mclust.res$classification) #ari
        #   # wss
        #   wss = sum(sapply(1:stevilo.skupin, function(k){
        #     sum(rowSums((
        #       data.scale[mclust.res$classification == k, ] - colMeans(data.scale[mclust.res$classification == k, ]))^2))
        #   }))
        #   res$wss.mclust[ind] = wss
        #   # pwss
        #   res$pwss.mclust[ind] = wss / sum((data.scale - colMeans(data.scale))^2)
        # }
      }
      return(res)
    }
  })
  attr(res, "time") = timePar
  cat("Time needed in min:", timePar[3]/60, "\n")
  saveRDS(res, "simulacija.RDS")
  stopCluster(cl)
}

################################# grafični prikaz ##############################
resAgg = aggregate(cbind(ari.kmeans, wss.kmeans, pwss.kmeans, ari.mclust, wss.mclust, pwss.mclust) ~ 
                     stevilo.spremenljivk + velikost.skupin + stevilo.skupin, data = res, FUN = mean)
# treba je dodat method...
library(ggplot2)
# risemo adjR2
ggplot(resAgg, aes(y = ari.kmeans, x = as.factor(stevilo.spremenljivk))) + # za x ponavadi dobro neki kar nima preveč vpliva
  geom_point() + geom_line() +
  facet_grid(stevilo.skupin ~ velikost.skupin, scales="free")
