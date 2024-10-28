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
m = 100 # st. ponovitev

stevilo.skupin.v = c(4, 8, 10)
velikost.skupin.v = c(20, 100, 200)
stevilo.spremenljivk.v = c(12, 24, 36)
diff.v = c(1, 2, 4, 10)

# parametri za test
m = 2
#stevilo.spremenljivk.v = 12
#velikost.skupin.v = 100
#stevilo.skupin.v = 8
#diff.v = 2

settings = expand.grid(i=1:m, stevilo.spremenljivk = rev(stevilo.spremenljivk.v), 
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
  library(mclust)
  
  # parallel computing
  nc = detectCores()-1 
  cl = makeCluster(nc, outfile="logSimulacija") # shranjujemo konzolo
  # nalozimo ustrezne pakete za vsak cluster worker
  clusterEvalQ(cl, { 
    library(mclust)})
  registerDoParallel(cl)
  
  set.seed(2024)

  res = cbind(settings, 
              ari.kmeans=NA, wss.kmeans=NA, pwss.kmeans=NA, 
              ari.mclust=NA, wss.mclust=NA, pwss.mclust=NA)
  for(row in 1:nrow(settings)){ 
    # row = 1
    # izberemo faktorje
    stevilo.spremenljivk = settings$stevilo.spremenljivk[row]
    stevilo.skupin = settings$stevilo.skupin[row]
    velikost.skupin = settings$velikost.skupin[row]
    diff = settings$diff[row]
  
    # generiramo podatke
    data = generiranje.podatkov(stevilo.spremenljivk= stevilo.spremenljivk, 
                                    velikost.skupin = velikost.skupin,
                                    stevilo.skupin = stevilo.skupin, 
                                    diff = diff) 
    # transformacija v data.frame
    data = as.data.frame(data)
    # skaliramo podatke za metodo razvrscanje na polagi modelov
    data.scale = scale(data[,1:stevilo.spremenljivk])
        
    # metoda kmeans
    kmeans.res = kmeans(data[,1:stevilo.spremenljivk], centers=stevilo.skupin, nstart = 100) #, nstart=nRep) 
    res$ari.kmeans[row] = blockmodeling::crand(data$skupina, kmeans.res$cluster) #ari
    res$wss.kmeans[row] = kmeans.res$tot.withinss #wss
    res$pwss.kmeans[row] = kmeans.res$tot.withinss/kmeans.res$totss #pwss
        
    # razvrscanje na polagi modelov
    suppressMessages({ # v konzoli se ne izpisuje fitting
      model = capture.output({
        mclust.res = mclust::Mclust(data.scale, G = stevilo.skupin) # kle nevem cist tocn, k zgori je za nstart kao zacetno stevilo skupin ane in pol sm mal googlala sam kle pa nevem cit tocn kaj nastavt...
      })
    })
    # ari
    res$ari.mclust[row] = blockmodeling::crand(data$skupina, mclust.res$classification) #ari
    # wss
    wss.mclust = sum(sapply(1:stevilo.skupin, function(k) {
      sum(rowSums((data.scale[mclust.res$classification == k, ] - colMeans(data.scale[mclust.res$classification == k, ]))^2))
    }))
    res$wss.mclust[row] = wss.mclust
    # pwss
    res$pwss.mclust[row] = wss.mclust / sum((data.scale - colMeans(data.scale))^2)
    
    # kje se nahaja zanka
    if(row%%10==0) cat("Iteration ", row, "/", nrow(settings), "complete! \n")
    }
  saveRDS(object = res, file="simulacija.RDS")
  stopCluster(cl)
}

# na enem slajdu v predavanjih pise: Why is "sum of squares within clusters" not a good measure?
# in moj zapisek na to je: Cilj clusteringa najti skupine ki so si čim manj podobne ampak elemnti v skupini pa čim bolj skupi. Nekatere metode optimize to mero(k-means) in nekatere ne, zato ni najboljša mera...
# sm sla gledat v lansko kar smo delala, ampak te dve metodi nimata nobene skupne mere...
# zdej pa nevem al dodava se wss al ne? mal sm googlala je se nek Silhuetni koeficient 
# meri pa, kako podobne so si točke znotraj gruče v primerjavi z najbližjo sosednjo gručo. Giblje se od -1 do 1, kjer večja vrednost pomeni boljše grupiranje.
################################# grafični prikaz ##############################
library(tidyr)
library(dplyr)
resLong = pivot_longer(res, cols =matches("^(ari|wss|pwss)\\."),  values_to = "value",
                       names_to = c("metric", "method"), names_pattern = "^(ari|wss|pwss)\\.(kmeans|mclust)") 
resWide <- resLong %>% pivot_wider(names_from = metric, values_from = value) # da so ari, wss in pwss vsaka svoj column

resAgg = aggregate(cbind(ari, wss, pwss) ~ stevilo.spremenljivk + velikost.skupin + stevilo.skupin + method, 
                   data = resWide, FUN = mean)

library(ggplot2)
# risemo adjR2
ggplot(resAgg, aes(y = ari, x = as.factor(stevilo.spremenljivk), col=method, group=method)) + # za x ponavadi dobro neki kar nima preveč vpliva
  geom_point() + geom_line() +
  facet_grid(stevilo.skupin ~ velikost.skupin, scales="free")





