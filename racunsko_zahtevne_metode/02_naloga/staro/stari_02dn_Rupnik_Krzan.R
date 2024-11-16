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
# m = 2
# stevilo.spremenljivk.v = 12
# velikost.skupin.v = 100
# stevilo.skupin.v = 8
# diff.v = 2

settings = expand.grid(i=1:m, stevilo.spremenljivk = rev(stevilo.spremenljivk.v), 
                       velikost.skupin = rev(velikost.skupin.v), 
                       stevilo.skupin=rev(stevilo.skupin.v),
                       diff = rev(diff.v))

useOld = TRUE # ne uporabljal starega rezultata

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
        mclust.res = mclust::Mclust(data.scale, G = stevilo.skupin) 
      })
    })
    # ari
    res$ari.mclust[row] = blockmodeling::crand(data$skupina, mclust.res$classification) 
    ## wss
    # wss.mclust = sum(sapply(1:stevilo.skupin, function(k) {
    #   sum(rowSums((as.matrix(data.scale[mclust.res$classification == k, ])- 
    #                  colMeans(as.matrix(data.scale[mclust.res$classification == k, ])))^2))
    # }))
    # res$wss.mclust[row] = wss.mclust
    # # pwss
    # res$pwss.mclust[row] = wss.mclust / sum((data.scale - colMeans(data.scale))^2)
    
    temp1 = aggregate(data[,1:stevilo.spremenljivk], by=list(mclust.res$classification),
                           function(x) sum(scale(x, scale=FALSE)^2))
    res$wss.mclust[row] = sum(temp1[, -1]) # Total (within) sum of squares
    
    # by=list(...) narediva kot da so vsi v enem clustru
    temp2 = aggregate(data[,1:stevilo.spremenljivk], by=list(rep(1, mclust.res$n)),
                      function(x) sum(scale(x, scale=FALSE)^2))
    res$pwss.mclust[row] = sum(temp1[, -1]) / sum(temp2[, -1])
    
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
# Tom28: ja jst imam zapisano da bi bila wss pristranska mera ker kokr si napisala jo ene metode optimizirajo ene pa ne...tko da jst 
# kaj pa če z "Randov indeks" ang. Rand index preveriva koliko se ujema prava razporeditev v skupine in ocenjena z metodo...misls da je ta mera ok?
# za Silhueto sm tole nasel in jo ne pohvalijo kej prevec ker nej bi spet favorizirala k-means
# https://www.reddit.com/r/datascience/comments/yzir4v/is_it_reasonable_to_compare_different_clustering/?rdt=49702

# Neza28: to mava ze ta randov index sam da mava adjusted...
################################# grafični prikaz ##############################
library(tidyr)
library(dplyr)
resLong = pivot_longer(res, cols =matches("^(ari|wss|pwss)\\."),  values_to = "value",
                       names_to = c("metric", "method"), names_pattern = "^(ari|wss|pwss)\\.(kmeans|mclust)") 
resWide <- resLong %>% pivot_wider(names_from = metric, values_from = value) # da so ari, wss in pwss vsaka svoj column


resAgg = aggregate(cbind(ari, wss, pwss) ~ stevilo.spremenljivk + velikost.skupin + stevilo.skupin + method, 
                   data = resWide, FUN = mean)
# Tom30: dodal sem na desno stran se diff ker ga nisva se upostevala
resAgg_diff = aggregate(cbind(ari, wss, pwss) ~ stevilo.spremenljivk + velikost.skupin + stevilo.skupin + method + diff, 
                   data = resWide, FUN = mean)

# ======================= ari ================================

# vrednosti dava v factor zaradi risanja
resAggFac = resAgg
resAggFac$stevilo.spremenljivk = as.factor(resAggFac$stevilo.spremenljivk)
resAggFac$velikost.skupin = as.factor(resAggFac$velikost.skupin)
resAggFac$stevilo.skupin = as.factor(resAggFac$stevilo.skupin)

resAggFac_diff = resAgg_diff
resAggFac_diff$stevilo.spremenljivk = as.factor(resAggFac_diff$stevilo.spremenljivk)
resAggFac_diff$velikost.skupin = as.factor(resAggFac_diff$velikost.skupin)
resAggFac_diff$stevilo.skupin = as.factor(resAggFac_diff$stevilo.skupin)
resAggFac_diff$diff = as.factor(resAggFac_diff$diff)


library(ggplot2)
# risemo adjR2
ggplot(resAggFac, aes(y = ari, x = stevilo.spremenljivk, col=method, group=method)) + 
  geom_point() + geom_line() +
  facet_grid(stevilo.skupin ~ velikost.skupin, scales="free")

# na x osi je velikost skupin
ggplot(resAggFac, aes(y = ari, x = velikost.skupin, col=method, group=method)) + 
  geom_point() + geom_line() +
  facet_grid(stevilo.skupin ~ stevilo.spremenljivk, scales="free")


# ======================= wss ================================

ggplot(resAggFac, aes(y = wss, x = stevilo.spremenljivk, col=method, group=method)) + 
  geom_point() + geom_line() +
  facet_grid(stevilo.skupin ~ velikost.skupin, scales="free")


# ======================= pwss ===============================

ggplot(resAggFac, aes(y = pwss, x = stevilo.spremenljivk, col=method, group=method)) + 
  geom_point() + geom_line() +
  facet_grid(stevilo.skupin ~ velikost.skupin, scales="free")

