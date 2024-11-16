## stara koda - en del:
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

