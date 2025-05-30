# potrebne knjiznice
library(tidyr)
library(dplyr)
library(ggplot2)
library(car)
library(ADGofTest)
library(lmerTest)
library(knitr)
library(kableExtra)

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

# Primer generiranih podatkov za 4 skupine, velikosti n = 100, 12 spremenljivk in diff = 2.
data.primer1 = generiranje.podatkov(stevilo.spremenljivk = 12, velikost.skupin = 100, stevilo.skupin = 4, diff = 2)
pairs(data.primer1[,1:4], col=data.primer1[,13])

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
              ari.kmeans=NA, ari.mclust=NA)
  
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
    
    # razvrscanje na polagi modelov
    suppressMessages({ # v konzoli se ne izpisuje fitting
      model = capture.output({
        mclust.res = mclust::Mclust(data.scale, G = stevilo.skupin) 
      })
    })
    # ari
    res$ari.mclust[row] = blockmodeling::crand(data$skupina, mclust.res$classification) 
    
    # kje se nahaja zanka
    if(row%%10==0) cat("Iteration ", row, "/", nrow(settings), "complete! \n")
  }
  saveRDS(object = res, file="simulacija.RDS")
  stopCluster(cl)
}

################################# grafični prikaz ##############################
res = readRDS("simulacija.RDS")

# ----------------------------- priprava podatkov ------------------------------
resLong = pivot_longer(res, cols =matches("^(ari)\\."),
                       values_to = "value",
                       names_to = c("metric", "method"),
                       names_pattern = "^(ari)\\.(kmeans|mclust)") 
resLong$method[resLong$method == "kmeans"] = "metoda voditeljev"
resLong$method[resLong$method == "mclust"] = "razvrscanje na podlagi modelov"
resWide <- resLong %>% pivot_wider(names_from = metric, values_from = value)

# izlociva le vrednosti z diff=2, saj kasneje samo te opazujeva
resWide_diff2 = resWide[resWide$diff==2,]

resAgg = aggregate(ari ~ stevilo.spremenljivk + velikost.skupin 
                   + stevilo.skupin + method, 
                   data = resWide_diff2, FUN = mean)

resAggFac = resAgg
resAggFac$stevilo.spremenljivk = as.factor(resAggFac$stevilo.spremenljivk)
resAggFac$velikost.skupin = as.factor(resAggFac$velikost.skupin)
resAggFac$stevilo.skupin = as.factor(resAggFac$stevilo.skupin)

resAgg_diff = aggregate(ari ~ stevilo.spremenljivk +
                          velikost.skupin + stevilo.skupin + method + diff, 
                        data = resWide, FUN = mean)

resAggFac_diff = resAgg_diff
resAggFac_diff$stevilo.spremenljivk = as.factor(resAggFac_diff$stevilo.spremenljivk)
resAggFac_diff$velikost.skupin = as.factor(resAggFac_diff$velikost.skupin)
resAggFac_diff$stevilo.skupin = as.factor(resAggFac_diff$stevilo.skupin)
resAggFac_diff$diff = as.factor(resAggFac_diff$diff)

# ----------------------------- grafi ----------------------------- 
## Prikaz ARI vrednosti razdeljen glede na velikos in število skupin.
ggplot(resAggFac_diff, aes(y = ari, x = stevilo.spremenljivk,
                           col=method, group=interaction(method, diff), linetype=diff)) + 
  geom_point() + geom_line() +
  scale_linetype_manual(values=c("dotted", "dotdash", "dashed", "solid"))+
  facet_grid(stevilo.skupin ~ velikost.skupin, scales="free")+
  xlab("stevilo spremenljivk") + 
  ylab("ARI") + 
  theme_minimal() + 
  labs(color = "Metoda:") 

## Prikaz ARI vrednosti razdeljen glede na število spremenljivk in število skupin.
# izbor podatkov
resAggFac_diff_12 <- resAggFac_diff %>%
  filter(diff %in% c(1, 2))
# graf
ggplot(resAggFac_diff_12, aes(y = ari, x = velikost.skupin,
                              col=method, group=interaction(method, diff), linetype=diff)) + 
  geom_point() + geom_line() +
  scale_linetype_manual(values=c("dotted", "solid"))+
  facet_grid(stevilo.spremenljivk ~ stevilo.skupin, scales="free")+
  xlab("velikost skupin") + 
  ylab("ARI") + 
  theme_minimal() + 
  labs(color = "Metoda:")

## Prikaz ARI vrednosti razdeljen glede na število spremenljivk in število skupin.
# izbor podatkov
resAggFac_diff_24 <- resAggFac_diff %>%
  filter(diff %in% c(2, 4))
# graf
ggplot(resAggFac_diff_24, aes(y = ari, x = velikost.skupin,
                              col=method, group=interaction(method, diff), linetype=diff)) + 
  geom_point() + geom_line() +
  scale_linetype_manual(values=c("dotted", "solid"))+
  facet_grid(stevilo.spremenljivk ~ stevilo.skupin, scales="free")+
  xlab("velikost skupin") + 
  ylab("ARI") + 
  theme_minimal() + 
  labs(color = "Metoda:")

## Prikaz ARI vrednosti razdeljen glede na velikost skupin in število skupin.
ggplot(resAggFac, aes(y = ari, x = stevilo.spremenljivk,
                      col=method, group=method)) + 
  geom_point() + geom_line() +
  facet_grid(stevilo.skupin ~ velikost.skupin, scales="free") +
  xlab("stevilo spremenljivk") + 
  ylab("ARI") + 
  theme_minimal() + 
  labs(color = "Metoda:") + 
  theme(legend.position = "bottom")

## Prikaz ARI vrednosti razdeljen glede na število spremenljivk in število skupin.
ggplot(resAggFac, aes(y = ari, x = velikost.skupin,
                      col=method, group=method)) + 
  geom_point() + geom_line() +
  facet_grid(stevilo.skupin ~ stevilo.spremenljivk, scales="free") + 
  xlab("velikost skupin") + 
  ylab("ARI") + 
  theme_minimal() + 
  labs(color = "Metoda:") +
  theme(legend.position = "bottom")


################################# analiza ##############################
# head(res)

# ----------------------------- ANOVA -----------------------------
## Priprava podatkov
# iz long formata vzameva le vrstice ki vsebujejo ari vrednost
resLongF = as.data.frame(resLong[resLong$metric == "ari",])

# spremeniva v faktor
for(sprem in c("i", "stevilo.spremenljivk", "velikost.skupin", "stevilo.skupin", "diff")){
  resLongF[[sprem]] = as.factor(resLongF[[sprem]])
}
# stolpec 'value' preimneujeva v 'ari'
names(resLongF) = sub('value', 'ari', names(resLongF))

## ANOVA
# k-means anova
aov.kmeans = aov(ari ~ stevilo.spremenljivk*velikost.skupin*stevilo.skupin*diff,
                 data = resLongF[resLongF$method == "metoda voditeljev",])
anova.kmeans = anova(aov.kmeans) 

# mclust anova
aov.mclust = aov(ari ~ stevilo.spremenljivk*velikost.skupin*stevilo.skupin*diff,
                 data = resLongF[resLongF$method == "razvrscanje na podlagi modelov",])
anova.mclust = anova(aov.mclust)

## Prikaz statistične značilnosti spremenljivk za obe metodi.
df_anova = data.frame("faktorji" = row.names(anova.kmeans)[1:15],
                      "k-means" = anova.kmeans$`Pr(>F)`[1:15],
                      "mclust" = anova.mclust$`Pr(>F)`[1:15])
kable(df_anova, align = c("l","c", "c"),
      col.names = c("faktorji", "metoda voditeljev", "razvrščanje na podlagi modelov"),
      caption = "Prikaz statistične značilnosti spremenljivk za obe metodi.") %>% 
  row_spec(0,bold=T) %>%
  kable_styling(full_width = F, latex_options = "HOLD_position") 

## Primerjava modelov za ANOVA.
primerjava = anova(aov.mclust, aov.kmeans)
kable(data.frame("metoda" = c("razvrščanje na podlagi modelov", "metoda voditeljev"),
                 "Res.Df" = primerjava$Res.Df,
                 "RSS" = primerjava$RSS,
                 "Df" = primerjava$Df,
                 "SS" = primerjava$`Sum of Sq`), align = "c",
      caption = "Primerjava modelov za ANOVA.") %>% 
  row_spec(0,bold=T) %>%
  kable_styling(full_width = F, latex_options = "HOLD_position") 

# --------------------------- linearni mesani modeli ---------------------------
resF = res
# izracun linearnih mesanih modelov
for(sprem in c("i", "stevilo.spremenljivk", "velikost.skupin", "stevilo.skupin", "diff")){
  resF[[sprem]] = as.factor(resF[[sprem]])
}
lmer.kmeans = lmer(ari.kmeans ~ stevilo.spremenljivk*velikost.skupin*stevilo.skupin*diff+(1|i),
                   data = resF)
vred_kmeans = anova(lmer.kmeans)
lmer.mclust = lmer(ari.mclust ~ stevilo.spremenljivk*velikost.skupin*stevilo.skupin*diff+(1|i),
                   data = resF)
vred_mclust = anova(lmer.mclust)

## Prikaz statistične značilnosti spremenljivk za obe metodi.
df_lmer = data.frame("faktorji" = row.names(vred_kmeans),
                     "k-means" = vred_kmeans$`Pr(>F)`,
                     "mclust" = vred_mclust$`Pr(>F)`)
kable(df_lmer, align = c("l","c", "c"),
      col.names = c("faktorji", "metoda voditeljev", "razvrščanje na podlagi modelov"),
      caption = "Prikaz statistične značilnosti spremenljivk za obe metodi.") %>% 
  row_spec(0,bold=T) %>%
  kable_styling(full_width = F, latex_options = "HOLD_position") 

## Primerjava modelov.
primerjava.lmer = anova(lmer.kmeans, lmer.mclust)

kable(data.frame("metoda" = c("metoda voditeljev", "razvrscanje na podlagi modelov"),
                 "npar" = primerjava.lmer$npar,
                 "AIC" = primerjava.lmer$AIC,
                 "BIC" = primerjava.lmer$BIC,
                 "logLik" = primerjava.lmer$logLik,
                 "deviance" = primerjava.lmer$deviance), align = "c",
      caption = "Primerjava modelov") %>% 
  row_spec(0,bold=T) %>%
  kable_styling(full_width = F, latex_options = "HOLD_position") 

