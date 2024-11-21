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
# # funkcija za generiranje podatkov
# 
# generiranje.podatkov = function(stevilo.spremenljivk, velikost.skupin, stevilo.skupin, diff){
#   # generiranje povprečij
#   M = diag(stevilo.skupin)*diff 
#   
#   # neinformativne spremenljivke - same 0
#   M = cbind(M, matrix(0, nrow=stevilo.skupin, ncol=stevilo.spremenljivk-stevilo.skupin)) 
#   S = diag(stevilo.spremenljivk)
#   
#   X = NULL 
#   # generamo podatke za vsako skupino posebaj
#   for(i in 1:stevilo.skupin){
#     iX = MASS::mvrnorm(n=velikost.skupin, mu = M[i,], Sigma = S)
#     X = rbind(X,iX)
#   }
#   
#   # dodamo se skupino
#   X = cbind(X, skupina=rep(1:stevilo.skupin,each=velikost.skupin)) # clu = skupina
#   return(X)
# }

source("generiranje_podatkov.R")
################################## simulacija ##################################
m = 100 # st. ponovitev

stevilo.neinformativnih.sprem.v = c(1, 4, 10)
velikost.skupin.v = c(20, 100, 200)
stevilo.skupin.v = c(4, 8, 14)
diff.v = c(1, 2, 4, 10)
cor.v = c(0, 0.2, 0.9)

# stevilo.skupin.v = c(4, 8, 10)
# velikost.skupin.v = c(20, 100, 200)
# stevilo.spremenljivk.v = c(12, 24, 36)
# diff.v = c(1, 2, 4, 10)

# # parametri za test
# m = 2
# stevilo.neinformativnih.sprem.v = 1
# velikost.skupin.v = 100
# stevilo.skupin.v = 4
# diff.v = 2
# cor.v = 0.2

settings = expand.grid(i=1:m, stevilo.neinformativnih.sprem = rev(stevilo.neinformativnih.sprem.v), 
                       velikost.skupin = rev(velikost.skupin.v), 
                       stevilo.skupin=rev(stevilo.skupin.v),
                       diff = rev(diff.v),
                       cor = rev(cor.v))

useOld = TRUE # ne uporabljal starega rezultata

if(useOld&&file.exists("simulacijaV3.RDS")){
  res = readRDS("simulacija.RDS")
}else{
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
  
  res2 = foreach(row = 1:nrow(settings), .combine=rbind) %dorng%{ 
    # row = 1
    # izberemo faktorje
    stevilo.neinformativnih.sprem = settings$stevilo.neinformativnih.sprem[row]
    stevilo.skupin = settings$stevilo.skupin[row]
    velikost.skupin = settings$velikost.skupin[row]
    diff = settings$diff[row]
    cor = settings$cor[row]
    
    # generiramo podatke
    data = generiranje.podatkov(velikost.skupin, 
                                stevilo.skupin, 
                                stevilo.neinformativnih.sprem, 
                                diff, 
                                cor)
    # transformacija v data.frame
    data = as.data.frame(data)
    # skaliramo podatke za metodo razvrscanje na polagi modelov
    data.scale = scale(data[, -ncol(data)])
    
    # metoda kmeans
    kmeans.res = kmeans(data[, -ncol(data)], centers=stevilo.skupin, nstart = 100) #, nstart=nRep) 
    ari.kmeans = blockmodeling::crand(data$skupina, kmeans.res$cluster) #ari
    
    # razvrscanje na polagi modelov
    suppressMessages({ # v konzoli se ne izpisuje fitting
      model = capture.output({
        mclust.res = mclust::Mclust(data.scale, G = stevilo.skupin) 
      })
    })
    # ari
    ari.mclust = blockmodeling::crand(data$skupina, mclust.res$classification) 
    
    # kje se nahaja zanka
    if(row%%10==0) cat("Iteration ", row, "/", nrow(settings), "complete! \n")
    
    return(c(ari.kmeans, ari.mclust))
    
  }
  res$ari.kmeans = res2[,1]
  res$ari.mclust = res2[,2]
  saveRDS(object = res, file="simulacijaV3.RDS")
  stopCluster(cl)
}

################################# grafični prikaz ##############################
res = readRDS("simulacijaV3.RDS")

# ----------------------------- priprava podatkov ------------------------------
resLong = pivot_longer(res, cols =matches("^(ari)\\."),
                       values_to = "value",
                       names_to = c("metric", "method"),
                       names_pattern = "^(ari)\\.(kmeans|mclust)") 
resLong$method[resLong$method == "kmeans"] = "metoda voditeljev"
resLong$method[resLong$method == "mclust"] = "razvrscanje na podlagi modelov"

resAgg = aggregate(value ~ stevilo.neinformativnih.sprem +
                     velikost.skupin + stevilo.skupin + method + diff + cor, 
                   data = resLong, FUN = mean)
# ce bova rabila se skupno stevilo spremneljivk
resAgg$skupaj_sprem = resAgg$stevilo.neinformativnih.sprem + resAgg$stevilo.skupin

resAgg_factor = resAgg
resAgg_factor$stevilo.neinformativnih.sprem = factor(resAgg_factor$stevilo.neinformativnih.sprem)
resAgg_factor$velikost.skupin = factor(resAgg_factor$velikost.skupin)
resAgg_factor$stevilo.skupin = factor(resAgg_factor$stevilo.skupin)
resAgg_factor$diff = factor(resAgg_factor$diff)
resAgg_factor$cor = factor(resAgg_factor$cor)
resAgg_factor$skupaj_sprem = factor(resAgg$skupaj_sprem)

# ----------------------------- grafi ----------------------------- 
## Prikaz ARI vrednosti diff=1

pod_diff1 = resAgg_factor %>% filter(diff == 1)

custom_labeller <- labeller(
  cor = function(x) paste("cor =", x),
  stevilo.neinformativnih.sprem = function(x) paste("st. neinfo. sprem. =", x)
)

ggplot(pod_diff1, aes(y = value, x = stevilo.skupin,
                      col=method, group=interaction(method, velikost.skupin), linetype=velikost.skupin)) + 
  geom_point() + geom_line() +
  scale_linetype_manual(values=c("dotted", "dashed", "solid"))+
  facet_grid(cor~stevilo.neinformativnih.sprem, scales="free", labeller = custom_labeller)+
  xlab("stevilo skupin") + 
  ylab("ARI") + 
  theme_minimal() + 
  labs(color = "Metoda:") 


## Prikaz ARI vrednosti diff=2

pod_diff2 = resAgg_factor %>% filter(diff == 2)

ggplot(pod_diff2, aes(y = value, x = stevilo.skupin,
                      col=method, group=interaction(method, velikost.skupin), linetype=velikost.skupin)) + 
  geom_point() + geom_line() +
  scale_linetype_manual(values=c("dotted", "dashed", "solid"))+
  facet_grid(cor~stevilo.neinformativnih.sprem, scales="free", labeller = custom_labeller)+
  xlab("stevilo skupin") + 
  ylab("ARI") + 
  theme_minimal() + 
  labs(color = "Metoda:") 


# glede na število neinfomrativnih spremenljivk
pod_diff2 = resAgg_factor %>% filter(diff == 2)

custom_labeller <- labeller(
  cor = function(x) paste("cor =", x),
  stevilo.skupin = function(x) paste("st. skupin =", x)
)

ggplot(pod_diff2, aes(y = value, x = stevilo.neinformativnih.sprem,
                      col=method, group=interaction(method, velikost.skupin), linetype=velikost.skupin)) + 
  geom_point() + geom_line() +
  scale_linetype_manual(values=c("dotted", "dashed", "solid"))+
  facet_grid(cor~stevilo.skupin, scales="free", labeller = custom_labeller)+
  xlab("stevilo neinformativnih spremenljivk") + 
  ylab("ARI") + 
  theme_minimal() + 
  labs(color = "Metoda:") 

## Prikaz ARI vrednosti diff=4

pod_diff4 = resAgg_factor %>% filter(diff == 4)

ggplot(pod_diff4, aes(y = value, x = stevilo.skupin,
                      col=method, group=interaction(method, velikost.skupin), linetype=velikost.skupin)) + 
  geom_point() + geom_line() +
  scale_linetype_manual(values=c("dotted", "dashed", "solid"))+
  facet_grid(cor~stevilo.neinformativnih.sprem, scales="free", labeller = custom_labeller)+
  xlab("stevilo skupin") + 
  ylab("ARI") + 
  theme_minimal() + 
  labs(color = "Metoda:") 

## Prikaz ARI vrednosti diff=10

pod_diff10 = resAgg_factor %>% filter(diff == 10)

ggplot(pod_diff10, aes(y = value, x = stevilo.skupin,
                       col=method, group=interaction(method, velikost.skupin), linetype=velikost.skupin)) + 
  geom_point() + geom_line() +
  scale_linetype_manual(values=c("dotted", "dashed", "solid"))+
  facet_grid(cor~stevilo.neinformativnih.sprem, scales="free", labeller = custom_labeller)+
  xlab("stevilo skupin") + 
  ylab("ARI") + 
  theme_minimal() + 
  labs(color = "Metoda:") 


################################# analiza ##############################
# head(res)

# ----------------------------- ANOVA -----------------------------
resLongF = resLong
# spremeniva v faktor
for(sprem in c("i", "stevilo.neinformativnih.sprem", "velikost.skupin", 
               "stevilo.skupin", "diff", "cor")){
  resLongF[[sprem]] = as.factor(resLongF[[sprem]])
}
# stolpec 'value' preimneujeva v 'ari'
names(resLongF) = sub('value', 'ari', names(resLongF))

useOld_aov = T # uporabi ze izracunane podatke
if(useOld_aov == F){
  aov_vred = aov(ari ~ stevilo.neinformativnih.sprem*velikost.skupin*stevilo.skupin*diff*cor*method,
                 data = resLongF)
  anova_vred_aov = anova(aov_vred) 
  saveRDS(object = anova_vred_aov, file="aov_vrednosti.RDS")
}
anova_vred_aov = readRDS("aov_vrednosti.RDS")

# prikaz v tabeli

kable((anova_vred_aov[1:(nrow(anova_vred_aov)-1),] %>%
         arrange(desc(`Sum Sq`))), align = "c",
      caption = "Prikaz rezultatov ANOVA testa.") %>%
  kable_styling(font_size = 7, latex_options = "HOLD_position")

# --------------------------- linearni mesani modeli ---------------------------
useOld_lmer = T # uporabi ze izracunane podatke
if(useOld_lmer == F){
  lmer_vred = lmer(ari ~ stevilo.neinformativnih.sprem*velikost.skupin*stevilo.skupin*diff*cor+method+(1|i),
                   data = resLongF)
  anova_vred_lmer = anova(lmer_vred) 
  saveRDS(object = anova_vred_lmer, file="lmer_vrednosti.RDS")
}
anova_vred_lmer = readRDS("lmer_vrednosti.RDS")

# prikaz v tabeli

kable((anova_vred_lmer[1:(nrow(anova_vred_lmer)-1),] %>%
         arrange(desc(`Sum Sq`))), align = "c",
      caption = "Prikaz rezultatov linearnih mešanih modelov.") %>%
  kable_styling(font_size = 7, latex_options = "HOLD_position")



