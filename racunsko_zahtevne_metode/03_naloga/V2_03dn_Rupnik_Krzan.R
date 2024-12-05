set.seed(2024)

library(ggplot2)
library(car)
library(tidyr)
library(dplyr)
library(knitr)
library(kableExtra)
library(glue)

# =============================== faktorji ====================================
alpha.v = c(0.6, 1, 1.2)
n.v = c(20, 200, 500)
st.ponovitev = 10000

# bootstrap vzorci
m = 1000

useOld = T # ne uporabljal starega rezultata

# ============================ simulacija podatkov ============================
# funkcija za generiranje podatkov
generiranje_podatkov  = function(beta0, beta1, beta2, alpha, n){
  # generiranje vrednosti za x1 in x2
  x1  = runif(n, 20, 60)
  x2  = runif(n, 2, 10)
  
  # generiranje napake
  epsilon  = rnorm(n, 0, x1^alpha) # dodava heteroskedasticnost
  
  # izračun napovedne spremenljivke za model (parametri = vhodni argumenti) 
  y  = beta0 + beta1 * x1 + beta2 * x2 + epsilon
  
  # podatke spravimo v podatkovni okvir in vrnemo kot rezultat funkcije
  data.frame(x1 = x1, x2 = x2, y = y) 
}

# ============================ funkcija za bootstrap ============================
# funkcija za bootstrap
bootstrap = function(m, podatki, alpha, n){
  # shranjevanje rezultatov
  res = matrix(NA, nrow = m, ncol = 5)
  colnames(res) = c("alpha", "velikost.vzorca", "int", "x1", "x2")
  ind_res = 1
  for(j in 1:m){ # bootstrap
    ind = sample(n, replace = T)
    
    # lm model
    iFitLm = lm(y ~ x1 + x2, data=podatki[ind,])
    iEst = summary(iFitLm)$coef
    
    # shranimo rezultate
    res[ind_res, 1:2] = c(alpha, n)
    res[ind_res, 3:5] = iEst[,1]
    
    ind_res = ind_res + 1
  }
  return(res)
}

# ================================ simulacija ==================================
settings = expand.grid(i = 1:st.ponovitev, alpha = rev(alpha.v), n=rev(n.v))
alpha.iz = 0.05

if(useOld&&file.exists("bootstrapV2.RDS")&&file.exists("klasicna_metodaV2.RDS")){
  rezultati_bootstrap = readRDS("bootstrapV2.RDS") 
  rezultati_klasicna = readRDS("klasicna_metodaV2.RDS")
}else{
  library(foreach)
  library(doParallel)
  library(doRNG)
  # parallel computing
  nc = detectCores()-1 
  cl = makeCluster(nc, outfile="log_skupaj") # shranjujemo konzolo
  registerDoParallel(cl)
  
  set.seed(2024)
  
  res2 = foreach(i = 1:nrow(settings), .combine=rbind) %dorng%{ 
    # generiranje podatkov
    alpha = settings$alpha[i]
    n = settings$n[i]
    data = generiranje_podatkov(beta0=100, beta1=3, beta2=2, alpha=alpha, n=n)
    
    ## bootstrap
    res = bootstrap(m, data, alpha, n)
    
    # izračun intervalov zaupanja
    int.iz  = quantile(res[,3], probs = c(alpha.iz/2, 1-alpha.iz/2))
    x1.iz  = quantile(res[,4], probs = c(alpha.iz/2, 1-alpha.iz/2))
    x2.iz  = quantile(res[,5], probs = c(alpha.iz/2, 1-alpha.iz/2))
    
    IZ_bootstrap = data.frame(alpha=alpha, velikost.vzorca = n,
                              int.coef = mean(res[,3]), x1.coef = mean(res[,4]), x2.coef = mean(res[,5]),
                              int.lower = int.iz[[1]], int.upper = int.iz[[2]],
                              x1.lower = x1.iz[[1]], x1.upper = x1.iz[[2]],
                              x2.lower = x2.iz[[1]], x2.upper =x2.iz[[2]])
    
    ## klasicna metoda
    # model
    model = lm(y ~ x1 + x2, data)  
    # koeficienti
    int = summary(model)$coef[1,1]
    x1 = summary(model)$coef[2,1]
    x2 = summary(model)$coef[3,1]
    # intervali zaupanja
    int.iz = confint(model)[1,]
    x1.iz = confint(model)[2,]
    x2.iz = confint(model)[3,]
    # shranjevanj
    intervali.zaupanja = data.frame(alpha = alpha, velikost.vzorca = n,
                                    int.coef = int, x1.coef = x1, x2.coef = x2,
                                    int.lower = int.iz[1], int.upper = int.iz[2],
                                    x1.lower = x1.iz[1], x1.upper = x1.iz[2],
                                    x2.lower = x2.iz[1], x2.upper = x2.iz[2])
    
    # kje se nahaja zanka
    if(i%%100==0) cat("Iteration ", i, "/", nrow(settings), "complete! \n")
    
    skupaj = cbind(IZ_bootstrap, intervali.zaupanja)
    
    return(skupaj)
    
  }
  rezultati_boot = data.frame(alpha = res2[,1], velikost.vzorca = res2[,2], 
                         int.coef = res2[,3], x1.coef = res2[,4], x2.coef = res2[,5],
                         int.lower = res2[,6], int.upper = res2[,7],
                         x1.lower = res2[,8], x1.upper = res2[,9],
                         x2.lower = res2[,10], x2.upper = res2[,11])
  
  rezultati_klasicna = data.frame(alpha = res2[,12], velikost.vzorca = res2[,13], 
                                  int.coef = res2[,14], x1.coef = res2[,15], x2.coef = res2[,16],
                                  int.lower = res2[,17], int.upper = res2[,18],
                                  x1.lower = res2[,19], x1.upper = res2[,20],
                                  x2.lower = res2[,21], x2.upper = res2[,22])
  # Shranjevanje rezultatov
  saveRDS(object = rezultati_boot, file = "bootstrapV2.RDS")
  saveRDS(object = rezultati_klasicna, file = "klasicna_metodaV2.RDS")
  
  stopCluster(cl)
}

rezultati_bootstrap = readRDS("V2_bootstrap.RDS")
rezultati_klasicna = readRDS("V2_klasicna_metodaV2.RDS")

# -------------------------- graficni prikazi ----------------------------------
# zdruziva rezultate
podatki_skupaj = rbind(rezultati_klasicna, rezultati_bootstrap)
# dodava metodo
podatki_skupaj = cbind(podatki_skupaj, rep(c("klasicna analiza(LR)", "bootstrap"), each=nrow(rezultati_klasicna)))
colnames(podatki_skupaj)[ncol(podatki_skupaj)] = "metoda"

IZ_skupaj = podatki_skupaj %>% group_by(alpha, velikost.vzorca, metoda) %>%
  summarise(int.coef = mean(int.coef), x1.coef = mean(x1.coef), x2.coef = mean(x2.coef),
            int.lower = mean(int.lower), int.upper = mean(int.upper),
            x1.lower = mean(x1.lower), x1.upper = mean(x1.upper),
            x2.lower = mean(x2.lower), x2.upper = mean(x2.upper))

IZ_skupaj$alpha = factor(IZ_skupaj$alpha)
IZ_skupaj$velikost.vzorca = factor(IZ_skupaj$velikost.vzorca)

# sirine intervalov
sirine_intervalov = IZ_skupaj %>%
  mutate(SI_int = abs(int.upper) - abs(int.lower), SI_x1 = abs(x1.upper) - abs(x1.lower), SI_x2 = abs(x2.upper) - x2.lower) %>%
  select(alpha, velikost.vzorca, SI_int, SI_x1, SI_x2, metoda)
sirine_intervalov = cbind(sirine_intervalov, podatki = rep(c("klasicna analiza(LR)", "bootstrap"), each=9))
sirine_intervalov$velikost.vzorca = factor(sirine_intervalov$velikost.vzorca)

# Grafi intervalov zaupanja(levo) in širine intervalov zaupanja(desno) za prosti koeficient(Intercept).
custom_labeller <- labeller(
  alpha = function(x) paste("\u03b1 =", x)
)

tabela_risanje = IZ_skupaj %>%
  inner_join(sirine_intervalov, by = c("alpha", "velikost.vzorca", "metoda"))
tabela_risanje$metoda <- factor(tabela_risanje$metoda)

g1 = ggplot(tabela_risanje, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_errorbar(aes(ymin = int.lower, ymax = int.upper), width=0.5, position = position_dodge2(reverse = TRUE, 0.3)) + 
  geom_point(aes(y = int.coef), position = position_dodge2(reverse = TRUE, 0.5), shape = 16, size = 1) +
  facet_grid(forcats::fct_inorder(glue('alpha*" = {alpha}"')) ~., labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ") +
  theme_minimal() + theme(legend.position = "none") +
  labs(color = "metoda")

g2 = ggplot(tabela_risanje, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_point(aes(y = SI_int), shape = 16, size = 1.2) +
  geom_line(aes(y = SI_int)) +
  facet_grid(forcats::fct_inorder(glue('alpha*" = {alpha}"')) ~., labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ") +
  theme_minimal() + theme(legend.position = "right") +
  labs(color = "metoda")
library(patchwork)
combined_plot = g1 + g2 + plot_layout(guides = "collect")
combined_plot

# Grafi intervalov zaupanja(levo) in širine intervalov zaupanja(desno) za koeficient pri x1(beta1).
g1 = ggplot(tabela_risanje, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_errorbar(aes(ymin = x1.lower, ymax = x1.upper), width=0.5, position = position_dodge2(reverse = TRUE, 0.3)) + 
  geom_point(aes(y = x1.coef), position = position_dodge2(reverse = TRUE, 0.5), shape = 16, size = 1) +
  facet_grid(forcats::fct_inorder(glue('alpha*" = {alpha}"')) ~., labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ") +
  theme_minimal() + theme(legend.position = "none") +
  labs(color = "metoda")

g2 = ggplot(tabela_risanje, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_point(aes(y = SI_x1), shape = 16, size = 1.2) +
  geom_line(aes(y = SI_x1)) +
  facet_grid(forcats::fct_inorder(glue('alpha*" = {alpha}"')) ~., labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ") +
  theme_minimal() + theme(legend.position = "right") +
  labs(color = "metoda")
library(patchwork)
combined_plot = g1 + g2 + plot_layout(guides = "collect")
combined_plot

# Grafi intervalov zaupanja(levo) in širine intervalov zaupanja(desno) za koeficient pri x2(beta2).
g1 = ggplot(tabela_risanje, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_errorbar(aes(ymin = x2.lower, ymax = x2.upper), width=0.5, position = position_dodge2(reverse = TRUE, 0.3)) + 
  geom_point(aes(y = x2.coef), position = position_dodge2(reverse = TRUE, 0.5), shape = 16, size = 1) +
  facet_grid(forcats::fct_inorder(glue('alpha*" = {alpha}"')) ~., labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ") +
  theme_minimal() + theme(legend.position = "none") +
  labs(color = "metoda")

g2 = ggplot(tabela_risanje, aes(x=velikost.vzorca, col=metoda, group=metoda)) +
  geom_point(aes(y = SI_x2), shape = 16, size = 1.2) +
  geom_line(aes(y = SI_x2)) +
  facet_grid(forcats::fct_inorder(glue('alpha*" = {alpha}"')) ~., labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ") +
  theme_minimal() + theme(legend.position = "right") +
  labs(color = "metoda")
library(patchwork)
combined_plot = g1 + g2 + plot_layout(guides = "collect")
combined_plot

# pokritost izracun
pokritost = podatki_skupaj %>% group_by(alpha, velikost.vzorca, metoda) %>%
  summarise(pokritost_int = mean(int.lower <= 100 & 100 <= int.upper),
            pokritost_x1 = mean(x1.lower <= 3 & 3 <= x1.upper),
            pokritost_x2 = mean(x2.lower <= 2 & 2 <= x2.upper))

pokritost_bootstrap = pokritost %>% filter(metoda == "bootstrap")
pokritost_klasicna = pokritost %>% filter(metoda == "klasicna analiza(LR)")

df_pokritost = data.frame("alpha" = pokritost_bootstrap$alpha, 
                          "n" = pokritost_bootstrap$velikost.vzorca,
                          "int_klasicna" = pokritost_klasicna$pokritost_int,
                          "int_bootstrap" = pokritost_bootstrap$pokritost_int,
                          "x1_klasicna" = pokritost_klasicna$pokritost_x1,
                          "x1_bootstrap" = pokritost_bootstrap$pokritost_x1,
                          "x2_klasicna" = pokritost_klasicna$pokritost_x2,
                          "x2_bootstrap" = pokritost_bootstrap$pokritost_x2)

kable(df_pokritost, 
      col.names = c("alpha", "velikost vzorca", rep(c("klasicna", "bootstrap"),3)),
      align = "c", digits = 4, caption = "Pokritost pri posameznih koeficientih in kombinacijah alphe in velikosti vzorca(n).") %>%
  add_header_above(c(" " = 2, "intercept" = 2, "beta1" = 2, "beta2" = 2))

# Graf pokritosti za vse tri koeficiente.
pokritostLong = pivot_longer(pokritost, cols =matches("^(pokritost)."),
                       values_to = "value",
                       names_to = c("metric"),
                       names_pattern = ".(int|x1|x2)") 

pokritostLong$metric = factor(pokritostLong$metric, levels = c("int", "x1", "x2"),
                              labels = c("Int", "beta1", "beta2"))

ggplot(pokritostLong, aes(x = factor(velikost.vzorca), y = value, col=metoda, group=metoda)) +
  geom_line() + geom_hline(yintercept = 0.95, size = 0.15) +
  geom_point() +
  # facet_grid(alpha~metric, scales="free", labeller = custom_labeller) +
  facet_grid(forcats::fct_inorder(glue('alpha*" = {alpha}"')) ~ metric, labeller = label_parsed, scales="free") +
  labs(x = "velikost vzorca (n)", y = " ") +
  theme(legend.position = "bottom") +
  theme_minimal()