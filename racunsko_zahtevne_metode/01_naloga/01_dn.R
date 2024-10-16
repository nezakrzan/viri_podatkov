set.seed(2024)

st_ponovitev = 5000
velikost = c(15, 40, 60, 100, 200, 1000)

############################### p - vrednost ##################################
settings = expand.grid(i=1:st_ponovitev, n = velikost,
                       var = c("1,10,1", "1,5,10", "1,1,1"))

if(file.exists("p_vrednost.RDS")){
  res<-readRDS("p_vrednost.RDS")
} else {
  res<-cbind(settings, pANOVA=NA, pANOVA_2=NA, pMETHOD3=NA)
  for(row in 1:nrow(settings)){
    # dolocitev velikosti skupin in varianc
    velikost = settings$n[row]
    var_skupaj = as.numeric(strsplit(as.character(settings$var[row]),",")[[1]])
    sd1 = sqrt(var_skupaj[1])
    sd2 = sqrt(var_skupaj[2])
    sd3 = sqrt(var_skupaj[3])
    
    # generiranje skupin
    x<-c(rnorm(velikost, 0, sd1),
         rnorm(velikost, 0, sd2),
         rnorm(velikost, 0, sd3))
    gr<-factor(rep(1:3, times=c(velikost,velikost,velikost)))
    
    # classic ANOVA
    res$pANOVA[row] = summary(aov(x~gr))[[1]][["Pr(>F)"]][1]
    
    # Welch's ANOVA
    res$pANOVA_2[row] = oneway.test(x~gr, var.equal = FALSE)$p.value
    
    # Levenejev test
    p_test_enak_V = leveneTest(x~gr)$`Pr(>F)`[1]
    if(p_test_enak_V > 0.05){
      res$pMETHOD3[row] = summary(aov(x~gr))[[1]][["Pr(>F)"]][1]
    } else {
      res$pMETHOD3[row] = oneway.test(x~gr, var.equal = FALSE)$p.value
    }
  }
  # shranjevanje
  saveRDS(object = res, file="p_vrednost.RDS")
}

# risanje p-vrednost: Prikaz p-vrednosti Anderson-Darling testa.
resLong = pivot_longer(res, cols=matches("^p[AM]"), values_to = "pVal",
                       names_to = "method", names_prefix = "p")
resLong$method[resLong$method == "ANOVA"] = "classic ANOVA"
resLong$method[resLong$method == "ANOVA_2"] = "Welch's ANOVA"
resLong$method[resLong$method == "METHOD3"] = "Levene's test"

levels(resLong$var) = c("var = (1,10,1)", "var = (1,5,10)", "var = (1,1,1)")

# testirava za enakomerno porazdelitev z Anderson-Darling testom
valAnal = aggregate(pVal~ n + var + method , data = resLong,
                    function(x)ad.test(x, distr.fun = punif)$p.value)
valAnal$method[valAnal$method == "ANOVA"] = "classic ANOVA"
valAnal$method[valAnal$method == "ANOVA_2"] = "Welch's ANOVA"
valAnal$method[valAnal$method == "METHOD3"] = "Levene's test"

# graf
valAnal$n<-as.factor(valAnal$n)
ggplot(valAnal, aes(y=pVal, col=method, group=method, x=n))+
  geom_point()+geom_line()+
  geom_hline(yintercept = 0.05)+
  facet_wrap(vars(var))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ylab("p-vrednost") + 
  xlab("velikost skupin(n)") + 
  labs(title = " ",
       color = " ") + 
  theme_minimal() +
  theme(legend.position = "bottom")

############################### velikost testa ##################################
# risanje velikosti testa: Prikaz velikosti testa pri različnih parametrih.
valAnal = aggregate(pVal~ n + var + method , data = resLong,
                    function(x)mean(x<0.05))
valAnal$method[valAnal$method == "ANOVA"] = "classic ANOVA"
valAnal$method[valAnal$method == "ANOVA_2"] = "Welch's ANOVA"
valAnal$method[valAnal$method == "METHOD3"] = "Levene's test"

valAnal$n<-as.factor(valAnal$n)
ggplot(valAnal, aes(y=pVal, col=method, group=method, x=n))+
  geom_point()+geom_line()+#geom_smooth(se=F)+
  geom_hline(yintercept = 0.05)+
  facet_wrap(vars(var)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ylab("delez zavrnitve") +
  xlab("velikost skupin(n)") + 
  labs(title = " ",
       color = " ") + 
  theme_minimal() +
  theme(legend.position = "bottom")

################################# moc testa ####################################
### enake variance
povp = c("0,.5,0", "0,-0.5,0.5")
velikost = c(15, 40, 60, 100, 200, 1000)

settings = expand.grid(i=1:st_ponovitev, n = velikost, povp = povp,
                       var = "1,1,1")

if(file.exists("moc_enake.RDS")){
  res_p<-readRDS("moc_enake.RDS")
} else {
  res_p<-cbind(settings, pANOVA=NA, pANOVA_2=NA, pMETHOD3=NA)
  for(row in 1:nrow(settings)){
    # dolocitev velikost skupin, variance, povprecja
    velikost = settings$n[row]
    var_skupaj = as.numeric(strsplit(as.character(settings$var[row]),",")[[1]])
    sd1 = sqrt(var_skupaj[1])
    sd2 = sqrt(var_skupaj[2])
    sd3 = sqrt(var_skupaj[3])
    povp_skupaj = as.numeric(strsplit(as.character(settings$povp[row]),",")[[1]])
    povp1 = povp_skupaj[1]
    povp2 = povp_skupaj[2]
    povp3 = povp_skupaj[3]
    
    # generiranje vzorca
    x<-c(rnorm(velikost, povp1, sd1),
         rnorm(velikost, povp2, sd2),
         rnorm(velikost, povp3, sd3))
    gr<-factor(rep(1:3, times=c(velikost,velikost,velikost)))
    
    # classic ANOVA
    res_p$pANOVA[row] = summary(aov(x~gr))[[1]][["Pr(>F)"]][1]
    # Welch's ANOVA
    res_p$pANOVA_2[row] = oneway.test(x~gr, var.equal = FALSE)$p.value
    # Levene test
    p_test_enak_V = leveneTest(x~gr)$`Pr(>F)`[1]
    if(p_test_enak_V > 0.05){
      res_p$pMETHOD3[row] = summary(aov(x~gr))[[1]][["Pr(>F)"]][1]
    } else {
      res_p$pMETHOD3[row] = oneway.test(x~gr, var.equal = FALSE)$p.value
    }
  }
  # shranjevanje
  saveRDS(object = res_p, file="moc_enake.RDS")
}

# risanje moci enake var: Prikaz moči testa v primeru enakih varianc.
resLong_P = pivot_longer(res_p, cols=matches("^p[AM]"), values_to = "power",
                         names_to = "method", names_prefix = "p")
levels(resLong_P$povp) = c("povprecja = (0,.5,0)", "povprecja = (0,-0.5,0.5)")
levels(resLong_P$var) = c("var = (1,1,1)")

valAnal_P = aggregate(power ~ n + method + povp, data = resLong_P,
                      function(x)mean(x<0.05))
valAnal_P$method[valAnal_P$method == "ANOVA"] = "classic ANOVA"
valAnal_P$method[valAnal_P$method == "ANOVA_2"] = "Welch's ANOVA"
valAnal_P$method[valAnal_P$method == "METHOD3"] = "Levene's test"

# graf
valAnal_P$n<-as.factor(valAnal_P$n)
ggplot(valAnal_P, aes(y=power, col=method, group=method, x=n))+
  geom_point()+geom_line()+
  geom_hline(yintercept = 0.05)+
  facet_wrap(vars(povp))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ylab("moc") +
  xlab("velikost skupin(n)") + 
  labs(title = " ",
       color = " ") + 
  theme_minimal()

### razlicne variance
velikost = c(15, 40, 60, 100, 200, 1000)
settings = expand.grid(i=1:st_ponovitev, n = velikost, povp = povp,
                       var = c("1,10,1", "1,5,10"))

if(file.exists("moc_razlicne.RDS")){
  res_p<-readRDS("moc_razlicne.RDS")
} else {
  res_p<-cbind(settings, pANOVA_2=NA, pMETHOD3=NA)
  for(row in 1:nrow(settings)){
    # dolocitev velikosti skupin, varainc, povprecij
    velikost = settings$n[row]
    var_skupaj = as.numeric(strsplit(as.character(settings$var[row]),",")[[1]])
    sd1 = sqrt(var_skupaj[1])
    sd2 = sqrt(var_skupaj[2])
    sd3 = sqrt(var_skupaj[3])
    povp_skupaj = as.numeric(strsplit(as.character(settings$povp[row]),",")[[1]])
    povp1 = povp_skupaj[1]
    povp2 = povp_skupaj[2]
    povp3 = povp_skupaj[3]
    
    # generiranje vzorca
    x<-c(rnorm(velikost, povp1, sd1),
         rnorm(velikost, povp2, sd2),
         rnorm(velikost, povp3, sd3))
    gr<-factor(rep(1:3, times=c(velikost,velikost,velikost)))
    
    # Welch's ANOVA
    res_p$pANOVA_2[row] = oneway.test(x~gr, var.equal = FALSE)$p.value
    # Levens test
    p_test_enak_V = leveneTest(x~gr)$`Pr(>F)`[1]
    if(p_test_enak_V > 0.05){
      res_p$pMETHOD3[row] = summary(aov(x~gr))[[1]][["Pr(>F)"]][1]
    } else {
      res_p$pMETHOD3[row] = oneway.test(x~gr, var.equal = FALSE)$p.value
    }
  }
  # shranjevanje
  saveRDS(object = res_p, file="moc_razlicne.RDS")
}

# risanje moci razlicne var: "Prikaz moči testa v primeru različnih varianc(vrstice predstavljajo varinaco skupin, stolpci pa povprečje posamezne skupine)
resLong_P = pivot_longer(res_p, cols=matches("^p[AM]"), values_to = "power",
                         names_to = "method", names_prefix = "p")
levels(resLong_P$povp) = c("povprecja = (0,0.5,0)", "povprecja = (0,-0.5,0.5)")
levels(resLong_P$var) = c("var = (1,10,1)", "var = (1,5,10)")

valAnal_P = aggregate(power ~ n + var + method + povp, data = resLong_P,
                      function(x)mean(x<0.05))
valAnal_P$method[valAnal_P$method == "ANOVA"] = "classic ANOVA"
valAnal_P$method[valAnal_P$method == "ANOVA_2"] = "Welch's ANOVA"
valAnal_P$method[valAnal_P$method == "METHOD3"] = "Levene's test"

# graf
valAnal_P$n<-as.factor(valAnal_P$n)
ggplot(valAnal_P, aes(y=power, col=method, group=method, x=n))+
  geom_point()+geom_line()+
  geom_hline(yintercept = 0.05)+
  facet_grid(var~povp)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ylab("moč") +
  xlab("velikost skupin(n)") + 
  labs(title = " ",
       color = " ") + 
  theme_minimal() 


