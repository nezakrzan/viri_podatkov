set.seed(2024)

n = 1000
mean = 0
vars = c("1,1,1", "1,10,1", "1,5,10")

# en faktor je predvidevam variaca in enga si morva se zbrat
# lohka si isto zbereva velikost vzorca, tko kt smo delal na vajah za tretjo nalogo? 
p1V<-c(0.1, 1/4, 0.5) # velikost vzorca

# št. ponovitev simulacije
m = 10000
settings<-expand.grid(i=1:m,mean=mean, var=vars, p1=p1V)

### Simulacija
if(file.exists("dn1.RDS")){
  res<-readRDS("dn1.RDS")
} else {
  res<-cbind(settings, pClassANOVA=NA, pWelchANOVA=NA, pLevene=NA)
  set.seed(2024)
  for(row in 1:nrow(settings)){
    # dolocim parametre za vzorec
    #row=1 
    vars.split = as.numeric(strsplit(as.character(settings$var[row]),",")[[1]])
    var.sample1 = vars.split[1]
    var.sample2 = vars.split[2]
    var.sample3 = vars.split[3]
    p1 = settings$p1[row]
    
    # velikost vzorecev - tole lahko kako drugač ker ne vem če je okej...
    n1 = n*p1
    n2 = n*p1
    n3 = n - n1 - n2
    
    # vzorec
    sample = c(rnorm(n1, mean = mean, sd = sqrt(var.sample1)),
               rnorm(n2, mean = mean, sd = sqrt(var.sample2)),
               rnorm(n3, mean = mean, sd = sqrt(var.sample3)))
    # skupine k jih rabva za ANOVA test
    group = factor(rep(1:3, times=c(n1,n2,n3)))
    
    # classic ANOVA
    res$pClassANOVA[row] = summary(aov(sample ~ group))[[1]][["Pr(>F)"]][1]
    
    # Welch's ANOVA
    res$pWelchANOVA[row] = oneway.test(sample ~ group, var.equal = FALSE)$p.value
    
    # Levene test(kao za enake variance oz. tita 3. tocka k je ne razumem cit)
    p.Levene = leveneTest(sample ~ group)$`Pr(>F)`[1]
    # in ce je nad 0.05 potem so enake variance torej je classic anova, ce ne je pa welch?
    if(p.Levene > 0.05){
      res$pLevene[row] = summary(aov(sample ~ group))[[1]][["Pr(>F)"]][1]
    } else {
      res$pLevene[row] = oneway.test(sample ~ group, var.equal = FALSE)$p.value
    }
    
    if(row%%10000==0) cat("Iteration ", row, "/", nrow(settings), "complete! \n")
  }
  saveRDS(object = res, file="dn1.RDS")
}

#### Analiza ----
#resLong<-tidyr::pivot_longer(res, cols=matches("^p[FBL]"), values_to = "pVal",names_to = "method", names_prefix = "p")
#head(resLong)

resLong = data.frame(i = res$i, mean = res$mean, var = res$var, p1 = res$p1, 
                     pVal = c(res$pClassANOVA, res$pWelchANOVA, res$pLevene), 
                     method = c(rep("pClassANOVA", nrow(res)), rep("pWelchANOVA", nrow(res)), rep("pLevene", nrow(res))))

valAnal<-aggregate(pVal~ var + p1 + method , data = resLong, function(x)ADGofTest::ad.test(x, distr.fun = punif)$p.value)
head(valAnal)

resPRej<-aggregate(pVal~ var + p1 + method , data = resLong, function(x)mean(x<0.05)) # kolikokrat zavračamo
names(resPRej)[names(resPRej)=="pVal"]<-"pRej"
head(resPRej)

### Plotting
# p vrednost glede na x = velikost vzorca in variance
valAnal$var = as.factor(valAnal$var)
ggplot(valAnal, aes(y=pVal, col=method, group=method, x=p1))+
  geom_point()+geom_line()+
  geom_hline(yintercept = 0.05)+
  facet_wrap(vars(var))

# p vrednost glede na x = variance in velikost vzorcev
ggplot(valAnal, aes(y=pVal, col=method, group=method, x=var))+
  geom_point()+geom_line()+
  geom_hline(yintercept = 0.05)+
  facet_wrap(vars(p1))

# kolikokrat zavračamo ?? delež ?? 
resPRej$var = as.factor(resPRej$var)
ggplot(resPRej, aes(y=pRej, col=method, group=method, x=var))+
  geom_point()+geom_line()+
  geom_hline(yintercept = 0.05)+
  facet_wrap(vars(p1))






