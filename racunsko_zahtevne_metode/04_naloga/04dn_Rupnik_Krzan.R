# https://github.com/ashishpatel26/Pima-Indians-Diabetes-Dataset-Missing-Value-Imputation/blob/master/Readme.md
# https://www.kaggle.com/code/faelk8/diabetes-eda-missing-values
# https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database

library(psych)
library(ggplot2)
library(tidyr)
library(dplyr)
library(knitr)
library(kableExtra)
library(multiUS)
library(mice)
library(arm)
library(randomForest)

# ============================== uvoz podatkov =================================

data = read.csv("pima_indians_diabetes_dataset.csv", header = TRUE, sep = ",", na.strings = c(""))
#summary(data)

# pregled mankajočih podatkov
round(colMeans(is.na(data)) * 100, 1)
round(mean(!complete.cases(data)) * 100, 1)
missing_data = (colSums(is.na(data)) / nrow(data)) * 100 # koliko % podatkov manka po stolpcih
kable(data.frame(missing_data),
      col.names = c("spremenljivka", "%"),
      align = "c", digits = 2,
      caption = "Odstotek mankajočih vrednosti pri posamezni spremenljivki.")

# ============================== pregled podatkov ==============================
kable(describe(data),
      align = "c", digits = 2,
      caption = "Opisne statistike podatkov.")

# Porazdelitev spremenljivke v podatkovnem nizu, kjer je  Class ciljna spremenljivka, ki označuje, ali ima posameznica diabetes (1) ali ne (0).
data$Class = as.factor(data$Class)
g1 = ggplot(data, aes(x=Pregnant)) + 
  geom_density() + 
  theme_bw()+
  labs(y = "gostota") 
g2 = ggplot(data, aes(x=Pregnant, fill=Class)) + 
  geom_density(alpha=0.4) + 
  theme_bw()+
  labs(y = "gostota") 
gridExtra::grid.arrange(g1, g2, ncol=2, top = paste("spremenljivka Pregnant"))

g1 = ggplot(data, aes(x=Glucose)) + 
  geom_density() + 
  theme_bw()+
  labs(y = "gostota") 
g2 = ggplot(data, aes(x=Glucose, fill=Class)) + 
  geom_density(alpha=0.4) + 
  theme_bw()+
  labs(y = "gostota") 
gridExtra::grid.arrange(g1, g2, ncol=2, top = paste("spremenljivka Glucose"))

g1 = ggplot(data, aes(x=BloodPressure)) + 
  geom_density() + 
  theme_bw()+
  labs(y = "gostota") 
g2 = ggplot(data, aes(x=BloodPressure, fill=Class)) + 
  geom_density(alpha=0.4) + 
  theme_bw()+
  labs(y = "gostota") 
gridExtra::grid.arrange(g1, g2, ncol=2, top = paste("spremenljivka BloodPressure"))

g1 = ggplot(data, aes(x=SkinThickness)) + 
  geom_density() + 
  theme_bw()+
  labs(y = "gostota") 
g2 = ggplot(data, aes(x=SkinThickness, fill=Class)) + 
  geom_density(alpha=0.4) + 
  theme_bw()+
  labs(y = "gostota") 
gridExtra::grid.arrange(g1, g2, ncol=2, top = paste("spremenljivka SkinThickness"))

g1 = ggplot(data, aes(x=Insulin)) + 
  geom_density() + 
  theme_bw()+
  labs(y = "gostota") 
g2 = ggplot(data, aes(x=Insulin, fill=Class)) + 
  geom_density(alpha=0.4) + 
  theme_bw()+
  labs(y = "gostota") 
gridExtra::grid.arrange(g1, g2, ncol=2, top = paste("spremenljivka Insulin"))

g1 = ggplot(data, aes(x=BMI)) + 
  geom_density() + 
  theme_bw()+
  labs(y = "gostota") 
g2 = ggplot(data, aes(x=BMI, fill=Class)) + 
  geom_density(alpha=0.4) + 
  theme_bw()+
  labs(y = "gostota") 
gridExtra::grid.arrange(g1, g2, ncol=2, top = paste("spremenljivka BMI"))

g1 = ggplot(data, aes(x=DiabetesPedigree)) + 
  geom_density() + 
  theme_bw()+
  labs(y = "gostota") 
g2 = ggplot(data, aes(x=DiabetesPedigree, fill=Class)) + 
  geom_density(alpha=0.4) + 
  theme_bw()+
  labs(y = "gostota") 
gridExtra::grid.arrange(g1, g2, ncol=2, top = paste("spremenljivka DiabetesPedigree"))

g1 = ggplot(data, aes(x=Age)) + 
  geom_density() + 
  theme_bw()+
  labs(y = "gostota") 
g2 = ggplot(data, aes(x=Age, fill=Class)) + 
  geom_density(alpha=0.4) + 
  theme_bw()+
  labs(y = "gostota") 
gridExtra::grid.arrange(g1, g2, ncol=2, top = paste("spremenljivka Age"))

# ====================== mehanizmi mankajocih vrednosti ========================
library(mice)
library(norm)
library(mix)
library(mitools)
library(foreign)
library(VIM)
library(psych) #describe
library(multiUS)
library(blockmodeling)
library(arm) #coefplot
library(modelsummary) #modelplot

# -------------------- pregled vzorcev mankajocih vrednosti --------------------
matrixplot(data) # nimamo razporejenih
# Ker je večina mankajočih vrednostih pri spremenljivkah SkinThickness in Insulin, nas zanimajo razvrstitve glede na druge spremenljivke
matrixplot(data, sortby="SkinThickness") # ce manka SkinThickness pol manka tud Insulin 
matrixplot(data, sortby="Insulin") # Ce je insulin potem mamo vse podatke, razn dveh izjem

matrixplot(data, sortby="Pregnant") # Večje število nosečnosti, večja verjetnost mankajočega podatka pri Insuline oziroma pri nekaterih vrednostih Pregnant tudi mankajoča vrednost Insulin(imava kr neki rdečih "kvadratkov"), mogoce tudi SkinThickness 
matrixplot(data, sortby="Glucose") # tud kle nc ne vidm
matrixplot(data, sortby="BloodPressure") # aha neki vidm, ce manka BloodPressure, pol manka tud SkinThickness pa Insulin(mogoce to mer ena oseba pa je en dan ni blo v sluzbo :))
matrixplot(data, sortby="BMI") # ce ni BMI potem obstaja velika verjetnost, da imamo mankajočo vrednost tudi pri spremenljivkah Insulin, SkinThickness, BloodPressure
matrixplot(data, sortby="DiabetesPedigree") # mogoče se mi zdi: pri manjših vrednostih spremenljivke DiabetesPedigree več mankajočih vrednosti pri spremenljivki Insulin
matrixplot(data, sortby="Age") # Pri visoki starosti večja verjetnost mankajočih vrednosti pri spremenljivkah Insulin in SkinThickness
matrixplot(data, sortby="Class") # ni vzorca

# Korelacije med mankajočimi vrednostmi
isMiss = is.na(data[,2:6])
missCor = cor(isMiss)
rownames(missCor) = colnames(missCor) = colnames(data)
missCor[is.na(missCor)] = 0
plotMat(missCor)

# ------------------------- Test MAR (Missing At Random) -----------------------
### mankajoca vrednost pri SkinThickness potem tudi pri Insulin
#   Testira se, ali obstaja statistično pomembna razlika v povprečju SkinThickness 
#   med tistimi, ki imajo manjkajoče vrednosti za Insulin, in tistimi, ki nimajo manjkajočih 
#   vrednosti za Insulin.
# H0: Povprečje SkinThickness v obeh skupinah (tistih, ki imajo podatke za Insulin, in tistih, ki nimajo podatkov za Insulin) so enaka. 
t.test(data$SkinThickness ~ is.na(data$Insulin))
# Rezultati:
#   - t = -0.171: vrednost zelo blizu 0, kar pomeni, da so povprečja obeh skupin skoraj enaka
#   - p-vrednost = 0.863 > 0.05: statistično neznačilno, ničelne domneve ne zavračamo, 
#   - odsotnost podatkov spremenljivke Insulin ni statistično povezana z vrednostmi SkinThickness
#   - Manjkajoči podatki za Insulin so lahko MCAR (Missing Completely At Random), saj ni sistematične povezave med manjkajočimi vrednostmi za Insulin in debelino kože (SkinThickness)

### Insulin
t.test(data$Pregnant ~ is.na(data$Insulin))
# Rezultati: 
#   - p - vrednost < 0.05: izredno majhna, kar pomeni, da lahko zavračamo ničelno hipotezo
#   - sklepamo, da manjkajoče vrednosti za Insulin niso MCAR (Missing Completely At Random). 
#   - Manjkajoči podatki za Insulin so lahko posledica MAR (Missing At Random) ali celo MNAR (Missing Not At Random), saj je njihova prisotnost povezana z značilnostjo opazovanih podatkov (v tem primeru število nosečnosti).
t.test(data$BMI ~ is.na(data$Insulin))
# Rezultati: 
#   - p - vrednost = 0.01 < 0.05: pomeni, da lahko zavračamo ničelno hipotezo
#   - sklepamo, da manjkajoče vrednosti za Insulin niso MCAR (Missing Completely At Random). 
#   - Prisotnost manjkajočih podatkov za Insulin je lahko povezana z vrednostjo indeksa telesne mase (MAR - Missing At Random)
t.test(data$Age ~ is.na(data$Insulin))
# Rezultati: 
#   - p - vrednost  < 0.05: pomeni, da lahko zavračamo ničelno hipotezo
#   - sklepamo, da manjkajoče vrednosti za Insulin niso MCAR (Missing Completely At Random). 
#   - Prisotnost manjkajočih podatkov za Insulin je verjetno povezana s starostjo in bi lahko bila označena kot MAR
t.test(data$BloodPressure ~ is.na(data$Insulin))
# Rezultati: 
#   - p - vrednost  < 0.05: pomeni, da lahko zavračamo ničelno hipotezo
#   - sklepamo, da manjkajoče vrednosti za Insulin niso MCAR (Missing Completely At Random). 
#   - Prisotnost manjkajočih podatkov za Insulin je verjetno povezana z BloodPressure in bi lahko bila označena kot MAR
t.test(data$DiabetesPedigree ~ is.na(data$Insulin))
# Rezultati: 
#   - p - vrednost  < 0.05: pomeni, da lahko zavračamo ničelno hipotezo
#   - obstaja statistično značilna razlika v povprečju DiabetesPedigree med obema skupinama (skupina, kjer so podatki za Insulin manjkajoči in skupina, kjer niso manjkajoči).
#   - Rezultati nakazujejo, da obstaja povezanost med manjkajočimi podatki za Insulin in vrednostmi DiabetesPedigree. Manjkajoči podatki za Insulin so v tem primeru povezani z nekoliko nižjimi vrednostmi DiabetesPedigree. 
#   - To bi lahko nakazovalo, da manjkajoči podatki niso naključni in da so lahko povezani z nekaterimi drugimi značilnostmi

### SkinThickness
t.test(data$BMI  ~ is.na(data$SkinThickness))
# Rezultati: 
#   - p - vrednost < 0.05: majhna, kar pomeni, da lahko zavračamo ničelno hipotezo
#   - test kaže, da obstaja statistično značilna razlika v povprečnem BMI med obema skupinama
#   - Manjkajoči podatki za SkinThickness najverjetneje niso popolnoma naključni (MCAR), saj je prisotnost manjkajočih podatkov povezana z BMI. 
t.test(data$Pregnant ~ is.na(data$SkinThickness))
# Rezultati: 
#   - p - vrednost < 0.05: izredno majhna, kar pomeni, da lahko zavračamo ničelno hipotezo
#   - obstaja statistično značilna razlika v povprečju Pregnant med obema skupinama (skupina, kjer so podatki za SkinThickness manjkajoči in skupina, kjer niso manjkajoči).
#   - Ta rezultat nakazuje, da odsotnost podatkov v spremenljivki SkinThickness morda je missing at random (MAR), saj je povezana z vrednostmi v spremenljivki Pregnant.
t.test(data$BloodPressure ~ is.na(data$SkinThickness))
# Rezultati: 
#   - p - vrednost < 0.05: izredno majhna, kar pomeni, da lahko zavračamo ničelno hipotezo
#   - Manjkajoči podatki za SkinThickness so v tem primeru povezani z višjim krvnim tlakom, saj so povprečne vrednosti v skupini z manjkajočimi podatki višje. 
#   - To bi lahko pomenilo, da manjkajoči podatki za SkinThickness niso naključni.
t.test(data$DiabetesPedigree ~ is.na(data$SkinThickness))
# Rezultati: 
#   - p - vrednost < 0.05: izredno majhna, kar pomeni, da lahko zavračamo ničelno hipotezo
#   - Manjkajoči podatki za SkinThickness so v tem primeru povezani z nekoliko nižjimi vrednostmi DiabetesPedigree. 
#   - To bi lahko nakazovalo, da manjkajoči podatki niso naključni in da so lahko povezani z drugimi dejavniki, kot so nižje vrednosti DiabetesPedigree pri tistih, ki nimajo podatkov za SkinThickness.
t.test(data$Age ~ is.na(data$SkinThickness))
# Rezultati: 
#   - p - vrednost < 0.05: izredno majhna, kar pomeni, da lahko zavračamo ničelno hipotezo
#   - Manjkajoči podatki za SkinThickness so v tem primeru povezani z višjo povprečno starostjo, saj so povprečne vrednosti v skupini z manjkajočimi podatki višje. 
#   - To bi lahko pomenilo, da manjkajoči podatki niso naključni in da so lahko povezani z drugimi dejavniki, kot so starost, saj so starejši ljudje morda manj verjetno vključeni v zbiranje podatkov za SkinThickness.

### BloodPressure
t.test(data$BMI ~ is.na(data$BloodPressure))
# Rezultati: 
#   - p - vrednost = 0.827 > 0.05: ne moremo zavrniti ničelne hipoteze
#   - Manjkajoči podatki za BloodPressure verjetno naključni in ni pomembnih razlik v povprečnih vrednostih BMI med posamezniki, pri katerih manjkajo podatki za BloodPressure, in tistimi, pri katerih so ti podatki prisotni
t.test(data$SkinThickness ~ is.na(data$BloodPressure))
# Rezultati: 
#   - p - vrednost = 0.58 > 0.05: ne moremo zavrniti ničelne hipoteze
#   - Manjkajoči podatki za BloodPressure verjetno naključni in da ni pomembnih razlik v povprečnih vrednostih debeline kože (SkinThickness) med posamezniki, pri katerih manjkajo podatki za BloodPressure, in tistimi, pri katerih so ti podatki prisotni.
t.test(data$Age ~ is.na(data$BloodPressure)) # zavračamo H0

### Glucose
t.test(data$Glucose ~ is.na(data$SkinThickness))
t.test(data$Glucose ~ is.na(data$Insulin))

# BMI
t.test(data$BMI ~ is.na(data$Glucose))
t.test(data$BMI ~ is.na(data$BloodPressure))
t.test(data$BMI ~ is.na(data$Insulin))
t.test(data$SkinThickness ~ is.na(data$BMI))
# Rezultati: 
#   - p - vrednost < 0.05: izredno majhna, kar pomeni, da lahko zavračamo ničelno hipotezo
#   - obstaja statistično značilna razlika v povprečju SkinThickness med obema skupinama (skupina, kjer so podatki za BMI manjkajoči in skupina, kjer niso manjkajoči).
#   - Ta rezultat nakazuje, da odsotnost podatkov v spremenljivki BMI morda ni missing at random (MAR), saj je povezana z vrednostmi v spremenljivki SkinThickness.

# ------------------------------ chi-kvdarta test ------------------------------
# Class
prop.table(table(is.na(data$SkinThickness), data$Class), 2) # večja verjetnost, da bo manjkala pri puncah, ki imajo diabetes
chisq.test(table(is.na(data$SkinThickness), data$Class))
# ni statistično značilne povezave med manjkajočimi podatki v stolpcu SkinThickness in kategorijami v spremenljivki Class

prop.table(table(is.na(data$Insulin), data$Class), 2) # večja verjetnost, da bo manjkala pri puncah, ki imajo diabetes
chisq.test(table(is.na(data$Insulin), data$Class))
# ni statistično značilne povezave med manjkajočimi podatki v stolpcu Insulin in kategorijami v spremenljivki Class

prop.table(table(is.na(data$BloodPressure), data$Class), 2) # večja verjetnost, da bo manjkala pri puncah, ki imajo diabetes
chisq.test(table(is.na(data$BloodPressure), data$Class))
# ni statistično značilne povezave med manjkajočimi podatki v stolpcu BloodPressure in kategorijami v spremenljivki Class

prop.table(table(is.na(data$Glucose), data$Class), 2) # večja verjetnost, da bo manjkala pri puncah, ki imajo diabetes

prop.table(table(is.na(data$BMI), data$Class), 2) # večja verjetnost, da bo manjkala pri puncah, ki nimajo diabetes

# ======================= dealing with missing values ==========================

# formula ki jo uporabiva v vsakem logističnem modelu

f <- formula(Class ~ .)

# ___________ originalni podatki (brez spreminjanja)

model_org <- glm(f, data=data, family = binomial(link="logit"))
# 376 enot izbrisanih zaradi manjkajočih podatkov

# __________ podatki brez manjkajocih vrednosti

brez_na <- complete.cases(data)
model_brez_na <- glm(f, data=data[brez_na,], family = binomial(link="logit"))

# ___________ random forest

dataRF <- rfImpute(f, data, niter=10)
model_RF <- glm(f, data=dataRF, family = binomial(link="logit"))


# ____________ Multiple imputation
# tabela manjkajočih vrednosti
md.pattern(data, rotate.names=TRUE)

# izračun manjkajočih vrednosti
if(file.exists("data_mice.RDS")){
  data_mice <- readRDS("data_mice.RDS")
} else {
  data_mice <- mice(data, m=40, maxit=50)
  saveRDS(data_mice, "data_mice.RDS")
}


# narisemo mean/sd...skonvergira?
plot(data_mice, layout=c(2, 5))

# naredimo model
model_mice = with(data=data_mice, expr=glm(Class ~ Pregnant + Glucose + BloodPressure + SkinThickness +
                                             Insulin + BMI + DiabetesPedigree + Age,
                                           family = binomial(link="logit")))
# zdruzimo podatke
mice_zdruzeno = pool(model_mice)
mice_summary = summary(mice_zdruzeno)

