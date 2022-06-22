library(data.table)
library(dplyr)
library(ggplot2)
library(corrplot)
library(reshape2)
library(car)
library(tidyverse)
library(tidyr)

install.packages("corrr")

#Appréhender la base 
data<-fread("Abide_Tabbed.csv")
names(data)

##Stats descriptive sur le nombre de femmes et d'hommes en général, et en fonction du groupe
data_maleASD=data %>%
  filter(DX_GROUP=="ASD") %>%
  filter(SEX==1)

data_femaleASD=data %>%
  filter(DX_GROUP=="ASD") %>%
  filter(SEX==2)

data_femaleC=data %>%
  filter(DX_GROUP=="Control") %>%
  filter(SEX==2)

data_maleC=data %>%
  filter(DX_GROUP=='Control') %>%
  filter(SEX==1)

data_ASD=data %>%
  filter(DX_GROUP=='ASD')

data_control=data %>%
  filter(DX_GROUP=='Control')

data_female=data %>%
  filter(SEX==2)
data_male=data %>%
  filter(SEX==1)

# Région du cerveau

ASD_cerveau <-data_ASD[,c(84:128)] 
sapply(ASD_cerveau, function(x) sum(is.na(x)))

crtl_cerveau <-data_control[,c(84:128)] 
sapply(crtl_cerveau, function(x) sum(is.na(x)))


femaleASD_cerveau <-data_femaleASD[,c(84:128)] 
sapply(femaleASD_cerveau, function(x) sum(is.na(x)))


maleASD_cerveau <-data_maleASD[,c(84:128)] 
sapply(maleASD_cerveau, function(x) sum(is.na(x)))


maleC_cerveau <-data_maleC[,c(84:128)] 
sapply(maleC_cerveau, function(x) sum(is.na(x)))


femaleC_cerveau <-data_femaleC[,c(84:128)] 
sapply(femaleC_cerveau, function(x) sum(is.na(x)))

female_cerveau <- data_female[,c(84:128)]
male_cerveau <-data_male[,c(84:128)]


##1.Table de données cerveau

ASDf_cerveau=as.data.frame(bind_cols(as.data.frame(sapply(femaleASD_cerveau,median,na.rm=TRUE)),as.data.frame(sapply(femaleASD_cerveau,mean,na.rm=TRUE)),as.data.frame(sapply(femaleASD_cerveau,sd,na.rm=TRUE)),as.data.frame(sapply(femaleASD_cerveau, function(x) sum(is.na(x)))))) 
Controlf_cerveau=as.data.frame(bind_cols(as.data.frame(sapply(femaleC_cerveau,median,na.rm=TRUE)),as.data.frame(sapply(femaleC_cerveau,mean,na.rm=TRUE)),as.data.frame(sapply(femaleC_cerveau,sd,na.rm=TRUE)),as.data.frame(sapply(femaleC_cerveau, function(x) sum(is.na(x))))))

ASDm_cerveau=as.data.frame(bind_cols(as.data.frame(sapply(maleASD_cerveau,median,na.rm=TRUE)),as.data.frame(sapply(maleASD_cerveau,mean,na.rm=TRUE)),as.data.frame(sapply(maleASD_cerveau,sd,na.rm=TRUE)),as.data.frame(sapply(maleASD_cerveau, function(x) sum(is.na(x))))))
Controlm_cerveau=as.data.frame(bind_cols(as.data.frame(sapply(maleC_cerveau,median,na.rm=TRUE)),as.data.frame(sapply(maleC_cerveau,mean,na.rm=TRUE)),as.data.frame(sapply(maleC_cerveau,sd,na.rm=TRUE)),as.data.frame(sapply(maleC_cerveau, function(x) sum(is.na(x))))))

f_cerveau=as.data.frame(bind_cols(as.data.frame(sapply(female_cerveau,median,na.rm=TRUE)),as.data.frame(sapply(female_cerveau,mean,na.rm=TRUE)),as.data.frame(sapply(female_cerveau,sd,na.rm=TRUE)),as.data.frame(sapply(female_cerveau, function(x) sum(is.na(x)))))) 
m_cerveau=as.data.frame(bind_cols(as.data.frame(sapply(male_cerveau,median,na.rm=TRUE)),as.data.frame(sapply(male_cerveau,mean,na.rm=TRUE)),as.data.frame(sapply(male_cerveau,sd,na.rm=TRUE)),as.data.frame(sapply(male_cerveau, function(x) sum(is.na(x)))))) 

ASDf_cerveau$var=rownames(ASDf_cerveau)
names(ASDf_cerveau)<-c("median_ASD_f","mean_ASD_f","sd_ASD_f","NA_ASD_f")

Controlf_cerveau$var=rownames(Controlf_cerveau)
names(Controlf_cerveau)<-c("median_C_f","mean_C_f","sd_C_f","NAC_f")

ASDm_cerveau$var=rownames(ASDm_cerveau)
names(ASDm_cerveau)<-c("median_ASD_m",'mean_ASD_m','sd_ASD_m','NA_ASD_m')

f_cerveau$var=rownames(f_cerveau)
names(f_cerveau)<-c("median_f","mean_f","sd_f","NA_f")

m_cerveau$var=rownames(m_cerveau)
names(m_cerveau)<-c("median_m","mean_m","sd_m","NA_m")

Controlm_cerveau$var=rownames(Controlm_cerveau)
names(Controlm_cerveau)<-c("medianC_m","meanC_m","sdC_m","NC_m")

Cleainingbrain_frame=as.data.frame(bind_cols(f_cerveau,ASDf_cerveau,m_cerveau,ASDm_cerveau))

#Table finale

Descriptive_data_brain=Cleainingbrain_frame[,c(1:4,6:9,11:14,16:19)]
fwrite(Descriptive_data_brain,'Brain descriptive statistics.csv')


###Méthodes
##Pour compter le nombre de valeurs manquantes
is.na(data)
which(is.na(data_maleASD),arr.ind=TRUE) ##trop de données pour cette méthode
## Compter le nombre de valeurs manquantes dans chacune des colonnes
sapply(data, function(x) sum(is.na(x)))
##Compter le nombre de NA dans une colonne en particulier
sapply(is.na(data$colonne))
##Afficher les lignes dont la valeur dans la colonne "colonne" est NA
data[is.na(data$colonne),]

#2.Table de données tests

test=data[,c(3:6,9:11,15:38,43:72)]

test_ASD=test %>%
  filter(DX_GROUP=='ASD')

test_f=test %>%
  filter(SEX==2)

test_m=test %>%
  filter(SEX==1)

testASD_f=test %>%
  filter(SEX==2) %>%
  filter(DX_GROUP=="ASD")

testC_f=test %>%
  filter(SEX==2) %>%
  filter(DX_GROUP=="Control")

testASD_m=test %>%
  filter(SEX==1) %>%
  filter(DX_GROUP=="ASD")

testC_m=test %>%
  filter(SEX==1) %>%
  filter(DX_GROUP=="Control")

test_f=test_f[,c(3:61)]
test_m=test_m[,c(3:61)]
testASD_f=testASD_f[,c(3:61)]
testC_f=testC_f[,c(3:61)]
testASD_m=testASD_m[,c(3:61)]
testC_m=testC_m[,c(3:61)]

Median_testf=as.data.frame(sapply(test_f,median,na.rm=TRUE))
mean_testf=as.data.frame(sapply(test_f,mean,na.rm=TRUE))
sd_testf=as.data.frame(sapply(test_f,sd,na.rm=TRUE))
#n=as.data.frame(sapply(ASD_cerveau,n))

Median_testm=as.data.frame(sapply(test_m,median,na.rm=TRUE))
mean_testm=as.data.frame(sapply(test_m,mean,na.rm=TRUE))
sd_testm=as.data.frame(sapply(test_m,sd,na.rm=TRUE))


sapply(data_maleASD, function(x) sum(is.na(x)))
NA_f=as.data.frame(sapply(test_f, function(x) sum(is.na(x))))
NA_m=as.data.frame(sapply(test_m, function(x) sum(is.na(x))))


Tabf=as.data.frame(bind_cols(Median_testf,mean_testf,sd_testf,NA_f)) 
TabASD_f=as.data.frame(bind_cols(as.data.frame(sapply(testASD_f,median,na.rm=TRUE)),as.data.frame(sapply(testASD_f,mean,na.rm=TRUE)),as.data.frame(sapply(testASD_f,sd,na.rm=TRUE)),as.data.frame(sapply(testASD_f, function(x) sum(is.na(x))))))
TabC_f=as.data.frame(bind_cols(as.data.frame(sapply(testC_f,median,na.rm=TRUE)),as.data.frame(sapply(testC_f,mean,na.rm=TRUE)),as.data.frame(sapply(testC_f,sd,na.rm=TRUE)),as.data.frame(sapply(testC_f, function(x) sum(is.na(x))))))

Tabm=as.data.frame(bind_cols(Median_testm,mean_testm,sd_testm,NA_m))
TabASD_m=as.data.frame(bind_cols(as.data.frame(sapply(testASD_m,median,na.rm=TRUE)),as.data.frame(sapply(testASD_m,mean,na.rm=TRUE)),as.data.frame(sapply(testASD_m,sd,na.rm=TRUE)),as.data.frame(sapply(testASD_m, function(x) sum(is.na(x))))))
TabC_m=as.data.frame(bind_cols(as.data.frame(sapply(testC_m,median,na.rm=TRUE)),as.data.frame(sapply(testC_m,mean,na.rm=TRUE)),as.data.frame(sapply(testC_m,sd,na.rm=TRUE)),as.data.frame(sapply(testC_m, function(x) sum(is.na(x))))))

  
Tabf$var=rownames(Tabf)
names(Tabf)<-c("Median_f","mean_f",'sd_f',"NA_f")

TabASD_f$var=rownames(TabASD_f)
names(TabASD_f)<-c("median_ASD_f",'mean_ASD_f',"sd_ASD_f","NA_ASD_f")

TabC_f$var=rownames(TabC_f)
names(TabC_f)<-c("medianC_f",'meanC_f',"sdC_f","NAC_f")


Tabm$var=rownames(Tabm)
names(Tabm)<-c("median_m","mean_m","sd_m","NA_m")

TabASD_m$var=rownames(TabASD_m)
names(TabASD_m)<-c("median_ASD_m","mean_ASD_m","sd_ASD_m","NA_ASD_m")

TabC_m$var=rownames(TabC_m)
names(TabC_m)<-c("medianC_m","meanC_m","sdC_m","NAC_m")

##Table finale
Cleaningtest_frame=as.data.frame(bind_cols(Tabf,TabASD_f,Tabm,TabASD_m))
Descriptive_data_test=Cleaningtest_frame[,c(1:4,6:9,11:14,16:19)]
fwrite(Descriptive_data_test,"Test descriptive statistics.csv")




##Matrice de corrélation


df=as.data.frame(data_ASD[,c(9:11,15:38,84:116,124:128)])
df2=as.data.frame(data_maleASD[,c(9:11,15:38,84:116,124:128)])


Cor1=cor(maleASD_cerveau[,c(1:34,37,40:45)],testASD_m[,c(18:58)],use='p')
corrplot(Cor1,diag=FALSE,number.cex=0.3,tl.cex=0.5, title="Matrice de corrélation hommes autistes")

corf_ASD=cor(femaleASD_cerveau[,c(1:32,37,40:45)],testASD_f[,c(5:28,30:58)],use='p')
corrplot(corf_ASD,diag=FALSE,tl.cex=0.5, title="Matrice de corrélation femmes autistes")





  
##Nuage de points pour visualiser les corrélations

plot(maleASD_cerveau$`Left-Inf-Lat-Vent`,testASD_m$SRS_AWARNESS)
plot(maleASD_cerveau$`Left-Inf-Lat-Vent`,testASD_m$SRS_COMMUNICATION)
plot(maleASD_cerveau$`Right-Putamen`,testASD_m$SRS_COMMUNICATION)
plot(maleASD_cerveau$`Right-Caudate`,testASD_m$WISC_IV_VOCAB_SCALED)
plot(maleASD_cerveau$`Right-Putamen`,testASD_m$WISC_IV_VOCAB_SCALED)
plot(maleASD_cerveau$`Right-Putamen`,testASD_m$WISC_IV_VCI)



##Graphique linéaire: FIGURES

Newdata=as.data.frame(bind_cols(as.data.frame(ASD_cerveau$`Right-Putamen`),as.data.frame(test_ASD$WISC_IV_VOCAB_SCALED)))
ggplot1=ggplot(Newdata,aes(x=ASD_cerveau$`Right-Putamen`,y=test_ASD$WISC_IV_VOCAB_SCALED))+geom_point(alpha=0.5,size=1)+geom_smooth(method="lm",formula=y~x)
print(ggplot1+labs(title="WISC vocabulary score and right putamen volume",y="WISC_IV_VOCAB_SCALED",x="Right Putamen volume (mm^3)"))

Newdata2=as.data.frame(bind_cols(as.data.frame(ASD_cerveau$`Right-Putamen`),as.data.frame(test_ASD$WISC_IV_VCI)))
ggplot2=ggplot(Newdata2,aes(x=ASD_cerveau$`Right-Putamen`,y=test_ASD$WISC_IV_VCI))+geom_point(alpha=0.5,size=1)+geom_smooth(method="lm",formula=y~x)
print(ggplot2+labs(title="WISC visual comprehension index score and right putamen volume",y="WISC_IV_VCI",x="Right Putamen volume (mm^3)"))

##Régression linéaire
a=scale(ASD_cerveau$`Right-Putamen`, center = TRUE, scale = TRUE)
b=scale(test_ASD$WISC_IV_VOCAB_SCALED, center = TRUE, scale = TRUE)
Model1 = lm(scale(b)~scale(a)+test_ASD$AGE_AT_SCAN+test_ASD$SEX)
summary(Model1)


