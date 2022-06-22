library(data.table)
library(dplyr)
library(ggplot2)
library(corrplot)
library(reshape2)
library(car)
library(tidyverse)
library(tidyr)

install.packages("corrr")
data<-fread("Abide_Tabbed.csv")
names(data)
pacman::p_load(pacman, psych,corrplot) 
sapply(data,function(x)length(unique(x))) ##génial pour voir le nombre de valeurs différentes dans chaque colonnes
#data=data %>%
  #filter(QCExcluded_vs_Included=="Included")
#View(data)
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

#Question 1: Région du cerveau

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


##Question 1
DT_cerveau=data[,c(84:128)]
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
fwrite(Cleaningbrain_frame,"descriptive_brain.csv")
Descriptive_data_brain=Cleainingbrain_frame[,c(1:4,6:9,11:14,16:19)]
fwrite(Descriptive_data_brain,'Brain descriptive statistics.csv')

sapply(DT_cerveau, function(x) sum(is.na(x)))



##Pour compter le nombre de valeurs manquantes: pls méthodes
is.na(data)
which(is.na(data_maleASD),arr.ind=TRUE) ##trop de données pour cette méthode

## compter le nombre de valeurs manquantes dans chacune des colonnes
sapply(data, function(x) sum(is.na(x)))
##compter le nombre de NA dans une colonne en particulier
sapply(is.na(data$colonne))
##afficher les lignes dont la valeur dans la colonne "colonne" est NA
data[is.na(data$colonne),]

#2

test=data[,c(3:6,9:11,15:38,43:72)]
describe(test)
Median_test=as.data.frame(sapply(test,median,na.rm=TRUE))
mean_test=as.data.frame(sapply(test,mean,na.rm=TRUE))
sd_test=as.data.frame(sapply(test,sd,na.rm=TRUE))

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
n=as.data.frame(sapply(ASD_cerveau,n))

Median_testm=as.data.frame(sapply(test_m,median,na.rm=TRUE))
mean_testm=as.data.frame(sapply(test_m,mean,na.rm=TRUE))
sd_testm=as.data.frame(sapply(test_m,sd,na.rm=TRUE))


##Question 2

sapply(data_maleASD, function(x) sum(is.na(x)))
NA_f=as.data.frame(sapply(test_f, function(x) sum(is.na(x))))
NA_m=as.data.frame(sapply(test_m, function(x) sum(is.na(x))))

#Tab=as.data.frame(bind_cols(Median_test,mean_test,sd_test)) 
#rename(Tab,c("Median"="sapply(test, median, na.rm = TRUE)","Mean"="sapply(test, mean, na.rm = TRUE)","sd"="sapply(test, sd, na.rm = TRUE)"))

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


Cleaningtest_frame=as.data.frame(bind_cols(Tabf,TabASD_f,Tabm,TabASD_m))
fwrite(Cleaningtest_frame,"descriptive_test.csv")
Descriptive_data_test=Cleaningtest_frame[,c(1:4,6:9,11:14,16:19)]
fwrite(Descriptive_data_test,"Test descriptive statistics.csv")

#3Outliers...



#ggplot(testASD_m) +
  aes(x = FIQ) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()


#df2=as.data.frame(data_maleASD[,c(9:10,15:31,101:128)]) %>%
  drop_na()

df=as.data.frame(data_ASD[,c(9:11,15:38,84:116,124:128)])

#M=cor(df,use="p")
#pdf=(file='matrice de corrélation ASD_tests_cerveau.pdf')
#corrplot(M,method='color',type="upper",diag=FALSE,tl.cex=0.5, title="Matrice de corrélation entre les données sur le cerveau et les tests de personnes autistes")
#dev.off()

df2=as.data.frame(data_maleASD[,c(9:11,15:38,84:116,124:128)])

#M2=cor(df2,use="p")
#pdf=(file='Hommes autistes')
#corrplot(M2,type='upper',diag=FALSE,tl.cex=0.5, title="Matrice de corrélation et hommes autistes")
#dev.off()

a=cor(ASD_cerveau[,c(1:34,37,40:45)],test_ASD[,c(5:60)],use='p')
corrplot(a,diag=FALSE,tl.cex=0.5, title="Matrice de corrélation personnes autistes")

b=cor(maleASD_cerveau[,c(1:34,37,40:45)],testASD_m[,c(18:58)],use='p')
corrplot(b,diag=FALSE,number.cex=0.3,tl.cex=0.5, title="Matrice de corrélation hommes autistes")
colf_ASD=cor(femaleASD_cerveau[,c(1:34,37,40:45)],testASD_f[,c(5:58)],use='p')
corrplot(colf_ASD,diag=FALSE,tl.cex=0.5, title="Matrice de corrélation femmes autistes")

#library(corrr)
#any_over_80 <-function(x) any(x>0.8,na.rm=TRUE)
#b %>%
  focus_if(any_over_80,mirror=TRUE)
#data.frame(b)%>%
  rownames_to_column() %>%
  gather(key="variable",value="correlation",-rowname) %>%
  filter(abs(correlation)>0.8)


c=cor(ASD_cerveau[,c(1:34,37,41,45)],crtl_cerveau[,c(1:34,37,40,45)],use='p')
corrplot(c,diag=FALSE,tl.cex=0.5, title="Matrice de corrélation cerveau: contrôles et autistes")


  
Left_hemisphre_ASD=ASD_cerveau %>% 
  select(1:8,12,13,15:18)
Right_hemisphere_ASD=ASD_cerveau %>%
  select(19:32)
  
d=cor(Left_hemisphre_ASD,Right_hemisphere_ASD,use='p')
lol=corrplot(d,type="upper",addCoef.col='black',number.cex=0.7,diag=FALSE,tl.cex=0.8,title="Matrice de corréltion entre les hemisphères cérébreaux de personnes autistes")



Left_hemisphre_Crtl=crtl_cerveau %>% 
  select(1:8,12,13,15:18)
Right_hemisphere_Crtl=crtl_cerveau %>%
  select(19:32)
e=cor(Left_hemisphre_Crtl,Right_hemisphere_Crtl,use='p')
corrplot(e,type="lower",addCoef.col = 'black',diag=FALSE,tl.cex=0.8,number.cex=0.7,title="Matrice de corréltion entre les hemisphères cérébreaux_contrôles")


cor_combined=d
cor_combined[upper.tri(cor_combined)]=d[upper.tri(e)]

cor_df=as.data.frame(cor_combined) %>%
  dplyr::mutate(var1=row.names(.)) %>%
  tidyr::gather("var2","value",-var1)
head(cor_df)

ggplot(cor_df,aes(var1,var2))+
  geom_tile(aes(fill=value),color="white")+
  ggtitle("Corrélation des volumes cérébraux entre autistes et contrôles") +
  scale_fill_gradient(low = "pink", high = "navy") + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + 
  labs(x=NULL, y=NULL) + 
  theme(legend.position = "right", 
        axis.ticks = element_blank(), 
        axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title=element_text(color="black", size=14,face="bold.italic",hjust=0.5))



ggplot(data_maleASD) +
  aes(x=data_maleASD$'WISC_IV_VOCAB_SCALED',y=data_maleASD$'Right-Putamen')+
  geom_point()
  
k=as.data.frame(data_maleASD$`Right-Putamen`)
l=as.data.frame(data_maleASD$WISC_IV_VOCAB_SCALED)

plot(k,l)

#Q1<-summary(DT_cerveau$`Left-Caudate`)[["1st Qu."]]
#Q3<-summary(DT_cerveau$`Left-Caudate`)[["3rd Qu."]]
#IQR=Q3-Q1
#LowerBound<-(Q1-1.5*IQR)
#UpperBound<-(Q3+1.5*IQR)
#Outlier=Observations > Q3 + 1.5*IQR or < Q1-1.5*IQR
#no=subset(DT_cerveau,DT_cerveau$Left-Caudate>(Q1))










plot(maleASD_cerveau$`Left-Inf-Lat-Vent`,testASD_m$SRS_AWARNESS)
plot(maleASD_cerveau$`Left-Inf-Lat-Vent`,testASD_m$SRS_COMMUNICATION)
plot(maleASD_cerveau$`Right-Putamen`,testASD_m$SRS_COMMUNICATION)
ggplot(maleASD_cerveau$`Right-Putamen`,testASD_m$WISC_IV_VOCAB_SCALED,main="Scatterplot: vocabulary and right putamen", xlab="Right-Putamen",ylab="WISC vocabulary scaled")
plot(maleASD_cerveau$`Right-Caudate`,testASD_m$WISC_IV_VOCAB_SCALED)
plot(maleASD_cerveau$`Left-Caudate`,testASD_m$WISC_IV_VOCAB_SCALED)
plot(maleASD_cerveau$`Right-Putamen`,testASD_m$WISC_IV_VCI,main="Scatterplot: visual comprehension and right putamen", xlab="Right-Putamen",ylab="WISC visual comprehension index")
print(plot+labs(y="WISX_IV_VOCAB_SCALED",x="Right-Putament"))
Model1 = lm(scale(b)~scale(a))
summary(Model1)
Dscatterplot(test_ASD$WISC_IV_VOCAB_SCALED~ASD_cerveau$`Right-Putamen`+test_ASD$AGE_AT_SCAN+test_ASD$SEX)

Controlf_cerveau=as.data.frame(bind_cols(as.data.frame(sapply(femaleC_cerveau,median,na.rm=TRUE)),as.data.frame(sapply(femaleC_cerveau,mean,na.rm=TRUE)),as.data.frame(sapply(femaleC_cerveau,sd,na.rm=TRUE)),as.data.frame(sapply(femaleC_cerveau, function(x) sum(is.na(x))))))

a=scale(ASD_cerveau$`Right-Putamen`, center = TRUE, scale = TRUE)
b=scale(test_ASD$WISC_IV_VOCAB_SCALED, center = TRUE, scale = TRUE)
c=scale(test_ASD$WISC_IV_VCI, center = TRUE, scale = TRUE)

dkdi=as.data.frame(bind_cols(as.data.frame(ASD_cerveau$`Right-Putamen`),as.data.frame(test_ASD$WISC_IV_VOCAB_SCALED)))
lpl=ggplot(dkdi,aes(x=ASD_cerveau$`Right-Putamen`,y=test_ASD$WISC_IV_VOCAB_SCALED))+geom_point(alpha=0.5,size=1)+geom_smooth(method="lm",formula=y~x)
print(lpl+labs(title="WISC vocabulary score and right putamen volume",y="WISC_IV_VOCAB_SCALED",x="Right Putamen volume (mm^3)"))

ml=as.data.frame(bind_cols(as.data.frame(ASD_cerveau$`Right-Putamen`),as.data.frame(test_ASD$WISC_IV_VCI)))
ll=ggplot(dkdi,aes(x=ASD_cerveau$`Right-Putamen`,y=test_ASD$WISC_IV_VCI))+geom_point(alpha=0.5,size=1)+geom_smooth(method="lm",formula=y~x)
print(ll+labs(title="WISC vocabulary comprehension index score and right putamen volume",y="WISC_IV_VCI",x="Right Putamen volume (mm^3)"))
