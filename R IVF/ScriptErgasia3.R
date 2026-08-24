remove.packages("rlang")
remove.packages("vctrs")
remove.packages("descr")
remove.packages("dplyr")
remove.packages("ggpubr")
remove.packages("descr")
remove.packages("tidyr")
remove.packages("readxl")


install.packages("rlang")
install.packages("vctrs")
install.packages("descr")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("descr")
install.packages("tidyr")
install.packages("readxl")
install.packages("expss")
install.packages("plotrix")
install.packages("EnvStats")

library("EnvStats")
library("plotrix")
library("vctrs")
library("dplyr")
library("ggpubr")
library("descr")
library("tidyr")
library("readxl")
library("rmarkdown")
library("rlang")
library("car")
library("expss")

dataFR <- read_excel("C:/Users/Mitsos/Desktop/dataFR.xlsx")
attach(dataFR)

nrow(dataFR)


for(i in 2:ncol(dataFR)){
  print(table(dataFR[,i],useNA = "always"))
}


###Food

Cal1<-matrix(0,nrow= nrow(dataFR), ncol=1) #calculate Calories for main food matrix method 1
Fat1<-matrix(0,nrow= nrow(dataFR), ncol=1)#calculate Fats for main food matrix method 1
Carbs1<-matrix(0,nrow= nrow(dataFR), ncol=1)#calculate Carbs for main food matrix method 1
Protein1<-matrix(0,nrow= nrow(dataFR), ncol=1)#calculate Proteins for main food matrix method 1
calfood<-c(287*0.9, 84*0.9, 45*1.9, 64*2.5, 124*2.5, 320*0.3, 131*0.8, 87*0.8, 52*1.4, 222*0.45, 884*0.15, 195*0.9)
fatfood<-c(19.29*0.9, 0.92*0.9, 2.63*1.9, 0.52*2.5, 2.99*2.5, 2.2*0.3, 1.05*0.8, 0.1*0.8, 0.17*1.4, 9.52*0.45, 100*0.15, 7.72*0.9)
carbfood<-c(0*0.9, 0*0.9, 4.82*1.9, 13.46*2.5, 2.99*2.5, 80.4*0.3, 24.93*0.8, 20.13*0.8, 13.81*1.4, 31.76*0.45, 0*0.15, 0*0.9)
Protfood<-c(26.41*0.9, 17.76*0.9, 1.69*1.9, 3.33*2.5, 2.99*2.5, 9.4*0.3, 5.15*0.8, 1.87*0.8, 0.26*1.4, 4.07*0.45, 0*0.15, 29.55*0.9)
freqfood<-c(0,0,2.5,0.21,0.57,1)
dishS<-c(0.7,1,2)

for (j in 1:length(calfood)){
  for (i in 1:nrow(dataFR)){
    if (!is.na(dataFR[i,82+j])==TRUE){
      
      if (freqfood[as.numeric(dataFR[i,82+j])]!=0){
        if (!is.na(dataFR[i,95])==TRUE){
          Cal1[i]<-Cal1[i]+(calfood[j]*freqfood[as.numeric(dataFR[i,82+j])])*dishS[as.numeric(dataFR[i,95])]
        }else{  Cal1[i]<-Cal1[i]+(calfood[j]*freqfood[as.numeric(dataFR[i,82+j])])*1}}
      
      if (fatfood[as.numeric(dataFR[i,82+j])]!=0){
        if (!is.na(dataFR[i,95])==TRUE){
          Fat1[i]<-Fat1[i]+(fatfood[j]*freqfood[as.numeric(dataFR[i,82+j])])*dishS[as.numeric(dataFR[i,95])]
        }else{ Fat1[i]<-Fat1[i]+(fatfood[j]*freqfood[as.numeric(dataFR[i,82+j])])*1}}
      
      if (carbfood[as.numeric(dataFR[i,82+j])]!=0){
        if (!is.na(dataFR[i,95])==TRUE){
          Carbs1[i]<-Carbs1[i]+(carbfood[j]*freqfood[as.numeric(dataFR[i,82+j])])*dishS[as.numeric(dataFR[i,95])]
        }else{ Carbs1[i]<-Carbs1[i]+(carbfood[j]*freqfood[as.numeric(dataFR[i,82+j])])*1} }
      
      if (Protfood[as.numeric(dataFR[i,82+j])]!=0){
        if (!is.na(dataFR[i,95])==TRUE){
          Protein1[i]<-Protein1[i]+(Protfood[j]*freqfood[as.numeric(dataFR[i,82+j])])*dishS[as.numeric(dataFR[i,95])]
        }else{ Protein1[i]<-Protein1[i]+(Protfood[j]*freqfood[as.numeric(dataFR[i,82+j])])*1}}
    }
  }
}


Cal2<-matrix(0,nrow= nrow(dataFR), ncol=1) #calculate Calories for main food matrix method 2 (everyone eats a litlle bit of everything, even rarely)
Fat2<-matrix(0,nrow= nrow(dataFR), ncol=1) #calculate Fats for main food matrix method 2
Carbs2<-matrix(0,nrow= nrow(dataFR), ncol=1) #calculate Carbs for main food matrix method 2
Protein2<-matrix(0,nrow= nrow(dataFR), ncol=1) #calculate Proteins for main food matrix method 2
freqfood2<-c(0.01,0.1,2.5,0.21,0.57,1)

for (j in 1:length(calfood)){
  for (i in 1:nrow(dataFR)){
    if (is.na(dataFR[i,82+j])==TRUE){
      if (!is.na(dataFR[i,95])==TRUE){
        Cal2[i]<-Cal2[i]+(calfood[j]*freqfood2[1])*dishS[as.numeric(dataFR[i,95])]
      }else{Cal2[i]<-Cal2[i]+(calfood[j]*freqfood2[1])*1}
      
      if (!is.na(dataFR[i,95])==TRUE){
        Fat2[i]<-Fat2[i]+(fatfood[j]*freqfood2[1])*dishS[as.numeric(dataFR[i,95])]
      }else{Fat2[i]<-Fat2[i]+(fatfood[j]*freqfood2[1])*1}
      
      if (!is.na(dataFR[i,95])==TRUE){
        Carbs2[i]<-Carbs2[i]+(carbfood[j]*freqfood2[1])*dishS[as.numeric(dataFR[i,95])]
      }else{Carbs2[i]<-Carbs2[i]+(carbfood[j]*freqfood2[1])*1}
      
      if (!is.na(dataFR[i,95])==TRUE){
        Protein2[i]<-Protein2[i]+(Protfood[j]*freqfood2[1])*dishS[as.numeric(dataFR[i,95])]
      }else{Protein2[i]<-Protein2[i]+(Protfood[j]*freqfood2[1])*1}
      
    }else{
      if (!is.na(dataFR[i,95])==TRUE){
        Cal2[i]<-Cal2[i]+(calfood[j]*freqfood2[as.numeric(dataFR[i,82+j])])*dishS[as.numeric(dataFR[i,95])]
      }else{Cal2[i]<-Cal2[i]+(calfood[j]*freqfood2[as.numeric(dataFR[i,82+j])])*1}
      
      if (!is.na(dataFR[i,95])==TRUE){
        Fat2[i]<-Fat2[i]+(fatfood[j]*freqfood2[as.numeric(dataFR[i,82+j])])*dishS[as.numeric(dataFR[i,95])]
      }else{Fat2[i]<-Fat2[i]+(fatfood[j]*freqfood2[as.numeric(dataFR[i,82+j])])*1}
      
      if (!is.na(dataFR[i,95])==TRUE){
        Carbs2[i]<-Carbs2[i]+(carbfood[j]*freqfood2[as.numeric(dataFR[i,82+j])])*dishS[as.numeric(dataFR[i,95])]
      }else{Carbs2[i]<-Carbs2[i]+(carbfood[j]*freqfood2[as.numeric(dataFR[i,82+j])])*1}
      
      if (!is.na(dataFR[i,95])==TRUE){
        Protein2[i]<-Protein2[i]+(Protfood[j]*freqfood2[as.numeric(dataFR[i,82+j])])*dishS[as.numeric(dataFR[i,95])]
      }else{Protein2[i]<-Protein2[i]+(Protfood[j]*freqfood2[as.numeric(dataFR[i,82+j])])*1}  
      
    }
  }
}


RstCal1<-matrix(0,nrow= nrow(dataFR), ncol=1) #Rest of Calories
RstFats<-matrix(0,nrow= nrow(dataFR), ncol=1) #Rest of Fats
RstCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) #Rest of Carbs
RstProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) #Rest of Protein

Br<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Bread
BrFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Bread
BrCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Bread
BrProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Bread


Sftd<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Soft- drinks
SftdFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Soft- drinks
SftdCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Soft- drinks
SftdProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Soft- drinks

Mlk<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Milk
MlkFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Milk
MlkCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Milk
MlkProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Milk

Yg<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Yogurt
YgFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Yogurt
YgCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Yogurt
YgProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Yogurt

Ch<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Cheese Total
ChFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Cheese Total
ChCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Cheese Total
ChProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Cheese Total

ChW<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Cheese white
ChWFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Cheese white
ChWCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Cheese white
ChWProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Cheese white

ChY<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Cheese Yellow
ChYFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Cheese Yellow
ChYCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Cheese Yellow
ChYProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Cheese Yellow

ChL<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Cheese Low-fat
ChLFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Cheese Low-fat
ChLCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Cheese Low-fat
ChLProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Cheese Low-fat

Dr<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Alcohol
DrFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Alcohol
DrCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Alcohol
DrProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Alcohol

DWW<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from White Whine
DWWFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from White Whine
DWWCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from White Whine
DWWProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from White Whine

DRW<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Red Whine
DRWFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Red Whine
DRWCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Red Whine
DRWProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Red Whine

DWH<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Whiskey
DWHFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Whiskey
DWHCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Whiskey
DWHProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Whiskey

DBR<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Beer
DBRFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Beer
DBRCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Beer
DBRProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Beer

DOU<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Ouzo
DOUFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Ouzo
DOUCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Ouzo
DOUProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Ouzo

Cff<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Coffee Total
CffFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Coffee Total
CffCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Coffee Total
CffProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Coffee Total

CfGr<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Coffee Greek
CfGrFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Coffee Greek
CfGrCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Coffee Greek
CfGrProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Coffee Greek

CfNC<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Coffee Nes/Cappuccino
CfNCFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Coffee Nes/Cappuccino
CfNCCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Coffee Nes/Cappuccino
CfNCProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Coffee Nes/Cappuccino

CfFil<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Coffee Filter
CfFilFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Coffee Filter
CfFilCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Coffee Filter
CfFilProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Coffee Filter

CfEs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Coffee Espresso (most common 4th choise)
CfEsFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Coffee Espresso (most common 4th choise)
CfEsCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Coffee Espresso (most common 4th choise)
CfEsProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Coffee Espresso (most common 4th choise)

Tea<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Tea Total
TeaFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Tea Total
TeaCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Tea Total
TeaProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Tea Total

Oil<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Oil-butter Total
OilFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Oil-butter Total
OilCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Oil-butter Total
OilProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Oil-butter Total

Sunoil<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Sunflower Total
SunoilFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Sunflower Total
SunoilCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Sunflower Total
SunoilProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Sunflower Total

Butter<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Butter Total
ButterFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Butter Total
ButterCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Butter Total
ButterProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Butter Total

Oliv<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Oil-Oil Total
OlivFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Oil-Oil Total
OlivCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Oil-Oil Total
OlivProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Oil-Oil Total

Margar<-matrix(0,nrow= nrow(dataFR), ncol=1) # Calories from Margarine Total
MargarFats<-matrix(0,nrow= nrow(dataFR), ncol=1) # Fats from Margarine Total
MargarCarbs<-matrix(0,nrow= nrow(dataFR), ncol=1) # Carbs from Margarine Total
MargarProtein<-matrix(0,nrow= nrow(dataFR), ncol=1) # Protein from Margarine Total


Brdt<-c(66,67,mean(66,67)) #bread calories
BrdFats<-c(0.82,1.07,mean(0.82,1.07)) #bread Fats
BrdCarbs<-c(12.65,12.26,mean(12.65,12.26)) #bread Carbs
BrdProtein<-c(1.91,2.37,mean(1.91,2.37)) #bread Protein

softdrink<-c(136,151,7,4) #Soft drink calories
softdrinkFats<-c(0.07,0,0.07,0) #Soft drink Fats
softdrinkCarbs<-c(35.18,39.04,0.85,0.53) #Soft drink Carbs
softdrinkProtein<-c(0.26,0,0.39,0.43) #Soft drink Protein 
sodacans<-c(0,1.5,3.5,4.5) # Number of soft drinks per day

milk<-c(146,122,83,mean(146,122,83)) #milk calories
milkFats<-c(7.93,4.81,0.2,mean(7.93,4.81,0.2)) #milk Fats
milkCarbs<-c(11.03,11.42,12.15,mean(11.03,11.42,12.15)) #milk Carbs
milkProtein<-c(7.86,8.05,8.26,mean(7.86,8.05,8.26)) #milk Protein

ygrt<-c(149,154,80,mean(149,154,80)) #yogurt calories
ygrtFats<-c(7.96,3.8,0,mean(7.96,3.8,0)) #yogurt Fats
ygrtCarbs<-c(11.42,17.25,15,mean(11.42,17.25,15)) #yogurt Carbs
ygrtProtein<-c(8.5,12.86,5,mean(8.5,12.86,5)) #yogurt Protein

chse<-c(60,70,49) #cheese calories
chseFats<-c(4,5,1.98) #cheese Fats
chseCarbs<-c(1,2,0.54) #cheese Carbs
chseProtein<-c(5,4,6.9) #cheese Protein

drink<-c(122,125,105,155,91) # Alcohol calories
drinkFats<-c(0,0,0,0,0) # Alcohol Fats
drinkCarbs<-c(3.82,3.84,0.04,12.78,0) # Alcohol Carbs
drinkProtein<-c(0.1,0.1,0,1.66,0) # Alcohol Protein
Alcgl<-c(0,1.5,3.5,4.5) #Number of glasses of alcohol per day


cofe<-c(2.4,74,2,1)#coffee calories
cofeFats<-c(0.1,3.98,0.05,0.11) #coffee Fats
cofeCarbs<-c(0,5.81,0.09,0) #coffee Carbs
cofeProtein<-c(0.3,4.08,0.28,0.07) #coffee Protein
cfgl<-c(0,1.5,3.5,4.5) #Number of glasses of coffee per day

tea<-2 #Tea calories
teaFats<-0 #Tea Fats
teaCarbs<-0.71 #Tea Carbs
teaProtein<-0 #Tea Protein
tgl<-c(0,1.5,3.5,4.5) #Number of glasses of Tea per day


oilfat<-c(119,120,102,75) # Oil-fats calories
oilfatFats<-c(13.5,13.6,11.52,8.46) # Oil-fats Fats
oilfatCarbs<-c(0,0,0.01,0) # Oil-fats Carbs
oilfatProtein<-c(0,0,0.12,0.09) # Oil-fats Protein



for(i in 1:nrow(dataFR[,97])){
  if(length(Brdt[as.numeric(dataFR[i,97])]*as.numeric(dataFR[i,99]))>0){
    Br[i]<-Brdt[as.numeric(dataFR[i,97])]*as.numeric(dataFR[i,99])}
  
  if(length(BrdFats[as.numeric(dataFR[i,97])]*as.numeric(dataFR[i,99]))>0){
    BrFats[i]<-BrdFats[as.numeric(dataFR[i,97])]*as.numeric(dataFR[i,99])}  
  
  if(length(BrdCarbs[as.numeric(dataFR[i,97])]*as.numeric(dataFR[i,99]))>0){
    BrCarbs[i]<-BrdCarbs[as.numeric(dataFR[i,97])]*as.numeric(dataFR[i,99])}
  
  if(length(BrdProtein[as.numeric(dataFR[i,97])]*as.numeric(dataFR[i,99]))>0){
    BrProtein[i]<-BrdProtein[as.numeric(dataFR[i,97])]*as.numeric(dataFR[i,99])}
  
  
  if(length(softdrink[as.numeric(dataFR[i,101])]*sodacans[as.numeric(dataFR[i,102])]>0)){
    Sftd[i]<-softdrink[as.numeric(dataFR[i,101])]*sodacans[as.numeric(dataFR[i,102])]}  
  
  if(length(softdrinkFats[as.numeric(dataFR[i,101])]*sodacans[as.numeric(dataFR[i,102])]>0)){
    SftdFats[i]<-softdrinkFats[as.numeric(dataFR[i,101])]*sodacans[as.numeric(dataFR[i,102])]}  
  
  if(length(softdrinkCarbs[as.numeric(dataFR[i,101])]*sodacans[as.numeric(dataFR[i,102])]>0)){
    SftdCarbs[i]<-softdrinkCarbs[as.numeric(dataFR[i,101])]*sodacans[as.numeric(dataFR[i,102])]} 
  
  if(length(softdrinkProtein[as.numeric(dataFR[i,101])]*sodacans[as.numeric(dataFR[i,102])]>0)){
    SftdProtein[i]<-softdrinkProtein[as.numeric(dataFR[i,101])]*sodacans[as.numeric(dataFR[i,102])]}  
  
  
  if(length(milk[as.numeric(dataFR[i,105])]*as.numeric(dataFR[i,104]))>0){
    Mlk[i]<-milk[as.numeric(dataFR[i,105])]*as.numeric(dataFR[i,104])}
  
  if(length(milkFats[as.numeric(dataFR[i,105])]*as.numeric(dataFR[i,104]))>0){
    MlkFats[i]<-milkFats[as.numeric(dataFR[i,105])]*as.numeric(dataFR[i,104])}
  
  if(length(milkCarbs[as.numeric(dataFR[i,105])]*as.numeric(dataFR[i,104]))>0){
    MlkCarbs[i]<-milkCarbs[as.numeric(dataFR[i,105])]*as.numeric(dataFR[i,104])}
  
  if(length(milkProtein[as.numeric(dataFR[i,105])]*as.numeric(dataFR[i,104]))>0){
    MlkProtein[i]<-milkProtein[as.numeric(dataFR[i,105])]*as.numeric(dataFR[i,104])}
  
  
  if(length(ygrt[as.numeric(dataFR[i,108])]*as.numeric(dataFR[i,107]))>0){
    Yg[i]<-ygrt[as.numeric(dataFR[i,108])]*as.numeric(dataFR[i,107])}
  
  if(length(ygrtFats[as.numeric(dataFR[i,108])]*as.numeric(dataFR[i,107]))>0){
    YgFats[i]<-ygrtFats[as.numeric(dataFR[i,108])]*as.numeric(dataFR[i,107])}
  
  if(length(ygrtCarbs[as.numeric(dataFR[i,108])]*as.numeric(dataFR[i,107]))>0){
    YgCarbs[i]<-ygrtCarbs[as.numeric(dataFR[i,108])]*as.numeric(dataFR[i,107])}
  
  if(length(ygrtProtein[as.numeric(dataFR[i,108])]*as.numeric(dataFR[i,107]))>0){
    YgProtein[i]<-ygrtProtein[as.numeric(dataFR[i,108])]*as.numeric(dataFR[i,107])}  
  
  
  
  if(length(chse[as.numeric(dataFR[i,110])])>0){
    ChW[i]<-chse[as.numeric(dataFR[i,110])]}  
  if(length(chse[as.numeric(dataFR[i,111])+1])>0){
    ChY[i]<-chse[as.numeric(dataFR[i,111])+1]}  
  if(length(chse[as.numeric(dataFR[i,112])+2])>0){
    ChL[i]<-chse[as.numeric(dataFR[i,112])+2]}
  A<-c()
  A<-c(cbind(ChW[i],ChY[i],ChL[i]))
  A<-as.matrix(A)
  Ch[i]<-mean(na.omit(A))
  
  
  if(length(chseFats[as.numeric(dataFR[i,110])])>0){
    ChWFats[i]<-chseFats[as.numeric(dataFR[i,110])]}  
  if(length(chseFats[as.numeric(dataFR[i,111])+1])>0){
    ChYFats[i]<-chseFats[as.numeric(dataFR[i,111])+1]}  
  if(length(chseFats[as.numeric(dataFR[i,112])+2])>0){
    ChLFats[i]<-chseFats[as.numeric(dataFR[i,112])+2]}
  A<-c()
  A<-c(cbind(ChWFats[i],ChYFats[i],ChLFats[i]))
  A<-as.matrix(A)
  ChFats[i]<-mean(na.omit(A))  
  
  
  if(length(chseCarbs[as.numeric(dataFR[i,110])])>0){
    ChWCarbs[i]<-chseCarbs[as.numeric(dataFR[i,110])]}  
  if(length(chseCarbs[as.numeric(dataFR[i,111])+1])>0){
    ChYCarbs[i]<-chseCarbs[as.numeric(dataFR[i,111])+1]}  
  if(length(chseCarbs[as.numeric(dataFR[i,112])+2])>0){
    ChLCarbs[i]<-chseCarbs[as.numeric(dataFR[i,112])+2]}
  A<-c()
  A<-c(cbind(ChWCarbs[i],ChYCarbs[i],ChLCarbs[i]))
  A<-as.matrix(A)
  ChCarbs[i]<-mean(na.omit(A))  
  
  
  if(length(chseProtein[as.numeric(dataFR[i,110])])>0){
    ChWProtein[i]<-chseProtein[as.numeric(dataFR[i,110])]}  
  if(length(chseProtein[as.numeric(dataFR[i,111])+1])>0){
    ChYProtein[i]<-chseProtein[as.numeric(dataFR[i,111])+1]}  
  if(length(chseProtein[as.numeric(dataFR[i,112])+2])>0){
    ChLProtein[i]<-chseProtein[as.numeric(dataFR[i,112])+2]}
  A<-c()
  A<-c(cbind(ChWProtein[i],ChYProtein[i],ChLProtein[i]))
  A<-as.matrix(A)
  ChProtein[i]<-mean(na.omit(A)) 
  
  
  if(length(drink[as.numeric(dataFR[i,116])])>0){
    DWW[i]<-drink[as.numeric(dataFR[i,116])]}  
  if(length(drink[as.numeric(dataFR[i,117])+1])>0){
    DRW[i]<-drink[as.numeric(dataFR[i,117])+1]}  
  if(length(drink[as.numeric(dataFR[i,118])+2])>0){
    DWH[i]<-drink[as.numeric(dataFR[i,118])+2]} 
  if(length(drink[as.numeric(dataFR[i,119])+3])>0){
    DBR[i]<-drink[as.numeric(dataFR[i,119])+3]}  
  if(length(drink[as.numeric(dataFR[i,120])+4])>0){
    DOU[i]<-drink[as.numeric(dataFR[i,120])+4]} 
  A<-c()
  A<-c(cbind(DWW[i],DRW[i],DWH[i],DBR[i],DOU[i]))
  A<-as.matrix(A)
  
  if(length(mean(na.omit(A))*Alcgl[as.numeric(dataFR[i,115])])>0){
    Dr[i]<-mean(na.omit(A))*Alcgl[as.numeric(dataFR[i,115])]} 
  
  if(length(drinkFats[as.numeric(dataFR[i,116])])>0){
    DWWFats[i]<-drinkFats[as.numeric(dataFR[i,116])]}  
  if(length(drinkFats[as.numeric(dataFR[i,117])+1])>0){
    DRWFats[i]<-drinkFats[as.numeric(dataFR[i,117])+1]}  
  if(length(drinkFats[as.numeric(dataFR[i,118])+2])>0){
    DWHFats[i]<-drinkFats[as.numeric(dataFR[i,118])+2]} 
  if(length(drinkFats[as.numeric(dataFR[i,119])+3])>0){
    DBRFats[i]<-drinkFats[as.numeric(dataFR[i,119])+3]}  
  if(length(drinkFats[as.numeric(dataFR[i,120])+4])>0){
    DOUFats[i]<-drinkFats[as.numeric(dataFR[i,120])+4]} 
  A<-c()
  A<-c(cbind(DWWFats[i],DRWFats[i],DWHFats[i],DBRFats[i],DOUFats[i]))
  A<-as.matrix(A)
  
  if(length(mean(na.omit(A))*Alcgl[as.numeric(dataFR[i,115])])>0){
    DrFats[i]<-mean(na.omit(A))*Alcgl[as.numeric(dataFR[i,115])]} 
  
  if(length(drinkCarbs[as.numeric(dataFR[i,116])])>0){
    DWWCarbs[i]<-drinkCarbs[as.numeric(dataFR[i,116])]}  
  if(length(drinkCarbs[as.numeric(dataFR[i,117])+1])>0){
    DRWCarbs[i]<-drinkCarbs[as.numeric(dataFR[i,117])+1]}  
  if(length(drinkCarbs[as.numeric(dataFR[i,118])+2])>0){
    DWHCarbs[i]<-drinkCarbs[as.numeric(dataFR[i,118])+2]} 
  if(length(drinkCarbs[as.numeric(dataFR[i,119])+3])>0){
    DBRCarbs[i]<-drinkCarbs[as.numeric(dataFR[i,119])+3]}  
  if(length(drinkCarbs[as.numeric(dataFR[i,120])+4])>0){
    DOUCarbs[i]<-drinkCarbs[as.numeric(dataFR[i,120])+4]} 
  A<-c()
  A<-c(cbind(DWWCarbs[i],DRWCarbs[i],DWHCarbs[i],DBRCarbs[i],DOUCarbs[i]))
  A<-as.matrix(A)
  
  if(length(mean(na.omit(A))*Alcgl[as.numeric(dataFR[i,115])])>0){
    DrCarbs[i]<-mean(na.omit(A))*Alcgl[as.numeric(dataFR[i,115])]} 
  
  if(length(drinkProtein[as.numeric(dataFR[i,116])])>0){
    DWWProtein[i]<-drinkProtein[as.numeric(dataFR[i,116])]}  
  if(length(drinkProtein[as.numeric(dataFR[i,117])+1])>0){
    DRWProtein[i]<-drinkProtein[as.numeric(dataFR[i,117])+1]}  
  if(length(drinkProtein[as.numeric(dataFR[i,118])+2])>0){
    DWHProtein[i]<-drinkProtein[as.numeric(dataFR[i,118])+2]} 
  if(length(drinkProtein[as.numeric(dataFR[i,119])+3])>0){
    DBRProtein[i]<-drinkProtein[as.numeric(dataFR[i,119])+3]}  
  if(length(drinkProtein[as.numeric(dataFR[i,120])+4])>0){
    DOUProtein[i]<-drinkProtein[as.numeric(dataFR[i,120])+4]} 
  A<-c()
  A<-c(cbind(DWWProtein[i],DRWProtein[i],DWHProtein[i],DBRProtein[i],DOUProtein[i]))
  A<-as.matrix(A)
  
  if(length(mean(na.omit(A))*Alcgl[as.numeric(dataFR[i,115])])>0){
    DrProtein[i]<-mean(na.omit(A))*Alcgl[as.numeric(dataFR[i,115])]}   
  
  
  if(length(cofe[as.numeric(dataFR[i,124])])>0){
    CfGr[i]<-cofe[as.numeric(dataFR[i,124])]}  
  if(length(cofe[as.numeric(dataFR[i,125])+1])>0){
    CfNC[i]<-cofe[as.numeric(dataFR[i,125])+1]}  
  if(length(cofe[as.numeric(dataFR[i,126])+2])>0){
    CfFil[i]<-cofe[as.numeric(dataFR[i,126])+2]} 
  if(dataFR[i,127]!=0){
    CfEs[i]<-cofe[1+3]}  
  A<-c()
  A<-c(cbind(CfGr[i],CfNC[i],CfFil[i],CfEs[i]))
  A<-as.matrix(A)
  if(length(mean(na.omit(A))*cfgl[as.numeric(dataFR[i,123])])>0){
    Cff[i]<-mean(na.omit(A))*cfgl[as.numeric(dataFR[i,123])]} 
  
  if(length(cofeFats[as.numeric(dataFR[i,124])])>0){
    CfGrFats[i]<-cofeFats[as.numeric(dataFR[i,124])]}  
  if(length(cofeFats[as.numeric(dataFR[i,125])+1])>0){
    CfNCFats[i]<-cofeFats[as.numeric(dataFR[i,125])+1]}  
  if(length(cofeFats[as.numeric(dataFR[i,126])+2])>0){
    CfFilFats[i]<-cofeFats[as.numeric(dataFR[i,126])+2]} 
  if(dataFR[i,127]!=0){
    CfEsFats[i]<-cofeFats[1+3]}  
  A<-c()
  A<-c(cbind(CfGrFats[i],CfNCFats[i],CfFilFats[i],CfEsFats[i]))
  A<-as.matrix(A)
  if(length(mean(na.omit(A))*cfgl[as.numeric(dataFR[i,123])])>0){
    CffFats[i]<-mean(na.omit(A))*cfgl[as.numeric(dataFR[i,123])]}   
  
  if(length(cofeCarbs[as.numeric(dataFR[i,124])])>0){
    CfGrCarbs[i]<-cofeCarbs[as.numeric(dataFR[i,124])]}  
  if(length(cofeCarbs[as.numeric(dataFR[i,125])+1])>0){
    CfNCCarbs[i]<-cofeCarbs[as.numeric(dataFR[i,125])+1]}  
  if(length(cofeCarbs[as.numeric(dataFR[i,126])+2])>0){
    CfFilCarbs[i]<-cofeCarbs[as.numeric(dataFR[i,126])+2]} 
  if(dataFR[i,127]!=0){
    CfEsCarbs[i]<-cofeCarbs[1+3]}  
  A<-c()
  A<-c(cbind(CfGrCarbs[i],CfNCCarbs[i],CfFilCarbs[i],CfEsCarbs[i]))
  A<-as.matrix(A)
  if(length(mean(na.omit(A))*cfgl[as.numeric(dataFR[i,123])])>0){
    CffCarbs[i]<-mean(na.omit(A))*cfgl[as.numeric(dataFR[i,123])]}  
  
  if(length(cofeProtein[as.numeric(dataFR[i,124])])>0){
    CfGrProtein[i]<-cofeProtein[as.numeric(dataFR[i,124])]}  
  if(length(cofeProtein[as.numeric(dataFR[i,125])+1])>0){
    CfNCProtein[i]<-cofeProtein[as.numeric(dataFR[i,125])+1]}  
  if(length(cofeProtein[as.numeric(dataFR[i,126])+2])>0){
    CfFilProtein[i]<-cofeProtein[as.numeric(dataFR[i,126])+2]} 
  if(dataFR[i,127]!=0){
    CfEsProtein[i]<-cofeProtein[1+3]}  
  A<-c()
  A<-c(cbind(CfGr[i],CfNC[i],CfFil[i],CfEs[i]))
  A<-as.matrix(A)
  if(length(mean(na.omit(A))*cfgl[as.numeric(dataFR[i,123])])>0){
    CffProtein[i]<-mean(na.omit(A))*cfgl[as.numeric(dataFR[i,123])]}  
  
  
  if(length(tgl[as.numeric(dataFR[i,129])]*tea)>0){
    Tea[i]<-tgl[as.numeric(dataFR[i,129])]*tea} 
  
  if(length(tgl[as.numeric(dataFR[i,129])]*teaCarbs)>0){
    TeaCarbs[i]<-tgl[as.numeric(dataFR[i,129])]*teaCarbs} 
  
  
  if(dataFR[i,137]==1){
    Oliv[i]<-oilfat[1]}
  if(dataFR[i,137]==2){
    Sunoil[i]<-oilfat[2]}
  if(dataFR[i,137]==3){
    Butter[i]<-oilfat[3]}
  if(dataFR[i,137]==4){
    Margar[i]<-oilfat[4]}
  if(dataFR[i,137]==5){
    Oliv[i]<-(oilfat[1])/2
    Sunoil[i]<-(oilfat[2])/2}
  if(dataFR[i,137]==6){
    Oliv[i]<-(oilfat[1])/2
    Butter[i]<-(oilfat[3])/2}
  if(dataFR[i,137]==7){
    Oliv[i]<-(oilfat[1])/2
    Margar[i]<-(oilfat[4])/2}
  if(dataFR[i,137]==8){
    Oliv[i]<-(oilfat[1])/4
    Sunoil[i]<-(oilfat[2])/4
    Butter[i]<-(oilfat[3])/4
    Margar[i]<-(oilfat[4])/4}
  if(dataFR[i,137]==9){
    Oliv[i]<-(oilfat[1])/3
    Sunoil[i]<-(oilfat[2])/3
    Butter[i]<-(oilfat[3])/3}
  Oil<-Oliv + Sunoil + Butter + Margar
  
  
  if(dataFR[i,137]==1){
    OlivFats[i]<-oilfatFats[1]}
  if(dataFR[i,137]==2){
    SunoilFats[i]<-oilfatFats[2]}
  if(dataFR[i,137]==3){
    ButterFats[i]<-oilfatFats[3]}
  if(dataFR[i,137]==4){
    MargarFats[i]<-oilfatFats[4]}
  if(dataFR[i,137]==5){
    OlivFats[i]<-(oilfatFats[1])/2
    SunoilFats[i]<-(oilfatFats[2])/2}
  if(dataFR[i,137]==6){
    OlivFats[i]<-(oilfatFats[1])/2
    ButterFats[i]<-(oilfatFats[3])/2}
  if(dataFR[i,137]==7){
    OlivFats[i]<-(oilfatFats[1])/2
    MargarFats[i]<-(oilfatFats[4])/2}
  if(dataFR[i,137]==8){
    OlivFats[i]<-(oilfatFats[1])/4
    SunoilFats[i]<-(oilfatFats[2])/4
    ButterFats[i]<-(oilfatFats[3])/4
    MargarFats[i]<-(oilfatFats[4])/4}
  if(dataFR[i,137]==9){
    OlivFats[i]<-(oilfatFats[1])/3
    SunoilFats[i]<-(oilfatFats[2])/3
    ButterFats[i]<-(oilfatFats[3])/3}
  OilFats<-OlivFats + SunoilFats + ButterFats + MargarFats
  
  
  
  if(dataFR[i,137]==3){
    ButterCarbs[i]<-oilfatCarbs[3]}
  if(dataFR[i,137]==6){
    ButterCarbs[i]<-(oilfatCarbs[3])/2}
  if(dataFR[i,137]==8){
    ButterCarbs[i]<-(oilfatCarbs[3])/4
    if(dataFR[i,137]==9){
      ButterCarbs[i]<-(oilfatCarbs[3])/3}
    OilCarbs<-OlivCarbs + SunoilCarbs + ButterCarbs + MargarCarbs
    
    
    if(dataFR[i,137]==3){
      ButterProtein[i]<-oilfatProtein[3]}
    if(dataFR[i,137]==4){
      MargarProtein[i]<-oilfatProtein[4]}
    if(dataFR[i,137]==6){
      ButterProtein[i]<-(oilfatProtein[3])/2}
    if(dataFR[i,137]==7){
      MargarProtein[i]<-(oilfatProtein[4])/2}
    if(dataFR[i,137]==8){
      ButterProtein[i]<-(oilfatProtein[3])/4
      MargarProtein[i]<-(oilfatProtein[4])/4}
    if(dataFR[i,137]==9){
      ButterProtein[i]<-(oilfatProtein[3])/3}
    OilProtein<-OlivProtein + SunoilProtein + ButterProtein + MargarProtein
    
  }}


Br[is.na(Br)]<-0
Sftd[is.na(Sftd)]<-0
Mlk[is.na(Mlk)]<-0
Yg[is.na(Yg)]<-0
Ch[is.na(Ch)]<-0
Dr[is.na(Dr)]<-0
Cff[is.na(Cff)]<-0
Tea[is.na(Tea)]<-0
Oil[is.na(Oil)]<-0

RstCal1<- Br + Sftd + Mlk + Yg + Ch + Dr + Cff + Tea + Oil


BrFats[is.na(BrFats)]<-0
SftdFats[is.na(SftdFats)]<-0
MlkFats[is.na(MlkFats)]<-0
YgFats[is.na(YgFats)]<-0
ChFats[is.na(ChFats)]<-0
DrFats[is.na(DrFats)]<-0
CffFats[is.na(CffFats)]<-0
TeaFats[is.na(TeaFats)]<-0
OilFats[is.na(OilFats)]<-0

RstFats<- BrFats + SftdFats + MlkFats + YgFats + ChFats + DrFats + CffFats + TeaFats + OilFats


BrCarbs[is.na(BrCarbs)]<-0
SftdCarbs[is.na(SftdCarbs)]<-0
MlkCarbs[is.na(MlkCarbs)]<-0
YgCarbs[is.na(YgCarbs)]<-0
ChCarbs[is.na(ChCarbs)]<-0
DrCarbs[is.na(DrCarbs)]<-0
CffCarbs[is.na(CffCarbs)]<-0
TeaCarbs[is.na(TeaCarbs)]<-0
OilCarbs[is.na(OilCarbs)]<-0

RstCarbs<- BrCarbs + SftdCarbs + MlkCarbs + YgCarbs + ChCarbs + DrCarbs + CffCarbs + TeaCarbs + OilCarbs


BrProtein[is.na(BrProtein)]<-0
SftdProtein[is.na(SftdProtein)]<-0
MlkProtein[is.na(MlkProtein)]<-0
YgProtein[is.na(YgProtein)]<-0
ChProtein[is.na(ChProtein)]<-0
DrProtein[is.na(DrProtein)]<-0
CffProtein[is.na(CffProtein)]<-0
TeaProtein[is.na(TeaProtein)]<-0
OilProtein[is.na(OilProtein)]<-0

RstProtein<- BrProtein + SftdProtein + MlkProtein + YgProtein + ChProtein + DrProtein + CffProtein + TeaProtein + OilProtein


Totalcal1<-Cal1+ RstCal1 #Total calories reported per person calculation 1
Totalcal2<-Cal2+ RstCal1 #Total calories reported per person calculation 2

TotalFats1<-Fat1+ RstFats #Total Fats reported per person calculation 1
TotalFats2<-Fat2+ RstFats #Total Fats reported per person calculation 2

TotalCarbs1<-Carbs1+ RstCarbs #Total Carbs reported per person calculation 1
TotalCarbs2<-Carbs2+ RstCarbs #Total Carbs reported per person calculation 2

TotalProtein1<-Protein1+ RstProtein #Total Protein reported per person calculation 1
TotalProtein2<-Protein2+ RstProtein #Total Protein reported per person calculation 2

hist(Totalcal1)
hist(Totalcal2)


hist(TotalFats1)
hist(TotalFats2)


hist(TotalCarbs1)
hist(TotalCarbs2)


hist(TotalProtein1)
hist(TotalProtein2)


#Ξεκάθαρα δεν τηρείται η συνθήκη της κανονικότητας οπότε δεν μπορώ να κάνω ANOVA οπότε θα κάνω τον απαραμετρικό έλεγχο 
#Kruskal-Wallis test
#Πρώτα έλεγχος για συσχέτηση των TOTAL με την κύηση.

dataFR<-cbind(dataFR,Totalcal1,Totalcal2,TotalFats1,TotalFats2,TotalCarbs1,TotalCarbs2,TotalProtein1,TotalProtein2)
names<-colnames(dataFR)


DiscrSTATCB<-matrix(NA,nrow=24,ncol=5) #Discriptive STAT for Child bearing women
colnames(DiscrSTATCB)<-c("No.Valid.People","Mean","Sd","Median","IQR")
rownames(DiscrSTATCB)<-c("All.Data.NCurP.Tcal1","All.Data.NCurP.Tcal2", "All.Data.NCurP.TF1", "All.Data.NCurP.TF2", "All.Data.NCurP.TCarb1", "All.Data.NCurP.TCarb2", "All.Data.NCurP.TPr1", "All.Data.NCurP.TPr2", 
                         "All.Data.CurP.Tcal1", "All.Data.CurP.Tcal2",  "All.Data.CurP.TF1",  "All.Data.CurP.TF2",  "All.Data.CurP.TCarb1",  "All.Data.CurP.TCarb2",  "All.Data.CurP.TPr1",  "All.Data.CurP.TPr2",
                         "Hosp.2.NCurp.Tcal1",   "Hosp.2.NCurp.Tcal2", "Hosp.2.NCurp.TF1",   "Hosp.2.NCurp.TF2",   "Hosp.2.NCurp.TCarb1",   "Hosp.2.NCurp.TCarb2",   "Hosp.2.NCurp.TPr1",   "Hosp.2.NCurp.TPr2"  )

for(i in 1:8){
  
  DiscrSTATCB[i,1]<-length(dataFR[Curently.pregnant==2,271+i])
  DiscrSTATCB[i,2]<-  mean(dataFR[Curently.pregnant==2,271+i])
  DiscrSTATCB[i,3]<- sd(dataFR[Curently.pregnant==2,271+i])
  DiscrSTATCB[i,4]<- median(dataFR[Curently.pregnant==2,271+i])
  DiscrSTATCB[i,5]<- IQR(dataFR[Curently.pregnant==2,271+i])
  
  
  DiscrSTATCB[i+8,1]<-length(dataFR[Curently.pregnant==1,271+i])
  DiscrSTATCB[i+8,2]<-  mean(dataFR[Curently.pregnant==1,271+i])
  DiscrSTATCB[i+8,3]<- sd(dataFR[Curently.pregnant==1,271+i])
  DiscrSTATCB[i+8,4]<- median(dataFR[Curently.pregnant==1,271+i])
  DiscrSTATCB[i+8,5]<- IQR(dataFR[Curently.pregnant==1,271+i])
  
  DiscrSTATCB[i+16,1]<-length(dataFR[Hospital==2&Curently.pregnant==2,271+i])
  DiscrSTATCB[i+16,2]<-  mean(dataFR[Hospital==2&Curently.pregnant==2,271+i])
  DiscrSTATCB[i+16,3]<- sd(dataFR[Hospital==2&Curently.pregnant==2,271+i])
  DiscrSTATCB[i+16,4]<- median(dataFR[Hospital==2&Curently.pregnant==2,271+i])
  DiscrSTATCB[i+16,5]<- IQR(dataFR[Hospital==2&Curently.pregnant==2,271+i])
  
}


PropOFats1<-dataFR$TotalFats1/(dataFR$TotalFats1+dataFR$TotalCarbs1+dataFR$TotalProtein1)
PropOCarbs1<-dataFR$TotalCarbs1/(dataFR$TotalFats1+dataFR$TotalCarbs1+dataFR$TotalProtein1)
PropOProtein1<-dataFR$TotalProtein1/(dataFR$TotalFats1+dataFR$TotalCarbs1+dataFR$TotalProtein1)

PropOFats2<-dataFR$TotalFats2/(dataFR$TotalFats2+dataFR$TotalCarbs2+dataFR$TotalProtein2)
PropOCarbs2<-dataFR$TotalCarbs2/(dataFR$TotalFats2+dataFR$TotalCarbs2+dataFR$TotalProtein2)
PropOProtein2<-dataFR$TotalProtein2/(dataFR$TotalFats2+dataFR$TotalCarbs2+dataFR$TotalProtein2)
PropoF1<-matrix(NA,nrow = nrow(dataFR),ncol=1)

for (i in 1:nrow(dataFR)){
  if (PropOFats1[i]>PropOCarbs1[i] & PropOFats1[i]>PropOProtein1[i]){
    PropoF1[i]<-1
  }
  if (PropOCarbs1[i]>PropOFats1[i] & PropOCarbs1[i]>PropOProtein1[i]){
    PropoF1[i]<-2
  }
  if (PropOProtein1[i]>PropOFats1[i] & PropOProtein1[i]>PropOCarbs1[i]){
    PropoF1[i]<-3
  }
  
}
table(is.na(PropoF1))


PropoF2<-matrix(NA,nrow = nrow(dataFR),ncol=1)
for (i in 1:nrow(dataFR)){
  if (PropOFats2[i]>PropOCarbs2[i] & PropOFats2[i]>PropOProtein2[i]){
    PropoF2[i]<-1
  }
  if (PropOCarbs2[i]>PropOFats2[i] & PropOCarbs2[i]>PropOProtein2[i]){
    PropoF2[i]<-2
  }
  if (PropOProtein2[i]>PropOFats2[i] & PropOProtein2[i]>PropOCarbs2[i]){
    PropoF2[i]<-3
  }
  
}
table(is.na(PropoF2))




#STAI Y1 and Y2

Anx<-c(1,2,3,4)
NAnx<-c(4,3,2,1)
AnxQ<-c(3,4,6,7,9,12,13,14,17,18,22,24,25,28,29,31,32,35,37,38,40)
ScoreSTAI1<-matrix(0,nrow=nrow(dataFR),1)
ScoreSTAI2<-matrix(0,nrow=nrow(dataFR),1)
NoNASTAI1<-matrix(0,nrow=nrow(dataFR),1)
NoNASTAI2<-matrix(0,nrow=nrow(dataFR),1)

for(j in 1:nrow(dataFR)){
  for (i in 1:20){
    if (i%in% AnxQ){
      if(!is.na(dataFR[j,194+i])==TRUE){
        ScoreSTAI1[j]<-ScoreSTAI1[j]+as.numeric(dataFR[j,194+i])
      }else{NoNASTAI1[j]<- NoNASTAI1[j]+1 }
    }else{
      if(!is.na(dataFR[j,194+i])==TRUE){
        ScoreSTAI1[j]<-ScoreSTAI1[j]+NAnx[as.numeric(dataFR[j,194+i])]
      }else{NoNASTAI1[j]<- NoNASTAI1[j]+1 }
    }}
  for (i in 21:40){
    if (i%in% AnxQ){
      if(!is.na(dataFR[j,194+i])==TRUE){
        ScoreSTAI2[j]<-ScoreSTAI2[j]+as.numeric(dataFR[j,194+i])
      }else{NoNASTAI2[j]<- NoNASTAI2[j]+1 }
    }else{
      if(!is.na(dataFR[j,194+i])==TRUE){
        ScoreSTAI2[j]<-ScoreSTAI2[j]+NAnx[as.numeric(dataFR[j,194+i])]
      }else{NoNASTAI2[j]<- NoNASTAI2[j]+1 }
    }}
  
}

CSTAITOTAL<-matrix(0,nrow=nrow(dataFR),1)
CScoreSTAI1<-matrix(0,nrow=nrow(dataFR),1)
CScoreSTAI2<-matrix(0,nrow=nrow(dataFR),1)
for(j in 1:nrow(dataFR)){
  for (i in 1:20){
    if(NoNASTAI1[j]>0){
      if(NoNASTAI1[j]>2){
        CScoreSTAI1[j]<-NA  
      }else{CScoreSTAI1[j]<-ceiling((ScoreSTAI1[j]/(20-NoNASTAI1[j]))*20)}
      
    }else{CScoreSTAI1[j]<-ScoreSTAI1[j]}
  }
  
  
  for (i in 21:40){
    if(NoNASTAI2[j]>0){
      if(NoNASTAI2[j]>2){
        CScoreSTAI2[j]<-NA  
      }else{CScoreSTAI2[j]<-ceiling((ScoreSTAI2[j]/(20-NoNASTAI2[j]))*20)}
      
    }else{CScoreSTAI2[j]<-ScoreSTAI2[j]}}}

CSTAITOTAL<-CScoreSTAI1+CScoreSTAI2


STAITOTAL<-cbind(ScoreSTAI1,NoNASTAI1,CScoreSTAI1,ScoreSTAI2,NoNASTAI2,CScoreSTAI2,CSTAITOTAL)

STAITOTAL


dataFR<-cbind(dataFR,ScoreSTAI1,NoNASTAI1,CScoreSTAI1,ScoreSTAI2,NoNASTAI2,CScoreSTAI2,CSTAITOTAL)




datahosp2wiP<-dataFR[Hospital==2,]
datahosp2woP<-dataFR[Hospital==2&Curently.pregnant==2,]
datahosp1<-dataFR[Hospital==1,]

hist(datahosp2wiP$CScoreSTAI1)
hist(datahosp2wiP$CScoreSTAI2)
hist(datahosp2wiP$CSTAITOTAL)

hist(datahosp1$CScoreSTAI1)
hist(datahosp1$CScoreSTAI2)
hist(datahosp1$CSTAITOTAL)

hist(dataFR$CScoreSTAI1)
hist(dataFR$CScoreSTAI2)
hist(dataFR$CSTAITOTAL)

#Δεν φαίνεται να υπάρχει κανονικότητα αλλά άς κάνουμε και ένα τεστ


shapiro.test(datahosp2wiP$CScoreSTAI1)
shapiro.test(datahosp2wiP$CScoreSTAI2)
shapiro.test(datahosp2wiP$CSTAITOTAL)

shapiro.test(datahosp1$CScoreSTAI1)
shapiro.test(datahosp1$CScoreSTAI2)
shapiro.test(datahosp1$CSTAITOTAL)

shapiro.test(dataFR$CScoreSTAI1)
shapiro.test(dataFR$CScoreSTAI2)
shapiro.test(dataFR$CSTAITOTAL)




#Qualite of life 

QoLq<-c(235:270)
G1Scor<-c(100,75,50,25,0)
G2Scor<-c(0,50,100)
G3Scor<-c(0,100)
G4Scor<-c(100,80,60,40,20,0)
G5Scor<-c(0,20,40,60,80,100)
G6Scor<-c(0,25,50,75,100)
G1<-c(1, 2, 20, 22, 34, 36)
G2<-c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12 )
G3<-c(13, 14, 15, 16, 17, 18, 19)
G4<-c(21, 23, 26, 27, 30 )
G5<-c(24, 25, 28, 29, 31)
G6<-c(32, 33, 35 )

Physical.functioning<-c(3,4,5,6,7,8,9,10,11,12) 
Role.limitations.due.tophysical.health<-c(13,14,15,16) 
Role.limitations.due.to.emotional.problems<-c(17,18,19)
Energy.fatigue<-c(23,27,29,31)
Emotional.well.being<-c(24,25,26,28,30)
Social.functioning<-c(20,32)
Pain<-c(21,22)
General.health<-c(1,33,34,35,36)

scoresQoLstage1<-matrix(NA,nrow=nrow(dataFR),ncol=36)

for (j in 1:36){
  for(i in 1:nrow(dataFR)){
    if(!is.na(dataFR[i,234+j])==TRUE){ 
      if(j%in%G1){
        scoresQoLstage1[i,j]<- G1Scor[as.numeric(dataFR[i,234+j])]
      }else  if(j%in%G2){
        scoresQoLstage1[i,j]<- G2Scor[as.numeric(dataFR[i,234+j])]
      }else  if(j%in%G3){
        scoresQoLstage1[i,j]<- G3Scor[as.numeric(dataFR[i,234+j])]
      }else  if(j%in%G4){
        scoresQoLstage1[i,j]<- G4Scor[as.numeric(dataFR[i,234+j])] 
      }else  if(j%in%G5){
        scoresQoLstage1[i,j]<- G5Scor[as.numeric(dataFR[i,234+j])]   
      }else  if(j%in%G6){
        scoresQoLstage1[i,j]<- G6Scor[as.numeric(dataFR[i,234+j])]}
    }
  }
}


scoresQoLstage2<-matrix(NA,nrow=nrow(dataFR),ncol=8)

for(i in 1:nrow(dataFR)){
  
  if(sum(is.na(scoresQoLstage1[i,Physical.functioning])) <= round(length(Physical.functioning)/2,0)){
    scoresQoLstage2[i,1]<-mean(na.omit(scoresQoLstage1[i,Physical.functioning]))}
  
  if(sum(is.na(scoresQoLstage1[i,Role.limitations.due.tophysical.health])) <= round(length(Role.limitations.due.tophysical.health)/2,0)){
    scoresQoLstage2[i,2]<-mean(na.omit(scoresQoLstage1[i,Role.limitations.due.tophysical.health]))  }
  
  if(sum(is.na(scoresQoLstage1[i,Role.limitations.due.to.emotional.problems])) <= round(length(Role.limitations.due.to.emotional.problems)/2,0)){
    scoresQoLstage2[i,3]<-mean(na.omit(scoresQoLstage1[i,Role.limitations.due.to.emotional.problems]))} 
  
  if(sum(is.na(scoresQoLstage1[i,Energy.fatigue])) <= round(length(Energy.fatigue)/2,0)){
    scoresQoLstage2[i,4]<-mean(na.omit(scoresQoLstage1[i,Energy.fatigue]))}
  
  if(sum(is.na(scoresQoLstage1[i,Emotional.well.being])) <= round(length(Emotional.well.being)/2,0)){
    scoresQoLstage2[i,5]<-mean(na.omit(scoresQoLstage1[i,Emotional.well.being]))}
  
  if(sum(is.na(scoresQoLstage1[i,Social.functioning])) <= round(length(Social.functioning)/2,0)){
    scoresQoLstage2[i,6]<-mean(na.omit(scoresQoLstage1[i,Social.functioning]))}
  
  if(sum(is.na(scoresQoLstage1[i,Pain])) <= round(length(Pain)/2,0)){
    scoresQoLstage2[i,7]<-mean(na.omit(scoresQoLstage1[i,Pain]))}
  
  if(sum(is.na(scoresQoLstage1[i,General.health])) <= round(length(General.health)/2,0)){
    scoresQoLstage2[i,8]<-mean(na.omit(scoresQoLstage1[i,General.health]))}
}


colnames(scoresQoLstage2)<-c("Physical.functioning","Role.limitations.due.tophysical.health","Role.limitations.due.to.emotional.problems","Energy.fatigue","Emotional.well.being","Social.functioning","Pain","General.health")



for (i in 1:8){
  hist(scoresQoLstage2[,i])
}


#Δεν φαίνεται να υπάρχει κανονικότητα αλλά άς κάνουμε και ένα τεστ

for (i in 1:8){
  print(colnames(scoresQoLstage2[,i]))
  a<-shapiro.test(scoresQoLstage2[,i])
  print(a)
}



dataFR <- read_excel("C:/Users/Mitsos/Desktop/dataFR.xlsx")
dataFR<-cbind(dataFR,Totalcal1,Totalcal2,TotalFats1,TotalFats2,TotalCarbs1,TotalCarbs2,TotalProtein1,TotalProtein2)
dataFR<-cbind(dataFR,ScoreSTAI1,NoNASTAI1,CScoreSTAI1,ScoreSTAI2,NoNASTAI2,CScoreSTAI2,CSTAITOTAL)
dataFR<-cbind(dataFR,scoresQoLstage2)
dataFR<-cbind(dataFR,PropoF1,PropoF2 )
attach(dataFR)





factorsqa<-c(2,6,7,9,10,11,12,13,14,17,19,20,21,22,23,24,46,50,52,53,58,59,60,61,63,66,seq(68,74),seq(76,81),152,162,163,166,167,169,171,173,175,178,180,seq(182,186),188,seq(189,193),271,295,296)
contin<-c(3,18,25,47,48,49,51,55,67,seq(272,294))

all_names<-colnames(dataFR)




set.seed(nrow(dataFR))
#Ελέγχω αν υπάρχει συσχέτιση μεταξή κάθε κατηγορικής με κάθε άλλη
PV5<-matrix(NA,nrow=length(factorsqa),ncol=length(factorsqa))

for (j in 1:length(factorsqa) ){
  k<-factorsqa[j]
  for ( i in 1:length(factorsqa)){
    t<-factorsqa[i]
    nds<-cbind(dataFR[,t],dataFR[,k])
    nds<-na.omit(nds)
    chsqth2<-chisq.test(nds[,1],nds[,2],simulate.p.value=TRUE,B=1e6)
    PV5[i,j]<-chsqth2$p.value
    print(c(colnames(dataFR)[t],colnames(dataFR)[k]))
    print(chsqth2)
  }}

PV5
colnames(PV5)<-all_names[factorsqa]
row.names(PV5)<-all_names[factorsqa]


set.seed(nrow(dataFR))
#Ελέγχω αν υπάρχει συσχέτιση μεταξή κάθε κατηγορικής με κάθε συνεχείς.
PV6<-matrix(NA,nrow=length(factorsqa),ncol=length(contin))

for (j in 1:length(contin) ){
  k<-contin[j]
  for ( i in 1:length(factorsqa)){
    t<-factorsqa[i]
    nds<-cbind(dataFR[,t],dataFR[,k])
    nds<-na.omit(nds)
    a<-kruskal.test(nds[,1] ~ nds[,2], data = nds)
    PV6[i,j]<-a$p.value
    print(c(colnames(dataFR)[t],colnames(dataFR)[k]))
    print(a$p.value)
  }}


PV6
colnames(PV6)<-all_names[contin]
row.names(PV6)<-all_names[factorsqa]



set.seed(nrow(dataFR))
#Ελέγχω αν υπάρχει συσχέτιση μεταξή κάθε συνεχείς με κάθε συνεχείς.
PV7<-matrix(NA,nrow=length(contin),ncol=2*length(contin))

for (j in 1:length(contin) ){
  k<-contin[j]
  for ( i in 1:length(contin)){
    t<-contin[i]
    nds<-cbind(dataFR[,t],dataFR[,k])
    nds<-na.omit(nds)
    a<-cor.test(nds[,1] ,nds[,2],method = "spearman", exact = FALSE)
    PV7[i,2*j-1]<-a$estimate
    PV7[i,2*j]<-a$p.value
    print(c(colnames(dataFR)[t],colnames(dataFR)[k]))
    print(a$p.value)
  }}


PV7
colnames(PV7)<-c("rho.Age" ,"Age" ,"rho.No.of.attempts.IVF","No.of.attempts.IVF",
                 "rho.No.of.childbirths","No.of.childbirths", "rho.No.of.miscarriages","No.of.miscarriages", "rho.Years.of.marital.status", "Years.of.marital.status",
                 "rho.Age.of.companion","Age.of.companion", "rho.Years.of.studies", "Years.of.studies",  "rho.Years.of.working.status",  "Years.of.working.status",
                 "rho.M2.of.house", "M2.of.house", "rho.Totalcal1", "Totalcal1", "rho.Totalcal2", "Totalcal2", "rho.TotalFats1", "TotalFats1", "rho.TotalFats2", "TotalFats2",
                 "rho.TotalCarbs1", "TotalCarbs1", "rho.TotalCarbs2", "TotalCarbs2", "rho.TotalProtein1", "TotalProtein1", "rho.TotalProtein2", "TotalProtein2",
                 "rho.ScoreSTAI1", "ScoreSTAI1", "rho.NoNASTAI1", "NoNASTAI1", "rho.CScoreSTAI1", "CScoreSTAI1", "rho.ScoreSTAI2", "ScoreSTAI2", "rho.NoNASTAI2","NoNASTAI2",
                 "rho.CScoreSTAI2", "CScoreSTAI2", "rho.CSTAITOTAL", "CSTAITOTAL", "rho.Physical.functioning", "Physical.functioning",
                 "rho.Role.limitations.due.tophysical.health","Role.limitations.due.tophysical.health",
                 "rho.Role.limitations.due.to.emotional.problems","Role.limitations.due.to.emotional.problems",
                 "rho.Energy.fatigue","Energy.fatigue", "rho.Emotional.well.being", "Emotional.well.being",
                 "rho.Social.functioning" ,"Social.functioning" , "rho.Pain", "Pain",
                 "rho.General.health" ,"General.health")
row.names(PV7)<-all_names[contin]
                  
 

factorsqa<-c(2,6,7,9,10,11,12,13,14,17,19,20,21,22,23,24,46,50,52,53,58,59,60,61,63,66,seq(68,74),seq(76,81),152,162,163,166,167,169,171,173,175,178,180,seq(182,186),188,seq(189,193),271,295,296)
contin<-c(3,18,25,47,48,49,51,55,67,seq(272,294))
all_names<-colnames(dataFR)



namescont<-c("No.of.childbirths","ScoreSTAI1","NoNASTAI2","Physical.functioning")


A<-dataFR[,rownames(subset(PV5[,c(57,59)],PV5[,57]<=0.1))]
B<-dataFR[,namescont]
dtflr<-cbind(A,B)
nrow(dtflr)
dtflr<-na.omit(dtflr)
nrow(dtflr)

dataFR <- read_excel("C:/Users/Mitsos/Desktop/dataFR.xlsx")
dataFR<-cbind(dataFR,Totalcal1,Totalcal2,TotalFats1,TotalFats2,TotalCarbs1,TotalCarbs2,TotalProtein1,TotalProtein2)
dataFR<-cbind(dataFR,ScoreSTAI1,NoNASTAI1,CScoreSTAI1,ScoreSTAI2,NoNASTAI2,CScoreSTAI2,CSTAITOTAL)
dataFR<-cbind(dataFR,scoresQoLstage2)
dataFR<-cbind(dataFR,PropoF1,PropoF2 )
attach(dataFR)

