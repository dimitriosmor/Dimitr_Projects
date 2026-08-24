#for (j in 1:length(factorsqa)){
library("plotrix")
library("dplyr")
library("EnvStats")

dataFR <- read_excel("C:/Users/Mitsos/Desktop/dataFR.xlsx")
dataFR<-cbind(dataFR,Totalcal1,Totalcal2,TotalFats1,TotalFats2,TotalCarbs1,TotalCarbs2,TotalProtein1,TotalProtein2)
dataFR<-cbind(dataFR,ScoreSTAI1,NoNASTAI1,CScoreSTAI1,ScoreSTAI2,NoNASTAI2,CScoreSTAI2,CSTAITOTAL)
dataFR<-cbind(dataFR,scoresQoLstage2)
dataFR<-cbind(dataFR,PropoF1,PropoF2 )
attach(dataFR)

load(file = "DataFRTel9.RData") 
attach(dataFR)  
factorsqa2<-c(4,5,6,7,9,10,11,12,13,14,17,19,20,21,22,23,24,46,50,52,53,56,57,58,59,60,61,63,66,seq(68,74),seq(76,81),96,97,100,101,102,103,104,105,106,108,109,seq(110,112),114,115,seq(116,119),122,123,seq(124,126),128,129,130,131,132,134,135,136,137,138,141,142,152,162,163,165,166,167,169,171,173,175,178,180,seq(182,186),188,seq(189,193),295,296)
contin2<-c(3,18,25,47,48,49,51,55,62,65,67,139,140,143,144,seq(146,151),seq(272,294))
factorsqa3<-c(2,120,145,271)
for (j in 1:length(factorsqa2)){
  
  i<-factorsqa2[j]
  k<-na.omit(dataFR[,i])
  k1<-na.omit(dataFR[Curently.pregnant==1,i])
  k2<-na.omit(dataFR[Curently.pregnant==2,i])
  
  ds<-as.numeric(table( k))
  ds1<-as.numeric(table( k1))
  ds2<-as.numeric(table( k2))
  
  nam<-names(table(k))
  nam1<-paste(round((ds/sum(ds)),digits = 4)*100,"%")
  nam2<- paste(nam,"is",nam1)
  
  namcp<-names(table(k1))
  nam11<-paste(round((ds1/sum(ds1)),digits = 4)*100,"%")
  nam21<- paste(namcp,"is",nam11)
  
  namcnp<-names(table(k2))
  nam12<-paste(round((ds2/sum(ds2)),digits = 4)*100,"%")
  nam22<- paste(namcnp,"is",nam12)
  
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  print(all_names[i]) 
  mainname<-all_names[i]
  mainname1<-paste(all_names[i], " For Currently Pregnant Women")
  mainname2<-paste(all_names[i], " For Currently Not Pregnant Women")
  
  pie3D(ds,main = mainname ,col = hcl.colors(length(ds),"Spectral"),labels =nam2,theta = 0.8,labelrad=2, labelcex = 0.8  )
  prop.table( table(as.vector( sapply( k, unlist ))) )
  prop.table(table( k))
  pie3D(ds1,main = mainname1 ,col = hcl.colors(length(ds1),"Spectral"),labels =nam21,theta = 0.8,labelrad=2, labelcex = 0.8  )
  prop.table(table( k1))
  pie3D(ds2,main = mainname2 ,col = hcl.colors(length(ds2),"Spectral"),labels =nam22,theta = 0.8,labelrad=4, labelcex = 0.8  )
  prop.table(table( k2))
  
  print(mainname2)
  print(nam22)
 
}



for (j in 1:length(contin2)){
  i<-contin2[j]
  k<-na.omit(dataFR[,i])
  k<-as.numeric(k)
  print(all_names[i]) 
  mainname<-paste(all_names[i], "For All The Women")
  mainname1<-paste(all_names[i],"For Currently Pregnant Women")
  mainname2<-paste(all_names[i],"For Currently Not Pregnant Women")
  
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  epdfPlot(k,main = mainname,xlab = "",cex.main=0.7)
  abline(v = mean(k),col = "darkgreen")
  abline(v = quantile(k,0.5),col = "red")
  abline(v = quantile(k,0.05),col = "steelblue1")
  abline(v = quantile(k,0.95),col = "steelblue1")
  
  bp1<-na.omit(dataFR[Curently.pregnant==1,i])
  bp2<-na.omit(dataFR[Curently.pregnant==2,i])
  bp1<-as.numeric(bp1)
  bp2<-as.numeric(bp2)
  
  epdfPlot(bp1,main = mainname1,xlab = "",cex.main=0.7)
  abline(v = mean(bp1),col = "darkgreen")
  abline(v = quantile(bp1,0.5),col = "red")
  abline(v = quantile(bp1,0.05),col = "steelblue1")
  abline(v = quantile(bp1,0.95),col = "steelblue1")
  
  epdfPlot(bp2,main = mainname2,xlab = "",cex.main=0.7)
  abline(v = mean(bp2),col = "darkgreen")
  abline(v = quantile(bp2,0.5),col = "red")
  abline(v = quantile(bp2,0.05),col = "steelblue1")
  abline(v = quantile(bp2,0.95),col = "steelblue1")
  print(paste("Για το διάγραμμα με τίτλο ",mainname,", έχουμε: μέση τιμή=",mean(k),", διάμεσο= ",quantile(k,0.5),"και 90% των παρατηρήσεων βρίσκονται στο διάστημα (",quantile(k,0.05),",",quantile(k,0.95),")"))
  print(paste("Για το διάγραμμα με τίτλο  ",mainname1,", έχουμε: μέση τιμή=",mean(bp1),", διάμεσο= ",quantile(bp1,0.5),"και 90% των παρατηρήσεων βρίσκονται στο διάστημα (",quantile(bp1,0.05),",",quantile(bp1,0.95),")"))
  print(paste("Για το διάγραμμα με τίτλο  ",mainname2,", έχουμε: μέση τιμή=",mean(bp2),", διάμεσο= ",quantile(bp2,0.5),"και 90% των παρατηρήσεων βρίσκονται στο διάστημα (",quantile(bp2,0.05),",",quantile(bp2,0.95),")"))
}



factorsqa2<-c(4,5,6,7,9,10,11,12,13,14,17,19,20,21,22,23,24,46,50,52,53,56,57,58,59,60,61,63,66,seq(68,74),seq(76,81),96,97,100,101,102,103,104,105,106,108,109,seq(110,112),114,115,seq(116,119),122,123,seq(124,126),128,129,130,131,132,134,135,136,137,138,141,142,152,162,163,165,166,167,169,171,173,175,178,180,seq(182,186),188,seq(189,193),295,296)
contin2<-c(3,18,25,47,48,49,51,55,62,65,67,139,140,143,seq(146,151),seq(272,294))
factorsqa3<-c(2,120,145,271)

factorsqa4<-c(2,4,5,6,7,9,10,11,12,13,14,17,19,20,21,22,23,24,46,50,52,53,56,57,58,59,60,61,63,66,seq(68,74),seq(76,81),96,97,100,101,102,103,104,105,106,108,109,seq(110,112),114,115,seq(116,120),122,123,seq(124,126),128,129,130,131,132,134,135,136,137,138,141,142,152,162,163,165,166,167,169,171,173,175,178,180,seq(182,186),188,seq(189,193),271,295,296)


set.seed(nrow(dataFR))
#Ελέγχω αν υπάρχει συσχέτιση μεταξή κάθε κατηγορικής με κάθε άλλη
PV51<-matrix(NA,nrow=length(factorsqa4),ncol=3*length(factorsqa4))

for (j in 1:length(factorsqa4) ){
  k<-factorsqa4[j]
  for ( i in 1:length(factorsqa4)){
    t<-factorsqa4[i]
    nds<-cbind(dataFR[,t],dataFR[,k],dataFR$Curently.pregnant)
    nds<-na.omit(nds)
    
    poserror<-tryCatch(chisq.test(nds[,1],nds[,2],simulate.p.value=TRUE,B=10000), error=function(e) e)
    if(!inherits(poserror,"error")){
    chsqth2<-chisq.test(nds[,1],nds[,2],simulate.p.value=TRUE,B=10000)
    PV51[i,3*j-2]<-chsqth2$p.value}
    
    print(c(colnames(dataFR)[t],colnames(dataFR)[k]))
    print(PV51[i,3*j-2])
    
    
    nds1<-nds[nds[,3]==1,]
    poserror<-tryCatch(chisq.test(nds1[,1],nds1[,2],simulate.p.value=TRUE,B=10000), error=function(e) e)
    if(!inherits(poserror,"error")){
      chsqth2<-chisq.test(nds1[,1],nds1[,2],simulate.p.value=TRUE,B=10000)
      PV51[i,3*j-1]<-chsqth2$p.value}
    
    print(paste(colnames(dataFR)[t]," ",colnames(dataFR)[k]," ","Curently Pregnant" ))
    print(PV51[i,3*j-1])
    
    
    nds2<-nds[nds[,3]==2,]
    poserror<-tryCatch(chisq.test(nds2[,1],nds2[,2],simulate.p.value=TRUE,B=10000), error=function(e) e)
    if(!inherits(poserror,"error")){
      chsqth2<-chisq.test(nds2[,1],nds2[,2],simulate.p.value=TRUE,B=10000)
      PV51[i,3*j]<-chsqth2$p.value}
    print(paste(colnames(dataFR)[t]," ",colnames(dataFR)[k]," ","Curently Not Pregnant" ))
    print(PV51[i,3*j])
  
    }}


namesPV51<-c()
for (j in 1:length(factorsqa4) ){
  k<-factorsqa4[j]
  namesPV51<-cbind(namesPV51,colnames(dataFR)[k],paste(colnames(dataFR)[k],"F.C.P"),paste(colnames(dataFR)[k],"F.C.N.P."))
}

PV51
colnames(PV51)<-namesPV51
row.names(PV51)<-all_names[factorsqa4]


set.seed(nrow(dataFR))
#Ελέγχω αν υπάρχει συσχέτιση μεταξή κάθε κατηγορικής με κάθε συνεχείς.
PV61<-matrix(NA,nrow=length(factorsqa4),ncol=3*length(contin2))

for (j in 1:length(contin2) ){
  k<-contin2[j]
  for ( i in 1:length(factorsqa4)){
    t<-factorsqa4[i]
    nds<-cbind(dataFR[,t],dataFR[,k],dataFR$Curently.pregnant)
    nds<-na.omit(nds)
    
    poserror<-tryCatch(a<-kruskal.test(nds[,1] ~ nds[,2], data = nds), error=function(e) e)
    if(!inherits(poserror,"error")){
        a<-kruskal.test(nds[,1] ~ nds[,2], data = nds)
        PV61[i,3*j-2]<-a$p.value}else{PV61[i,3*j-2]<-NA}
    print(c(colnames(dataFR)[t],colnames(dataFR)[k]))
    print(PV61[i,3*j-2])
    nds1<-nds[nds[,3]==1,]
    
    poserror<-tryCatch(a<-kruskal.test(nds1[,1] ~ nds1[,2], data = nds1), error=function(e) e)
    if(!inherits(poserror,"error")){
        a<-kruskal.test(nds1[,1] ~ nds1[,2], data = nds1)
        PV61[i,3*j-1]<-a$p.value}else{PV61[i,3*j-1]<-NA}
    print(paste(colnames(dataFR)[t]," ",colnames(dataFR)[k]," ","Curently Pregnant" ))
    print(PV61[i,3*j-1])
    
    nds2<-nds[nds[,3]==2,]
    poserror<-tryCatch(a<-kruskal.test(nds2[,1] ~ nds2[,2], data = nds2), error=function(e) e)
    if(!inherits(poserror,"error")){
        a<-kruskal.test(nds2[,1] ~ nds2[,2], data = nds2)
        PV61[i,3*j]<-a$p.value}else{PV61[i,3*j]<-NA}
    print(paste(colnames(dataFR)[t]," ",colnames(dataFR)[k]," ","Curently Not Pregnant" ))
    print(PV61[i,3*j])
  }}

namesPV61<-c()
for (j in 1:length(contin2) ){
  k<-contin2[j]
  namesPV61<-cbind(namesPV61,colnames(dataFR)[k],paste(colnames(dataFR)[k]," F.C.P." ),paste(colnames(dataFR)[k]," F.C.N.P."))
 }
PV61
colnames(PV61)<-namesPV61
row.names(PV61)<-all_names[factorsqa4]



set.seed(nrow(dataFR))
#Ελέγχω αν υπάρχει συσχέτιση μεταξή κάθε συνεχείς με κάθε συνεχείς.
PV71<-matrix(NA,nrow=length(contin2),ncol=3*length(contin2))
PV71rho<-matrix(NA,nrow=length(contin2),ncol=3*length(contin2))

for (j in 1:length(contin2) ){
  k<-contin2[j]
  for ( i in 1:length(contin2)){
    t<-contin2[i]
    nds<-cbind(as.numeric(dataFR[,t]),as.numeric(dataFR[,k]),dataFR$Curently.pregnant)
    nds<-na.omit(nds)
    
    poserror<-tryCatch(cor.test(nds[,1] ,nds[,2],method = "spearman", exact = FALSE), error=function(e) e)
    if(!inherits(poserror,"error")){
    a<-cor.test(nds[,1] ,nds[,2],method = "spearman", exact = FALSE)
    PV71rho[i,3*j-2]<-a$estimate
    PV71[i,3*j-2]<-a$p.value}

    
    print(paste(colnames(dataFR)[t]," and ",colnames(dataFR)[k]))
    print(paste("rho = ",PV71rho[i,3*j-2], " and ","p-value= ",PV71[i,3*j-2]))
    
    nds1<-nds[nds[,3]==1,]
    poserror<-tryCatch(a<-cor.test(nds1[,1] ,nds1[,2],method = "spearman", exact = FALSE), error=function(e) e)
    if(!inherits(poserror,"error")){
      a<-cor.test(nds1[,1] ,nds1[,2],method = "spearman", exact = FALSE)
      PV71rho[i,3*j-1]<-a$estimate
      PV71[i,3*j-1]<-a$p.value}
    
    print(paste(colnames(dataFR)[t]," and ",colnames(dataFR)[k]," for "," Curently Pregnant"  ))
    print(paste("rho = ",PV71rho[i,3*j-2], " and ","p-value= ",PV71[i,3*j-2]) )
    
    nds2<-nds[nds[,3]==2,]
    poserror<-tryCatch(a<-cor.test(nds2[,1] ,nds2[,2],method = "spearman", exact = FALSE), error=function(e) e)
    if(!inherits(poserror,"error")){
      a<-cor.test(nds2[,1] ,nds2[,2],method = "spearman", exact = FALSE)
      PV71rho[i,3*j]<-a$estimate
      PV71[i,3*j]<-a$p.value}
    
    print(paste(colnames(dataFR)[t]," and ",colnames(dataFR)[k]," for "," Curently Not Pregnant"  ))
    print(paste("rho = ",PV71rho[i,3*j-2], " and ","p-value= ",PV71[i,3*j-2]))

  }}



namesPV71<-c()
namesPV71rho<-c()
for (j in 1:length(contin2) ){
  k<-contin2[j] 
  namesPV71<-cbind(namesPV71,colnames(dataFR)[k],paste(colnames(dataFR)[k]," for ","Curently Pregnant" ),paste(colnames(dataFR)[k]," for ","Curently Not Pregnant" ))
  namesPV71rho<-cbind(namesPV71rho,paste("rho.",colnames(dataFR)[k]),paste("rho.",colnames(dataFR)[k]," for ","Curently Pregnant" ),paste("rho.",colnames(dataFR)[k]," for ","Curently Not Pregnant"))
}
colnames(PV71)<-namesPV71
colnames(PV71rho)<-namesPV71rho
row.names(PV71)<-all_names[contin2]
row.names(PV71rho)<-all_names[contin2]


PV71
PV71rho


factorsqa4<-c(2,4,5,6,7,9,10,11,12,13,14,17,19,20,21,22,23,24,46,50,52,53,56,57,58,59,60,61,63,66,seq(68,74),seq(76,81),96,97,100,101,102,103,104,105,106,108,109,seq(110,112),114,115,seq(116,120),122,123,seq(124,126),128,129,130,131,132,134,135,136,137,138,141,142,152,162,163,165,166,167,169,171,173,175,178,180,seq(182,186),188,seq(189,193),271,295,296)
load(file = "DataFRTel10.RData") 


PV61T<-t(PV61)
for (j in 1:length(factorsqa4)) {
  t<-factorsqa4[j]
  print(all_names[t])
  zz1<-subset(PV51[,c(3*j-2,3*j-1,3*j)],PV51[,3*j-2]<=0.05|PV51[,3*j-1]<=0.05|PV51[,3*j]<=0.05)
  row.names(zz1)<-row.names(subset(PV51[,c(3*j-2,3*j-1,3*j)],PV51[,3*j-2]<=0.05|PV51[,3*j-1]<=0.05|PV51[,3*j]<=0.05))
  print(zz1)

  
  zz2<-subset(PV61T[c(3*j-2,3*j-1,3*j),j],PV61T[,3*j-2]<=0.05|PV61T[,3*j-1]<=0.05|PV61T[,3*j]<=0.05)
  row.names(zz2)<-row.names(subset(PV61[,c(3*j-2,3*j-1,3*j)],PV61[,3*j-2]<=0.05|PV61[,3*j-1]<=0.05|PV61[,3*j]<=0.05))
  print(zz2)
  
}





for (j in 1:length(contin2) ){
  t<-contin2[j]
  print(all_names[t])
  gg<-subset(PV71[,c(2*j-1,2*j)],PV71[,2*j]<=0.05)
  print(gg)
  
}




PV61T[,105]










PV61T<-t(PV61)  
for (j in 1:length(factorsqa4)) {
  t<-factorsqa4[j]
  print(all_names[t])
  zz<-subset(PV51[,j],PV51[,j]<=0.05)
  print(zz)
  dd<-subset(PV61T[,c(j,59)],PV61T[,j]<=0.05)
  print(dd)
  
}



for (j in 1:length(contin2) ){
  t<-contin2[j]
  print(all_names[t])
  gg<-subset(PV71[,c(2*j-1,2*j)],PV71[,2*j]<=0.05)
  print(gg)
  
}
















