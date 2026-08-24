

set.seed(nrow(dataFR))
#Ελέγχω αν υπάρχει συσχέτιση μεταξή κάθε κατηγορικής με κάθε άλλη

for (j in 1:1 ){
  zz<-subset(PV5[,c(j,59)],PV5[,j]<=0.1)
  corvar<-rownames(zz)
  k<-factorsqa[j]
  
  for ( i in 1:length(corvar)){
    t<-corvar[i]
    nds<-cbind(dataFR[,t],dataFR[,k])
    nds<-na.omit(nds)
    print(paste(c("Table of True data of ", all_names[k])))
    print(table(dataFR[,k]))
    print(paste(c("Table of True data of ", t)))
    print(table(dataFR[,t]))
    
    print(paste(c("For the varibles ", all_names[k]," and ",t," we have:" )))
    print(paste(c("Table of Data used for Chi-square test of : ", all_names[k])))
    print(table(dataFR[dataFR[,k]==nds[,2],k]))
    print(paste(c("Table of Data used for Chi-square test of : ", t )))
    print(table(dataFR[dataFR[,t]==nds[,1],t]))
    
    print(paste(c("Barplot to showcase the conection of the variables : ")))
    barplot(table(nds[,1],nds[,2]),xlab = all_names[k], ylab = t ,legend.text = TRUE,col = c(2:length(unique(nds[,1]))+1))
    
    barplot(table(nds[,1],nds[,2]),xlab = all_names[k], ylab = t  ,beside = TRUE,legend.text = TRUE,col = c(2:length(unique(nds[,1]))+1))
    
  }
  for ( i in 1:length(contin)){
    dd<-subset(PV6[,c(i,26)],PV6[,i]<=0.1)
    if(nrow(dd)==0){}
    else if(all_names[k] %in%rownames(dd)){
      t<-contin[i]
      nds1<-dataFR[,k]
      nds2<-as.numeric(dataFR[,t])
      nds<-cbind(nds1,nds2)
      nds<-nds[complete.cases(nds), ]
      nds[,2]<-as.numeric(nds[,2])
      print(paste(c("Table of True data of ", all_names[k])))
      print(table(dataFR[,k]))
      print(paste(c("Discriptive Statistics of True Data of : ", all_names[t] )))
      print(paste(c("Mean: ", mean(na.omit(dataFR[,t])))))
      print(paste(c("Standar Deviation: ", sd(na.omit(dataFR[,t])))))
      print(paste(c("For the varibles ", all_names[k]," and ",all_names[t]," we have p-value of  Kruskal-Wallis test: ", dd[all_names[k],1] )))
      print(paste(c("Table of Data used for Kruskal-Wallis test of : ", all_names[k])))
      print(table(nds[,1]))
      #print(paste(c("Discriptive Statistics of True Data of : ", all_names[t], " used for Kruskal-Wallis test." )))
      #print(paste(c("Mean: ", mean(nds[,2]))))
      #print(paste(c("Standar Deviation: ", sd(nds[,2]))))
      
      print(paste(c("Boxplot to showcase the conection of the variables : ")))
      boxplot(as.numeric(nds[,2])~nds[,1],xlab = all_names[k],ylab =all_names[t], col = c(2:length(unique(nds[,1]))+2) )
    }}
}



set.seed(nrow(dataFR))
#Ελέγχω αν υπάρχει συσχέτιση μεταξή κάθε συνεχείς με κάθε συνεχείς.
for (j in 1:length(contin) ){
  k<-contin[j]
  gg<-subset(PV7[,c(2*j,50)],PV7[,2*j]<=0.1)
  for ( i in 1:length(contin)){
  if(nrow(gg)==0){}
  else if(all_names[i] %in%rownames(gg)){ 
    t<-contin[i]
    nds<-cbind(dataFR[,t],dataFR[,k])
    nds<-na.omit(nds)
    print(all_names[t])
    print(all_names[k])
    print(paste(c("For the varibles ", all_names[k]," and ",all_names[t],
                  " we have rho of Spearman Correlation: ", PV7[all_names[k],2*j-1], 
                  " and p-value: ",PV7[all_names[k],2*j] )))
    plot(as.numeric(nds[,2])~as.numeric(nds[,1]), xlab=all_names[k], ylab=all_names[t])
  }}}

PV7


print(pie(table(na.omit(dataFR$Previously.Worked)), main = "Previously.Worked",col=rainbow(length(table(na.omit(dataFR[,k]))))))

nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"Kids"=dataFR$Kids)
nds<-na.omit(nds)
table(nds)



nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"Age.that.atempts.started"=dataFR$Age.that.atempts.started)
nds<-na.omit(nds)
table(nds)

nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"Use.eggs.of.donor"=dataFR$Use.eggs.of.donor)
nds<-na.omit(nds)
table(nds)

nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"Use.sperm.of.donor"=dataFR$Use.sperm.of.donor)
nds<-na.omit(nds)
table(nds)

nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"Previously.Worked"=dataFR$Previously.Worked)
nds<-na.omit(nds)
table(nds)

nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"Average.monthly.income"=dataFR$Average.monthly.income)
nds<-na.omit(nds)
table(nds)

Perceived.social.status

nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"Perceived.social.status"=dataFR$Perceived.social.status)
nds<-na.omit(nds)
table(nds)

nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"Social.status.growing.up"=dataFR$Social.status.growing.up)
nds<-na.omit(nds)
table(nds)

nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"Physical.activities"=dataFR$Physical.activities)
nds<-na.omit(nds)
table(nds)

nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"Stressful.events.in.life"=dataFR$Stressful.events.in.life)
nds<-na.omit(nds)
table(nds)


nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"Faithfull"=dataFR$Faithfull)
nds<-na.omit(nds)
table(nds)

nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"Did.your.faith.helped.at.your.attempt.for.childbearing"=dataFR$Did.your.faith.helped.at.your.attempt.for.childbearing)
nds<-na.omit(nds)
table(nds)

nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"How.frequently.have.sex.with.your.partner"=dataFR$How.frequently.have.sex.with.your.partner)
nds<-na.omit(nds)
table(nds)

nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"Feeling.less.feminine.because.of.infertility"=dataFR$Feeling.less.feminine.because.of.infertility)
nds<-na.omit(nds)
table(nds)

nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"Support.from.the.State"=dataFR$Support.from.the.State)
nds<-na.omit(nds)
table(nds)

nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"Support.from.third.parties"=dataFR$Support.from.third.parties)
nds<-na.omit(nds)
table(nds)
 
nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"PropoF1"=dataFR$PropoF1)
nds<-na.omit(nds)
table(nds)

nds<-data.frame("Curently.pregnant"= dataFR$Curently.pregnant,"PropoF2"=dataFR$PropoF2)
nds<-na.omit(nds)
table(nds)






library("plotrix")
library("dplyr")
library("EnvStats")
load(file = "DataFRTel8.RData") 
attach(dataFR)
nam1<-c()  
for (j in 1:length(factorsqa)){
  i<-factorsqa[j]
  k<-na.omit(dataFR[,i])
  ds<-as.numeric(table( k))
  nam<-names(table(k))
  nam1<-paste(round((ds/sum(ds)),digits = 4)*100,"%")
  nam2<- paste(nam,"is",nam1)
  
  print(all_names[i]) 
  mainname<-all_names[i]
  pie(ds,main = mainname, col = rainbow(length(ds)),labels =nam2,explode = 0.2)
  
  pie3D(ds,main = mainname ,col = hcl.colors(length(ds),"Spectral"),labels =nam2,explode = 0.05,theta = 0.8, labelcex = 0.8  )
  
  pie3D(ds,main = mainname ,col = hcl.colors(length(ds),"Spectral"),labels =nam2,theta = 0.8, labelcex = 0.8  )
  
}


  k<-na.omit(dataFR[,52])
  ds<-as.numeric(table( k))
  nam<-names(table(k))
  nam1<-paste(round((ds/sum(ds)),digits = 4)*100,"%")
  nam2<- paste(nam,"is",nam1)
  
  print(all_names[52]) 
  mainname<-all_names[52]
  pie(ds,main = mainname, col = rainbow(length(ds)),labels =nam2,explode = 0.2)
  
  pie3D(ds,main = mainname ,col = hcl.colors(length(ds),"Spectral"),labels =nam2,explode = 0.05,theta = 0.8, labelcex = 0.8  )
  
  pie3D(ds,main = mainname ,col = hcl.colors(length(ds),"Spectral"),labels =nam2,theta = 0.8, labelcex = 0.8  )

  for (j in 184:185){
    i<-j
    k<-na.omit(dataFR[,i])
    ds<-as.numeric(table( k))
    nam<-names(table(k))
    nam1<-paste(round((ds/sum(ds)),digits = 4)*100,"%")
    nam2<- paste(nam,"is",nam1)
    
    print(all_names[i]) 
    mainname<-all_names[i]
    pie(ds,main = mainname, col = rainbow(length(ds)),labels =nam2,explode = 0.2)
    
    pie3D(ds,main = mainname ,col = hcl.colors(length(ds),"Spectral"),labels =nam2,explode = 0.05,theta = 0.8, labelcex = 0.8  )
    
    pie3D(ds,main = mainname ,col = hcl.colors(length(ds),"Spectral"),labels =nam2,theta = 0.8, labelcex = 0.8  )
    
  }

for (j in 1:1){
  i<-contin[j]
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
  print(paste("For ",mainname,", we have: mean=",mean(k),", median= ",quantile(k,0.5),", 90% of observations between (",quantile(k,0.05),quantile(k,0.95),")"))
  print(paste("For ",mainname1,", we have: mean=",mean(bp1),", median= ",quantile(bp1,0.5),", 90% of observations between (",quantile(bp1,0.05),quantile(bp1,0.95),")"))
  print(paste("For ",mainname2,", we have: mean=",mean(bp2),", median= ",quantile(bp2,0.5),", 90% of observations between (",quantile(bp2,0.05),quantile(bp2,0.95),")"))
}


  dataFR <- read_excel("C:/Users/Mitsos/Desktop/dataFR.xlsx")
  dataFR<-cbind(dataFR,Totalcal1,Totalcal2,TotalFats1,TotalFats2,TotalCarbs1,TotalCarbs2,TotalProtein1,TotalProtein2)
  dataFR<-cbind(dataFR,ScoreSTAI1,NoNASTAI1,CScoreSTAI1,ScoreSTAI2,NoNASTAI2,CScoreSTAI2,CSTAITOTAL)
  dataFR<-cbind(dataFR,scoresQoLstage2)
  dataFR<-cbind(dataFR,PropoF1,PropoF2 )
  attach(dataFR)
  
  table(dataFR$Feeling.less.feminine.because.of.infertility)
