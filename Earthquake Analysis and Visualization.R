library("EnvStats")
library(lubridate)
library(openxlsx)
library(leaflet)
library("tidyverse")
library("ggplot2")
library("magrittr")
library("ggmap")
library('dplyr')

Earthquakes_v3 <- read.csv("C:/Users/Mitsos/Desktop/Earthquakes_v3.csv")

E3<-Earthquakes_v3 

E3$DATETIME<-parse_date_time(E3$DATETIME, orders = c("mdy HM", "ymd HMS"), tz = "UTC")  




subs1<-subset(E3, E3$MAGNITUDE>=4.5)



year_u <- unique(year(E3$DATETIME))

averages <- data.frame()
for(i in 1965:max(year_u)){
  

  subsets <- subset(E3$MAGNITUDE, year(E3$DATETIME) == i)
  

  average <- mean(subsets)
  var1<-var(subsets)
  

  temp <- data.frame( i, t_mean = average, t_var=var1,length(subsets))
  

  averages <- rbind(averages, temp )}

averages2 <- data.frame()
for(i in 1965:max(year_u)){
  
  
  subsets <- subset(subs1$MAGNITUDE, year(subs1$DATETIME) == i)
  
  
  average <- mean(subsets)
  var1<-var(subsets)
  
  
  temp <- data.frame( i, t_mean = average, t_var=var1,length(subsets), maxmagn= max(subsets))
  
 
  averages2 <- rbind(averages2, temp )}


averages<-cbind(averages,averages2 )


m_averages <- data.frame()
for(i in 1:12){
  
  
  subsets <- subset(E3$MAGNITUDE, month(E3$DATETIME) == i)
  

  average <- mean(subsets)
  var1<-var(subsets)
  
  
  temp <- data.frame( i, t_mean = average, t_var=var1,length(subsets))
  

  m_averages <- rbind(m_averages, temp )}

m_averages2 <- data.frame()

for(i in 1:12){
  

  subsets <- subset(subs1$MAGNITUDE, month(subs1$DATETIME) == i)
  

  average <- mean(subsets)
  var1<-var(subsets)
  

  temp <- data.frame( i, t_mean = average, t_var=var1,length(subsets))
  
  
  m_averages2 <- rbind(m_averages2, temp )}

m_averages<-cbind(m_averages,m_averages2)



d_averages <- data.frame()
for(i in 1:31){
  

  subsets <- subset(E3$MAGNITUDE, day(E3$DATETIME) == i)
  
 
  average <- mean(subsets)
  var1<-var(subsets)
  
  
  temp <- data.frame( i, t_mean = average, t_var=var1,length(subsets))
  

  d_averages <- rbind(d_averages, temp )}


d_averages2 <- data.frame()
for(i in 1:31){
  
  
  subsets <- subset(subs1$MAGNITUDE, day(subs1$DATETIME) == i)
  
 
  average <- mean(subsets)
  var1<-var(subsets)
  
  
  temp <- data.frame( i, t_mean = average, t_var=var1,length(subsets))
  
  
  d_averages2 <- rbind(d_averages2, temp )}

d_averages<-cbind(d_averages,d_averages2)







avgmagn_by_year2 <- averages2[order(-averages2$t_mean,-averages2$length.subsets.),]
max_magn_by_year<-averages2[order(-averages2$maxmagn),]
top_avgmagn_by_year2<-head(avgmagn_by_year2,0.10*nrow(avgmagn_by_year2))
top_max_magn_by_year<-head(max_magn_by_year,0.10*nrow(max_magn_by_year))




plot.ecdf( averages$t_mean)  
plot.ecdf( averages2$t_mean)

epdfPlot(averages$t_mean)
epdfPlot(averages2$t_mean)
pr2001<-subset(E3,year(E3$DATETIME)<2001 )
aft2001<-subset(E3,year(E3$DATETIME)>2000)
normalpr2001<-(pr2001$MAGNITUDE - mean(pr2001$MAGNITUDE))/sd(pr2001$MAGNITUDE)
normalaft2001<-(aft2001$MAGNITUDE- mean(aft2001$MAGNITUDE))/sd(aft2001$MAGNITUDE)
  
ks_tst<-  ks.test(normalpr2001, normalaft2001)
print(ks_tst)

  
epdfPlot(pr2001$MAGNITUDE)
epdfPlot(aft2001$MAGNITUDE)

df_compare1 <- data.frame(
  value = c(normalpr2001, normalaft2001),
  group = c(rep("prior 2001 normalized gathered data", length(normalpr2001)), 
            rep("2001 -2003 normalized gathered data", length(normalaft2001)))
)

# Density plot
ggplot(df_compare1, aes(x = value, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution Comparison",
       subtitle = paste("P-value:", format(4.174e-175, scientific = TRUE)),
       x = "Value",
       y = "Density") +
  theme_minimal()

# QQ plot
qqplot(normalpr2001, normalaft2001, 
       main = "Q-Q Plot",
       xlab = "prior 2001 normalized gathered data Quantiles",
       ylab = "2001 -2003 normalized gathered data Quantiles")
abline(0, 1, col = "red", lwd = 2)




pr_magnitude <- pr2001$MAGNITUDE
aft_magnitude <- aft2001$MAGNITUDE

df_compare <- data.frame(
  value = c(pr_magnitude, aft_magnitude),
  group = c(rep("prior 2001 gathered data", length(pr_magnitude)), 
            rep("2001 -2003 gathered data", length(aft_magnitude)))
)

# Density plot
ggplot(df_compare, aes(x = value, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution Comparison",
       subtitle = paste("P-value:", format(4.174e-175, scientific = TRUE)),
       x = "Value",
       y = "Density") +
  theme_minimal()

# QQ plot
qqplot(pr_magnitude, aft_magnitude, 
       main = "Q-Q Plot",
       xlab = "prior 2001 gathered data Quantiles",
       ylab = "2001 -2003 gathered data Quantiles")
abline(0, 1, col = "red", lwd = 2)



pal <- colorFactor(
  palette = "viridis",
  domain = year(subs1$DATETIME)
)

leaflet(subs1) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~LONG,
    lat = ~LAT,
    radius = ~MAGNITUDE * 0.05,  # Size by magnitude
    color = ~pal(year(subs1$DATETIME)),
    fillOpacity = 0.7,
    popup = ~paste(
      "Year:", year(subs1$DATETIME),
      "<br>Magnitude:", MAGNITUDE,
      "<br>Depth:", DEPTH, "km"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~year(subs1$DATETIME),
    title = "Year",
    position = "bottomright"
  )



#top 10% years by mean magnitude and earthquakes more than 4.5 Richter

subs2<- subset(subs1, year(subs1$DATETIME) %in% top_avgmagn_by_year2$i)




pal <- colorFactor(
  palette = "viridis",
  domain = year(subs2$DATETIME)
)

leaflet(subs2) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~LONG,
    lat = ~LAT,
    radius = ~MAGNITUDE * 0.05,  
    color = ~pal(year(subs2$DATETIME)),
    fillOpacity = 0.7,
    popup = ~paste(
      "Year:", year(subs2$DATETIME),
      "<br>Magnitude:", MAGNITUDE,
      "<br>Depth:", DEPTH, "km"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~year(subs2$DATETIME),
    title = "Year",
    position = "bottomright"
  )


subs3<-data.frame()
for(i in 1965:max(year_u)){

  subsets <- subset(E3, year(E3$DATETIME) == i)
  subsets <- subset(subsets, subsets$MAGNITUDE == max(subsets$MAGNITUDE))
  subs3 <- rbind(subs3, na.omit(subsets) )}
Max_eq_by_year<-subs3 

avgmagn_by_year2 <- averages2[order(-averages2$t_mean,-averages2$length.subsets.),]
max_magn_by_year<-averages2[order(-averages2$maxmagn),]
top_avgmagn_by_year2<-head(avgmagn_by_year2,0.10*nrow(avgmagn_by_year2))
top_max_magn_by_year<-head(max_magn_by_year,0.10*nrow(max_magn_by_year))


subs4<-data.frame()
for(i in 1965:max(year_u)){
  subsets <- subset(E3, year(E3$DATETIME) == i)
  subsets <- subsets[order(-subsets$MAGNITUDE),]
  top_subsets<-head(subsets,0.10*nrow(subsets))
  subs4 <- rbind(subs4, na.omit(top_subsets) )}

Top_10p_eq_by_year<-subs4
  
  
pal <- colorFactor(
  palette = "viridis",
  domain = year(subs3$DATETIME)
)

leaflet(subs3) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~LONG,
    lat = ~LAT,
    radius = ~MAGNITUDE * 0.05,  
    color = ~pal(year(subs3$DATETIME)),
    fillOpacity = 0.7,
    popup = ~paste(
      "Year:", year(subs3$DATETIME),
      "<br>Magnitude:", MAGNITUDE,
      "<br>Depth:", DEPTH, "km"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~year(subs3$DATETIME),
    title = "Year",
    position = "bottomright"
  )



write.xlsx(averages, 'eqbyyear.xlsx')
write.xlsx(m_averages, 'eqbymonth.xlsx')
write.xlsx(d_averages, 'eqbyday.xlsx')
write.xlsx(E3,'E3.xlsx')
write.xlsx(Max_eq_by_year,'Max_eq_by_year.xlsx')
write.xlsx(Top_10p_eq_by_year,'Top_10p_eq_by_year.xlsx')





