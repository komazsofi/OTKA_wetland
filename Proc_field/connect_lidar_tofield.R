"
@author: Zsofia Koma, UvA
Aim: process wetland OTKA project data step : contact field data to the lidar
"

library(readxl)
library(data.table)
library(rgdal)

library(ggplot2)
library(scales)
library(gridExtra)

#workingdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/2019Nov/"
workingdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis1/"
setwd(workingdir)

## Import
balaton <- read_excel("data_for_calib_field.xlsx",sheet = 2)
balaton_lidar=read.csv("balaton_lidar.csv")

tisza <- read_excel("data_for_calib_field.xlsx",sheet = 3)
tisza_lidar=read.csv("tisza_leafoff_lidar.csv")
tiszaon_lidar=read.csv("tisza_leafon_lidar.csv")
tisza_lidar$pointid <- NA
tiszaon_lidar$pointid <- NA

ferto <- read_excel("data_for_calib_field.xlsx",sheet = 4)
ferto_lidar=read.csv("ferto_lidar.csv")

## connact quadrat str. to lidar
lidar_balaton_merged=merge(balaton,balaton_lidar, by.x=c('point_ID','point_name'), by.y=c('point_ID','point_name'))
lidar_balaton_merged_cleaned=subset(lidar_balaton_merged,select=c(1,2,20,6:19,25:48,50:59,61:66))

lidar_ferto_merged=merge(ferto,ferto_lidar, by.x=c('point_ID','point_name'), by.y=c('point_ID','point_name'))
lidar_ferto_merged_cleaned=subset(lidar_ferto_merged,select=c(1,2,20,6:19,25:48,50:59,61:66))

lidar_tisza_merged=merge(tisza,tisza_lidar, by.x=c('point_name'), by.y=c('pont_nm'))
lidar_tisza_merged_cleaned=subset(lidar_tisza_merged,select=c(5,1,20,6:19,25:58,59:61,63,64,62))

lidar_tiszaon_merged=merge(tisza,tiszaon_lidar, by.x=c('point_name'), by.y=c('pont_nm'))
lidar_tiszaon_merged_cleaned=subset(lidar_tiszaon_merged,select=c(5,1,20,6:19,25:58,59:61,63,64,62))

lidar_balaton_merged_cleaned$season <- "leafoff"
lidar_ferto_merged_cleaned$season <-"leafoff"
lidar_tisza_merged_cleaned$season <- "leafoff"
lidar_tiszaon_merged_cleaned$season <- "leafon"

names(lidar_tisza_merged_cleaned) <- names(lidar_balaton_merged_cleaned)
names(lidar_tiszaon_merged_cleaned) <- names(lidar_balaton_merged_cleaned)
names(lidar_ferto_merged_cleaned) <- names(lidar_balaton_merged_cleaned)

all_merged=rbind(lidar_balaton_merged_cleaned,lidar_ferto_merged_cleaned,lidar_tisza_merged_cleaned) #lidar_tiszaon_merged_cleaned
write.csv(all_merged,"lidar_wquadratfield.csv")

## Pole based metrics calc

balaton_lidar$lake <- "balaton"
tisza_lidar$lake <- "tisza"
tiszaon_lidar$lake <- "tisza"
ferto_lidar$lake <- "ferto"

balaton_lidar$season <- "leafoff"
tisza_lidar$season <- "leafoff"
tiszaon_lidar$season <- "leafon"
ferto_lidar$season <- "leafoff"

tisza_lidar$pointid <- NA
tiszaon_lidar$pointid <- NA

balaton_lidar_sel=subset(balaton_lidar,select=c(1:27,32:41,43:50))
ferto_lidar_sel=subset(ferto_lidar,select=c(1:27,32:41,43:50))

tisza_lidar_sel=subset(tisza_lidar,select=c(1:25,46,26,30:42,44,45,43,47,48))
tiszaon_lidar_sel=subset(tiszaon_lidar,select=c(1:42,62,46:58,60,61,59,63,64))
names(tisza_lidar_sel) <- names(balaton_lidar_sel)
names(tiszaon_lidar_sel) <- names(balaton_lidar_sel)
names(ferto_lidar_sel) <- names(balaton_lidar_sel)

data=rbind(balaton_lidar_sel,ferto_lidar_sel,tisza_lidar_sel) #tiszaon_lidar_sel

# FHD
data$fhd_pole<-NA
fhd_pole=subset(data,select=c(29:37))

for (i in seq(1,length(data$X))) {
  print(i)
  
  v=as.numeric(fhd_pole[i,])
  
  p<- table(v)
  p <- p/sum(p)
  entropy=sum(-p*log(p))
  
  data$fhd_pole[i] <- entropy
  
}

data$fhd_pole_rao<-NA
fhd_pole=subset(data,select=c(29:37))

for (i in seq(1,length(data$X))) {
  print(i)
  
  v=as.numeric(fhd_pole[i,])
  p <- v/sum(v)
  
  if (is.na(p)==TRUE) {data$fhd_pole_rao[i] <- NA}
  
  else if (is.na(p)==FALSE) {
    
    h <- as.matrix(seq(0.5,4.5,0.5))
    D=dist(h)
    D <- as.matrix(D)
    
    quadratic_ent=c(crossprod(p, D %*% p)) / 2
    
    data$fhd_pole_rao[i] <- quadratic_ent
    
  }
  
}

# biomass sum pole contact
data$sum_pole_contact=rowSums(data[,c(29:37)])

# pole height
polecontact=subset(data,select=c(29:37))

polecontact[polecontact>0] <- 1
polecontact[,1]=polecontact[,1]*0.5
polecontact[,2]=polecontact[,2]*1
polecontact[,3]=polecontact[,3]*1.5
polecontact[,4]=polecontact[,4]*2
polecontact[,5]=polecontact[,5]*2.5
polecontact[,6]=polecontact[,6]*3
polecontact[,7]=polecontact[,7]*3.5
polecontact[,8]=polecontact[,8]*4
polecontact[,9]=polecontact[,9]*4.5

data$pole_height=apply(polecontact, 1, max)

# Export
lidar_wpointsfield=subset(data,select=c(1:27,38:49))
write.csv(lidar_wpointsfield,"lidar_wpointsfield.csv")
