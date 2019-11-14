"
@author: Zsofia Koma, UvA
Aim: process wetland OTKA project data step : analyse field data
"

library(readxl)
library(data.table)

#workingdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/2019Nov/"
workingdir="D:/Sync/_Amsterdam/11_AndrasProject/Analysis/2019Nov/"
setwd(workingdir)

data <- read_excel("quadrat_forR.xlsx",sheet = 1)

# veg height
data$veg_height_m=data$veg_height/100

# LAI
data$sum_leaf_weight=rowSums(data[,c(44,50,53,57,62,66,70,74,78,82)])

# FHD
data$fhd_pole<-NA
fhd_pole=subset(data,select=c(12:21))

for (i in seq(1,35)) {
  print(i)
  
  v=as.numeric(fhd_pole[i,])
  
  p<- table(v)
  p <- p/sum(p)
  entropy=sum(-p*log(p))
  
  data$fhd_pole[i] <- entropy
  
}

data$fhd_bio<-NA
fhd_bio=subset(data,select=c(47,51,55,59,63,67,71,75,79))

for (i in seq(1,35)) {
  print(i)
  
  v=as.numeric(fhd_bio[i,])
  
  p<- table(v)
  p <- p/sum(p)
  entropy=sum(-p*log(p))
  
  data$fhd_bio[i] <- entropy
  
}

# Rao-quadratic FHD

data$fhd_pole_rao<-NA
fhd_pole=subset(data,select=c(12:21))

for (i in seq(1,35)) {
  print(i)
  
  v=as.numeric(fhd_pole[i,])
  p <- v/sum(v)
  
  h <- as.matrix(seq(0.5,5,0.5))
  D=dist(h)
  D <- as.matrix(D)
  
  quadratic_ent=c(crossprod(p, D %*% p)) / 2
  
  data$fhd_pole_rao[i] <- quadratic_ent
  
}

data$fhd_bio_rao<-NA
fhd_bio=subset(data,select=c(43,47,51,55,59,63,67,71,75,79))

for (i in seq(1,35)) {
  print(i)
  
  v=as.numeric(fhd_bio[i,])
  p <- v/sum(v)
  
  h <- as.matrix(seq(0.5,5,0.5))
  D=dist(h)
  D <- as.matrix(D)
  
  quadratic_ent=c(crossprod(p, D %*% p)) / 2
  
  data$fhd_bio_rao[i] <- quadratic_ent
  
}

#Shannon diversity
data$shan_div<-NA
shan_div=subset(data,select=c(39:42))

for (i in seq(1,35)) {
  print(i)
  
  v=as.numeric(shan_div[i,])
  
  p<- table(v)
  p <- p/sum(p)
  entropy=sum(-p*log(p))
  
  data$shan_div[i] <- entropy
  
}

# export field calibration between point and quadrat
data_forcalib_field=subset(data,select=c(2,4,5,6,7,28,22,23,24,25,38,88:94))
names(data_forcalib_field) <- c("location","FID","point_name","point_ID","veg_type_sp","veg_type","lai","gap_fraction","veg_h_pole","sum_pole_contacts","total weight","veg_height_m","sum_leaf_weight","fhd_pole","fhd_bio","fhd_pole_rao","fhd_bio_rao","shan_div")

write.csv(data_forcalib_field,"data_for_calib_field.csv")

# export quadrat info to lidar
data_quadtrat_tolidar=subset(data_forcalib_field,select=c(1:6,11,12,13,15,17))
write.csv(data_quadtrat_tolidar,"data_quadtrat_tolidar.csv")