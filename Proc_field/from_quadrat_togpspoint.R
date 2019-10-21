"
@author: Zsofia Koma, UvA
Aim: process wetland OTKA project data step : analyse field data
"

library(readxl)
library(data.table)
library(rgdal)

library(ggplot2)
library(scales)
library(gridExtra)

workingdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/field_measured_data/"
setwd(workingdir)

data <- read_excel("quadrat_forR.xlsx",sheet = 1)

## Height
data$veg_height_m=data$veg_height/100

model <- lm(veg_height_m ~ `veg height from pole`, data = data)
summary(model)

ggplot(data =data, aes(x=veg_height_m, y=`veg height from pole`)) + geom_point() + geom_smooth(method='lm') + theme_minimal() + xlab("Vegetation height [m]") + ylab("Pole height [m]")

## LAI
data$sum_leaf_weight=rowSums(data[,c(44,50,53,57,62,66,70,74,78,82)])

data_sel=subset(data,select=c(44,50,53,57,62,66,70,74,78,82))
data_sel$sum_leaf_weight=rowSums(data_sel[,1:10])

model_lai <- lm(sum_leaf_weight ~ `lai photo lai`, data = data)
summary(model_lai)

ggplot(data =data, aes(x=sum_leaf_weight, y=`lai photo lai`)) + geom_point() + geom_smooth(method='lm') + theme_minimal() + xlab("Sum of leaf weight") + ylab("LAI proxy")

model_lai2 <- lm(`total weight` ~ `lai photo lai`, data = data)
summary(model_lai2)

ggplot(data =data, aes(x=`total weight`, y=`lai photo lai`)) + geom_point() + geom_smooth(method='lm') + theme_minimal() + xlab("Sum of leaf weight") + ylab("LAI proxy")

model_frac <- lm(sum_leaf_weight ~ `lai photo gap fraction`, data = data)
summary(model_frac)

ggplot(data =data, aes(x=sum_leaf_weight, y=`lai photo gap fraction`)) + geom_point() + geom_smooth(method='lm') + theme_minimal() + xlab("Sum of leaf weight") + ylab("Gap franction proxy")

model_frac2 <- lm(`total weight` ~ `lai photo gap fraction`, data = data)
summary(model_frac2)

ggplot(data =data, aes(x=`total weight`, y=`lai photo gap fraction`)) + geom_point() + geom_smooth(method='lm') + theme_minimal() + xlab("Sum of leaf weight") + ylab("Gap franction proxy")

## FHD
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

data=data[data$fhd_bio>0,]

ggplot(data =data, aes(x=fhd_bio, y=fhd_pole)) + geom_point() + geom_smooth(method='lm') + theme_minimal() + xlab("FHD from biomass") + ylab("FHD from pole measure")

model_fhd <- lm(fhd_bio ~ fhd_pole, data = data)
summary(model_fhd)

# Rao-quadratic FHD

data$fhd_pole_rao<-NA
fhd_pole=subset(data,select=c(12:21))

for (i in seq(1,30)) {
  print(i)
  
  v=as.numeric(fhd_pole[i,])
  p <- v/sum(v)
  
  p <- as.matrix(p)
  D=dist(p)
  D <- as.matrix(D)
  
  quadratic_ent=c(crossprod(p, D %*% p)) / 2
  
  data$fhd_pole_rao[i] <- quadratic_ent
  
}

data$fhd_bio_rao<-NA
fhd_bio=subset(data,select=c(47,51,55,59,63,67,71,75,79))

for (i in seq(1,30)) {
  print(i)
  
  v=as.numeric(fhd_bio[i,])
  p <- v/sum(v)
  
  p <- as.matrix(p)
  D=dist(p)
  D <- as.matrix(D)
  
  quadratic_ent=c(crossprod(p, D %*% p)) / 2
  
  data$fhd_bio_rao[i] <- quadratic_ent
  
}

ggplot(data =data, aes(x=fhd_bio_rao, y=fhd_pole_rao)) + geom_point() + geom_smooth(method='lm') + theme_minimal() + xlab("Quadratic FHD from biomass") + ylab("Quadratic FHD from pole measure")

model_fhd_rao <- lm(fhd_bio_rao ~ fhd_pole_rao, data = data)
summary(model_fhd_rao)
