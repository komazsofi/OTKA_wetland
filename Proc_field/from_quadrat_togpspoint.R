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
library(ggrepel)
library(grid)

library(stargazer)

workingdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/field_measured_data/"
setwd(workingdir)

data <- read_excel("quadrat_forR.xlsx",sheet = 1)

## Height
data$veg_height_m=data$veg_height/100

model <- lm(veg_height_m ~ `veg height from pole`, data = data)
summary(model)

p1=ggplot(data =data, aes(x=veg_height_m, y=`veg height from pole`)) + geom_point(aes(colour=`1_veg_genus`)) + geom_smooth(method='lm') + theme_minimal() + xlab("Vegetation height [m]") + ylab("Pole height [m]")

## LAI
data$sum_leaf_weight=rowSums(data[,c(44,50,53,57,62,66,70,74,78,82)])

data_sel=subset(data,select=c(44,50,53,57,62,66,70,74,78,82))
data_sel$sum_leaf_weight=rowSums(data_sel[,1:10])

model_lai <- lm(sum_leaf_weight ~ `lai photo lai`, data = data[data$sum_leaf_weight>0,])
summary(model_lai)

p2=ggplot(data =data[data$sum_leaf_weight>0,], aes(x=sum_leaf_weight, y=`lai photo lai`)) + geom_point(aes(colour=`1_veg_genus`)) + geom_smooth(method='lm') + theme_minimal() + xlab("Sum of leaf weight") + ylab("LAI proxy")

model_lai2 <- lm(`total weight` ~ `lai photo lai`, data = data[data$`total weight`>0,])
summary(model_lai2)

p3=ggplot(data =data[data$`total weight`>0,], aes(x=`total weight`, y=`lai photo lai`)) + geom_point(aes(colour=`1_veg_genus`)) + geom_smooth(method='lm') + theme_minimal() + xlab("Sum of weight") + ylab("LAI proxy")

model_frac <- lm(sum_leaf_weight ~ `lai photo gap fraction`, data = data[data$sum_leaf_weight>0,])
summary(model_frac)

p4=ggplot(data =data[data$sum_leaf_weight>0,], aes(x=sum_leaf_weight, y=`lai photo gap fraction`)) + geom_point(aes(colour=`1_veg_genus`)) + geom_smooth(method='lm') + theme_minimal() + xlab("Sum of leaf weight") + ylab("Gap franction proxy")

model_frac2 <- lm(`total weight` ~ `lai photo gap fraction`, data = data[data$`total weight`>0,])
summary(model_frac2)

p5=ggplot(data=data[data$`total weight`>0,], aes(x=`total weight`, y=`lai photo gap fraction`)) + geom_point(aes(colour=`1_veg_genus`)) + geom_smooth(method='lm') + theme_minimal() + xlab("Sum of weight") + ylab("Gap franction proxy")

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

p6=ggplot(data =data[data$fhd_bio>0,], aes(x=fhd_bio, y=fhd_pole)) + geom_point(aes(colour=`1_veg_genus`)) + geom_smooth(method='lm') + theme_minimal() + xlab("FHD from biomass") + ylab("FHD from pole measure")

model_fhd <- lm(fhd_bio ~ fhd_pole, data = data[data$fhd_bio>0,])
summary(model_fhd)

# Rao-quadratic FHD

data$fhd_pole_rao<-NA
fhd_pole=subset(data,select=c(12:21))

for (i in seq(1,35)) {
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

for (i in seq(1,35)) {
  print(i)
  
  v=as.numeric(fhd_bio[i,])
  p <- v/sum(v)
  
  p <- as.matrix(p)
  D=dist(p)
  D <- as.matrix(D)
  
  quadratic_ent=c(crossprod(p, D %*% p)) / 2
  
  data$fhd_bio_rao[i] <- quadratic_ent
  
}

p7=ggplot(data =data[data$fhd_bio_rao>0,], aes(x=fhd_bio_rao, y=fhd_pole_rao)) + geom_point(aes(colour=`1_veg_genus`)) + geom_smooth(method='lm') + theme_minimal() + xlab("Quadratic FHD from biomass") + ylab("Quadratic FHD from pole measure")

model_fhd_rao <- lm(fhd_bio_rao ~ fhd_pole_rao, data = data[data$fhd_bio_rao>0,])
summary(model_fhd_rao)

fig=grid.arrange(
  p1,
  p2,
  p3,
  p4,
  p5,
  p6,
  p7,
  ncol=3,
  nrow=3
)

stargazer(model,model_lai,model_lai2,model_frac,model_frac2,model_fhd,model_fhd_rao, type = "text",out="point_to_quadrat.txt")
