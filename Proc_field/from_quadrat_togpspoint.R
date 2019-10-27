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

p1=ggplot(data =data, aes(x=veg_height_m, y=`veg height from pole`)) + geom_point(aes(colour=veg_type_2)) + geom_smooth(method='lm') + theme_minimal() + xlab("Vegetation height [m]") + ylab("Pole height [m]")

## LAI
data$sum_leaf_weight=rowSums(data[,c(44,50,53,57,62,66,70,74,78,82)])

data_sel=subset(data,select=c(44,50,53,57,62,66,70,74,78,82))
data_sel$sum_leaf_weight=rowSums(data_sel[,1:10])

model_lai <- lm(sum_leaf_weight ~ `lai photo lai`, data = data[data$veg_type_2=="phragmites",])
summary(model_lai)

model_lai_h <- lm(`lai photo lai` ~ sum_leaf_weight+veg_height_m, data = data[data$veg_type_2=="phragmites",])
summary(model_lai_h)

p2=ggplot(data =data[data$veg_type_2=="phragmites",], aes(x=sum_leaf_weight, y=`lai photo lai`)) + geom_point(aes(colour=veg_type)) + geom_smooth(method='lm') + theme_minimal() + xlab("Sum of leaf weight") + ylab("LAI proxy")

model_lai2 <- lm(`total weight` ~ `lai photo lai`, data = data[data$veg_type_2=="phragmites",])
summary(model_lai2)

p3=ggplot(data =data[data$veg_type_2=="phragmites",], aes(x=`total weight`, y=`lai photo lai`)) + geom_point(aes(colour=veg_type)) + geom_smooth(method='lm') + theme_minimal() + xlab("Biomass (total weight)") + ylab("LAI proxy")

model_frac <- lm(sum_leaf_weight ~ `lai photo gap fraction`, data = data[data$veg_type_2=="phragmites",])
summary(model_frac)

p4=ggplot(data =data[data$veg_type_2=="phragmites",], aes(x=sum_leaf_weight, y=`lai photo gap fraction`)) + geom_point(aes(colour=veg_type)) + geom_smooth(method='lm') + theme_minimal() + xlab("Sum of leaf weight") + ylab("Gap franction proxy")

model_frac2 <- lm(`total weight` ~ `lai photo gap fraction`, data = data[data$veg_type_2=="phragmites",])
summary(model_frac2)

p5=ggplot(data=data[data$veg_type_2=="phragmites",], aes(x=`total weight`, y=`lai photo gap fraction`)) + geom_point(aes(colour=veg_type)) + geom_smooth(method='lm') + theme_minimal() + xlab("Biomass (total weight)") + ylab("Gap franction proxy")

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

p6=ggplot(data =data, aes(x=fhd_bio, y=fhd_pole)) + geom_point(aes(colour=veg_type_2)) + geom_smooth(method='lm') + theme_minimal() + xlab("FHD from biomass") + ylab("FHD from pole measure")

model_fhd <- lm(fhd_bio ~ fhd_pole, data = data)
summary(model_fhd)

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

p7=ggplot(data =data, aes(x=fhd_bio_rao, y=fhd_pole_rao)) + geom_point(aes(colour=veg_type_2)) + geom_smooth(method='lm') + theme_minimal() + xlab("Quadratic FHD from biomass") + ylab("Quadratic FHD from pole measure")

model_fhd_rao <- lm(fhd_bio_rao ~ fhd_pole_rao, data = data)
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

stargazer(model,model_lai,model_lai2,model_lai_h,model_frac,model_frac2,model_fhd,model_fhd_rao, type = "text",out="point_to_quadrat.txt")

# total biomass vs. pole contact height

model_bvsh <- lm(`veg height from pole` ~ `total weight`, data = data[data$veg_type_2=="phragmites",])
summary(model_bvsh)

p8=ggplot(data =data[data$veg_type_2=="phragmites",], aes(x=`veg height from pole`, y=`total weight`)) + geom_point(aes(colour=veg_type_2)) + geom_smooth(method='lm') + theme_minimal() + xlab("Pole height [m]") + ylab("Total biomass")

# total biomass vs. full contact

model_bm <- lm(sum_pole_contacts ~ `total weight`, data = data[data$veg_type_2=="phragmites",])
summary(model_bm)

p9=ggplot(data =data[data$veg_type_2=="phragmites",], aes(x=sum_pole_contacts, y=`total weight`)) + geom_point(aes(colour=veg_type_2)) + geom_smooth(method='lm') + theme_minimal() + xlab("Sum pole contacts") + ylab("Total biomass")

# LAI vs. quadrat height

model_lai_h2 <- lm(`lai photo lai` ~ veg_height_m, data = data[data$veg_type_2=="phragmites",])
summary(model_lai_h2)

p10=ggplot(data =data[data$veg_type_2=="phragmites",], aes(x=`lai photo lai`, y=veg_height_m)) + geom_point(aes(colour=veg_type_2)) + geom_smooth(method='lm') + theme_minimal() + xlab("LAI") + ylab("Vegetation height")

# total dry vs. pole 0-1 m /total
data$sum_dry_weight=rowSums(data[,c(45,49,54,58,60,64,68,72,76,80)])
data$pole01_persum=(sum(data$`5_pole_con`,data$`5_1_pole_c`))/data$sum_pole_contacts

model_dry <- lm(sum_dry_weight ~ pole01_persum, data = data[data$veg_type_2=="phragmites",])
summary(model_dry)

p11=ggplot(data =data[data$veg_type_2=="phragmites",], aes(x=sum_dry_weight, y=pole01_persum)) + geom_point(aes(colour=veg_type_2)) + geom_smooth(method='lm') + theme_minimal() + xlab("Total dry weight") + ylab("Pole contact between 0-1 to total")

# above 1.5 m pole contact per all vs. above 1.5 biomass per all
data$above15_pole_persum=(sum(data$`5_2_pole_c`,data$`5_pole_c_2`,data$`5_3_pole_c`,data$`5_pole_c_3`,data$`5_4_pole_c`,data$`5_pole_c_4`,data$`5_5_pole_c`))/data$sum_pole_contacts
data$above15_bio_persum=(sum(data$`1.5_2_total_weight`,data$`2_2.5_total_weight`,data$`2.5_3_total_weight`,data$`3_3.5_total_weight`,data$`3.5_4_total_weight`,data$`4_4.5_total_weight`,data$`4.5_5_total_weight`))/data$`total weight`

model_corti <- lm(above15_pole_persum ~ above15_bio_persum, data = data[data$veg_type_2=="phragmites",])
summary(model_corti)

p12=ggplot(data =data[data$veg_type_2=="phragmites",], aes(x=above15_pole_persum, y=above15_bio_persum)) + geom_point(aes(colour=veg_type_2)) + geom_smooth(method='lm') + theme_minimal() + xlab("Above 1.5 m pole contact to total") + ylab("Above 1.5 m biomass to total")