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

library(stargazer)

workingdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/field_measured_data/"
setwd(workingdir)

data <- read_excel("quadrat_forR.xlsx",sheet = 1)
data$veg_height_m=data$veg_height/100

model <- lm(veg_height_m ~ `veg height from pole`, data = data)
summary(model)

ggplot(data =data, aes(x=veg_height_m, y=`veg height from pole`)) + geom_point() + geom_smooth(method='lm') + theme_minimal() + xlab("Vegetation height [m]") + ylab("Pole height [m]")

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
