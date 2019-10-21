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

workingdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/field_measured_data/"
setwd(workingdir)

## Import
balaton <- read_excel("quadrat_forR.xlsx",sheet = 2)
balaton_lidar=read.csv("C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/lidar_anal/balaton_lidar.csv")

tisza <- read_excel("quadrat_forR.xlsx",sheet = 3)
tisza_lidar=read.csv("C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/lidar_anal/tisza_leafoff_lidar.csv")

ferto_1 <- read_excel("quadrat_forR.xlsx",sheet = 4)
ferto_lidar=read.csv("C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/lidar_anal/ferto_lidar.csv")
ferto_2 <- read_excel("quadrat_forR.xlsx",sheet = 5)

## Merging
lidar_balaton_merged=merge(balaton,balaton_lidar, by.x=c('point_ID','point_name'), by.y=c('point_ID','point_name'))
lidar_balaton_merged_cleaned=subset(lidar_balaton_merged,select=c(1,2,7:25,28:87,93:116,133:138))

lidar_ferto1_merged=merge(ferto_1,ferto_lidar, by.x=c('point_ID','point_name'), by.y=c('point_ID','point_name'))
lidar_ferto1_merged_cleaned=subset(lidar_ferto1_merged,select=c(1,2,7:25,28:87,93:116,133:138))

lidar_ferto2_merged=merge(ferto_2,ferto_lidar, by.x=c('point_ID','point_name'), by.y=c('point_ID','point_name'))
lidar_ferto2_merged_cleaned=subset(lidar_ferto2_merged,select=c(1,2,7:25,28:87,93:116,133:138))

lidar_tisza_merged=merge(tisza,tisza_lidar, by.x=c('point_name'), by.y=c('pont_nm'))
lidar_tisza_merged_cleaned=subset(lidar_tisza_merged,select=c(6,1,7:25,28:87,93:116,131:133,135,136,134))

names(lidar_tisza_merged_cleaned) <- names(lidar_balaton_merged_cleaned)

lidar_balaton_merged_cleaned$lake<- "balaton"
lidar_ferto1_merged_cleaned$lake<- "ferto-hun"
lidar_tisza_merged_cleaned$lake<- "tisza"

all_merged=rbind(lidar_balaton_merged_cleaned,lidar_ferto1_merged_cleaned,lidar_tisza_merged_cleaned)

write.csv(all_merged,"lidar_and_field_merged.csv")
