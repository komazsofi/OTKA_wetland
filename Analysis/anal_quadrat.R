library(dplyr)
library(stringr)
library(tidyr)

library(FactoMineR)
library(factoextra)

library(ggplot2)
library(gridExtra)
library(GGally)

library(usdm)

workdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/2019Nov/"
setwd(workdir)

lidar_wpointsfield=read.csv("lidar_wquadratfield.csv")
lidar_wpointsfield_filt=lidar_wpointsfield[(lidar_wpointsfield$veg_type_sp == "phragmites" | lidar_wpointsfield$veg_type_sp == "typha" & lidar_wpointsfield$season == "leafoff"), ]

forFHD=subset(lidar_wpointsfield_filt,select=c(19:47,4,14))
forFHD_rao=subset(lidar_wpointsfield_filt,select=c(19:47,12,14))

forheight=subset(lidar_wpointsfield_filt,select=c(19:47,52,14))
forshandiv=subset(lidar_wpointsfield_filt,select=c(19:47,13,14))
forbiomass=subset(lidar_wpointsfield_filt,select=c(19:47,11,14))

forFHD %>%
  gather(-fhd_bio,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_bio,color=lake)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat FHD") +
  theme_bw()

forFHD_rao %>%
  gather(-fhd_bio_rao,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_bio_rao,color=lake)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat FHD Rao") +
  theme_bw()

forheight %>%
  gather(-veg_height_m,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = veg_height_m,color=lake)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat vegetation height") +
  theme_bw()

forshandiv %>%
  gather(-shan_div,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = shan_div,color=lake)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat shannon diversity") +
  theme_bw()

-+
  