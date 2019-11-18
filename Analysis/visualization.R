library(dplyr)
library(stringr)
library(tidyr)

library(FactoMineR)
library(factoextra)

library(ggplot2)
library(gridExtra)
library(GGally)

library(corrplot)
library(usdm)

library(randomForest)
library(caret)

library(stargazer)
library(ggpubr)
library(GGally)

# Import

#workdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/2019Nov/"
workdir="D:/Sync/_Amsterdam/11_AndrasProject/Analysis/2019Nov/"
setwd(workdir)

lidar_wquadratfield=read.csv("lidar_wquadratfield.csv")
lidar_wquadratfield_filt=lidar_wquadratfield[lidar_wquadratfield$season == "leafoff", ]
lidar_wquadratfield_filt=lidar_wquadratfield[lidar_wquadratfield$sum_pole_contacts < 60, ]

lidar_wpointsfield=read.csv("lidar_wpointsfield.csv")
lidar_wpointsfield_filt=lidar_wpointsfield[lidar_wpointsfield$season == "leafoff", ]

# points to field

forFHD_field=subset(lidar_wquadratfield_filt,select=c(7,8,9,10,14,16,4,15))
forFHD_rao_field=subset(lidar_wquadratfield_filt,select=c(7,8,9,10,14,16,4,17))
forbiomass_field=subset(lidar_wquadratfield_filt,select=c(7,8,9,10,14,16,4,11))
forleafweight_field=subset(lidar_wquadratfield_filt,select=c(7,8,9,10,14,16,4,13))
forheight_field=subset(lidar_wquadratfield_filt,select=c(7,8,9,10,14,16,4,12))

forFHD_field %>%
  gather(-fhd_bio,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_bio)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat FHD vs. points measurements") +
  theme_bw()

forFHD_rao_field %>%
  gather(-fhd_bio_rao,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_bio_rao)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat FHD vs. points measurements") +
  theme_bw()

forbiomass_field %>%
  gather(-total.weight,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = total.weight)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat FHD vs. points measurements") +
  theme_bw()

forleafweight_field %>%
  gather(-sum_leaf_weight,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = sum_leaf_weight)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat FHD vs. points measurements") +
  theme_bw()

forheight_field %>%
  gather(-veg_height_m,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = veg_height_m)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat FHD vs. points measurements") +
  theme_bw()

# Quadrat vs. lidar
forFHD=subset(lidar_wquadratfield_filt,select=c(20,21,24,25,37,38,39,40,41,52,28,43,44,4,15))
forFHD_rao=subset(lidar_wquadratfield_filt,select=c(20,21,24,25,37,38,39,40,41,52,28,43,44,4,17))
forheight=subset(lidar_wquadratfield_filt,select=c(20,21,24,25,37,38,39,40,41,52,28,43,44,4,12))
forbiomass=subset(lidar_wquadratfield_filt,select=c(20,21,24,25,37,38,39,40,41,52,28,43,44,4,11))
forLAI=subset(lidar_wquadratfield_filt,select=c(20,21,24,25,37,38,39,40,41,52,28,43,4,44,13))

#Visualize

forFHD %>%
  gather(-fhd_bio,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_bio)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat FHD") +
  theme_bw()

forFHD_rao %>%
  gather(-fhd_bio_rao,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_bio_rao)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat FHD Rao") +
  theme_bw()

forheight %>%
  gather(-veg_height_m,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = veg_height_m)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat vegetation height") +
  theme_bw()

forLAI %>%
  gather(-sum_leaf_weight,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = sum_leaf_weight)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat leaf weight") +
  theme_bw()

forbiomass %>%
  gather(-total.weight,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = total.weight)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat leaf weight") +
  theme_bw()

# Points vs. lidar
forFHD_point=subset(lidar_wpointsfield_filt,select=c(15,16,11,19,43,34,35,28,29,30,31,32,12,55,48))
forFHD_rao_point=subset(lidar_wpointsfield_filt,select=c(15,16,11,19,43,34,35,28,29,30,31,32,12,55,49))
forheight_point=subset(lidar_wpointsfield_filt,select=c(15,16,11,19,43,34,35,28,29,30,31,32,12,55,51))
forbiomass_point=subset(lidar_wpointsfield_filt,select=c(15,16,11,19,43,34,35,28,29,30,31,32,12,55,50))
forLAI_point=subset(lidar_wpointsfield_filt,select=c(15,16,11,19,43,34,35,28,29,30,31,32,12,55,46))

forFHD_point %>%
  gather(-fhd_pole,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_pole)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point FHD") +
  theme_bw()

forFHD_rao_point %>%
  gather(-fhd_pole_rao,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_pole_rao)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat FHD Rao") +
  theme_bw()

forheight_point %>%
  gather(-pole_height,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = pole_height)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat vegetation height") +
  theme_bw()

forLAI_point %>%
  gather(-gct_lai,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = gct_lai)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat leaf weight") +
  theme_bw()

forbiomass_point %>%
  gather(-sum_pole_contact,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = sum_pole_contact)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat leaf weight") +
  theme_bw()


