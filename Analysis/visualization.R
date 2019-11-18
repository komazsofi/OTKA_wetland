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

workdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/2019Nov/"
#workdir="D:/Sync/_Amsterdam/11_AndrasProject/Analysis/2019Nov/"
setwd(workdir)

lidar_wquadratfield=read.csv("lidar_wquadratfield.csv")
lidar_wquadratfield_filt=lidar_wquadratfield[lidar_wquadratfield$season == "leafoff", ]
lidar_wquadratfield_filt=lidar_wquadratfield[lidar_wquadratfield$sum_pole_contacts < 60, ]

lidar_wpointsfield=read.csv("lidar_wpointsfield.csv")
lidar_wpointsfield_filt=lidar_wpointsfield[lidar_wpointsfield$season == "leafoff", ]
lidar_wpointsfield_leafon=lidar_wpointsfield[lidar_wpointsfield$season == "leafon", ]
lidar_wpointsfield_bal=lidar_wpointsfield[lidar_wpointsfield$lake == "balaton", ]
lidar_wpointsfield_fer=lidar_wpointsfield[lidar_wpointsfield$lake == "ferto", ]
lidar_wpointsfield_tisza=lidar_wpointsfield[(lidar_wpointsfield$season == "leafoff" & lidar_wpointsfield$lake == "tisza"), ]

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

ggsave("forFHD_field.png")

forFHD_rao_field %>%
  gather(-fhd_bio_rao,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_bio_rao)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat FHD rao vs. points measurements") +
  theme_bw()

ggsave("forFHD_rao_field.png")

forbiomass_field %>%
  gather(-total.weight,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = total.weight)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat biomass vs. points measurements") +
  theme_bw()

ggsave("forbiomass_field.png")

forleafweight_field %>%
  gather(-sum_leaf_weight,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = sum_leaf_weight)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat leaf weight vs. points measurements") +
  theme_bw()

ggsave("forleafweight_field.png")

forheight_field %>%
  gather(-veg_height_m,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = veg_height_m)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat height vs. points measurements") +
  theme_bw()

ggsave("forheight_field.png")

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

ggsave("forFHD.png")

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

ggsave("forFHD_rao.png")

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

ggsave("forheight.png")

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

ggsave("forLAI.png")

forbiomass %>%
  gather(-total.weight,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = total.weight)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat biomass") +
  theme_bw()

ggsave("forbiomass.png")

# Points vs. lidar leaf off all
forFHD_point=subset(lidar_wpointsfield_filt,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,54))
forFHD_rao_point=subset(lidar_wpointsfield_filt,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,55))
forheight_point=subset(lidar_wpointsfield_filt,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,57))
forbiomass_point=subset(lidar_wpointsfield_filt,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,56))
forLAI_point=subset(lidar_wpointsfield_filt,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,46))

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

ggsave("forFHD_point.png")

forFHD_rao_point %>%
  gather(-fhd_pole_rao,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_pole_rao)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point FHD Rao") +
  theme_bw()

ggsave("forFHD_rao_point.png")

forheight_point %>%
  gather(-pole_height,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = pole_height)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point vegetation height") +
  theme_bw()

ggsave("forheight_point.png")

forLAI_point %>%
  gather(-gct_lai,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = gct_lai)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point LAI") +
  theme_bw()

ggsave("forLAI_point.png")

forbiomass_point %>%
  gather(-sum_pole_contact,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = sum_pole_contact)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point biomass") +
  theme_bw()

ggsave("forbiomass_point.png")

# Points vs. lidar leaf on
forFHD_point_leafon=subset(lidar_wpointsfield_leafon,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,54))
forFHD_rao_point_leafon=subset(lidar_wpointsfield_leafon,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,55))
forheight_point_leafon=subset(lidar_wpointsfield_leafon,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,57))
forbiomass_point_leafon=subset(lidar_wpointsfield_leafon,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,56))
forLAI_point_leafon=subset(lidar_wpointsfield_leafon,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,46))

forFHD_point_leafon %>%
  gather(-fhd_pole,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_pole)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point FHD") +
  theme_bw()

ggsave("forFHD_point_leafon.png")

forFHD_rao_point_leafon %>%
  gather(-fhd_pole_rao,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_pole_rao)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point FHD Rao") +
  theme_bw()

ggsave("forFHD_rao_point_leafon.png")

forheight_point_leafon %>%
  gather(-pole_height,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = pole_height)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point vegetation height") +
  theme_bw()

ggsave("forheight_point_leafon.png")

forLAI_point_leafon %>%
  gather(-gct_lai,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = gct_lai)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point LAI") +
  theme_bw()

ggsave("forLAI_point_leafon.png")

forbiomass_point_leafon %>%
  gather(-sum_pole_contact,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = sum_pole_contact)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point biomass") +
  theme_bw()

ggsave("forbiomass_point_leafon.png")

# Points vs. lidar balaton
forFHD_point_bal=subset(lidar_wpointsfield_bal,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,54))
forFHD_rao_point_bal=subset(lidar_wpointsfield_bal,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,55))
forheight_point_bal=subset(lidar_wpointsfield_bal,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,57))
forbiomass_point_bal=subset(lidar_wpointsfield_bal,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,56))
forLAI_point_bal=subset(lidar_wpointsfield_bal,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,46))

forFHD_point_bal %>%
  gather(-fhd_pole,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_pole)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point FHD") +
  theme_bw()

ggsave("forFHD_point_bal.png")

forFHD_rao_point_bal %>%
  gather(-fhd_pole_rao,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_pole_rao)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point FHD Rao") +
  theme_bw()

ggsave("forFHD_rao_point_bal.png")

forheight_point_bal %>%
  gather(-pole_height,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = pole_height)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point vegetation height") +
  theme_bw()

ggsave("forheight_point_bal.png")

forLAI_point_bal %>%
  gather(-gct_lai,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = gct_lai)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point LAI") +
  theme_bw()

ggsave("forLAI_point_bal.png")

forbiomass_point_bal %>%
  gather(-sum_pole_contact,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = sum_pole_contact)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point biomass") +
  theme_bw()

ggsave("forbiomass_point_bal.png")

# Points vs. lidar ferto
forFHD_point_fer=subset(lidar_wpointsfield_fer,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,54))
forFHD_rao_point_fer=subset(lidar_wpointsfield_fer,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,55))
forheight_point_fer=subset(lidar_wpointsfield_fer,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,57))
forbiomass_point_fer=subset(lidar_wpointsfield_fer,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,56))
forLAI_point_fer=subset(lidar_wpointsfield_fer,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,46))

forFHD_point_fer %>%
  gather(-fhd_pole,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_pole)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point FHD") +
  theme_bw()

ggsave("forFHD_point_fer.png")

forFHD_rao_point_fer %>%
  gather(-fhd_pole_rao,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_pole_rao)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point FHD Rao") +
  theme_bw()

ggsave("forFHD_rao_point_fer.png")

forheight_point_fer %>%
  gather(-pole_height,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = pole_height)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point vegetation height") +
  theme_bw()

ggsave("forheight_point_fer.png")

forLAI_point_fer %>%
  gather(-gct_lai,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = gct_lai)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point LAI") +
  theme_bw()

ggsave("forLAI_point_fer.png")

forbiomass_point_fer %>%
  gather(-sum_pole_contact,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = sum_pole_contact)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point biomass") +
  theme_bw()

ggsave("forbiomass_point_fer.png")

# Points vs. lidar tisza leafoff
forFHD_point_ti=subset(lidar_wpointsfield_tisza,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,54))
forFHD_rao_point_ti=subset(lidar_wpointsfield_tisza,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,55))
forheight_point_ti=subset(lidar_wpointsfield_tisza,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,57))
forbiomass_point_ti=subset(lidar_wpointsfield_tisza,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,56))
forLAI_point_ti=subset(lidar_wpointsfield_tisza,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,46))

forFHD_point_ti %>%
  gather(-fhd_pole,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_pole)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point FHD") +
  theme_bw()

ggsave("forFHD_point_tisza.png")

forFHD_rao_point_ti %>%
  gather(-fhd_pole_rao,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_pole_rao)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point FHD Rao") +
  theme_bw()

ggsave("forFHD_rao_point_tisza.png")

forheight_point_ti %>%
  gather(-pole_height,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = pole_height)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point vegetation height") +
  theme_bw()

ggsave("forheight_point_tisza.png")

forLAI_point_ti %>%
  gather(-gct_lai,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = gct_lai)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point LAI") +
  theme_bw()

ggsave("forLAI_point_tisza.png")

forbiomass_point_ti %>%
  gather(-sum_pole_contact,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = sum_pole_contact)) +
  geom_point(aes(color=lake)) +
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point biomass") +
  theme_bw()

ggsave("forbiomass_point_tisza.png")

# dependencies of the lidar metrics (lakes, season, pdens)

ggpairs(lidar_wpointsfield_filt[,c(11,12,7,15,39,30,31,24,25,26,8,27,28,52)], aes(colour =lake, alpha = 0.4))
ggsave("lidar_perlakes.png")

ggpairs(lidar_wpointsfield[(lidar_wpointsfield$lake== "tisza"),c(11,12,7,15,39,30,31,24,25,26,8,27,28,53)], aes(colour =season, alpha = 0.4))
ggsave("lidar_perseasons.png")

pdens_point=subset(lidar_wpointsfield_filt,select=c(11,12,7,15,39,30,31,24,25,26,8,27,28,52,41))

pdens_point %>%
  gather(-pdens.grd,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = pdens.grd,color=lake)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point LAI") +
  theme_bw()

ggsave("pdens_perlake.png")

pdens_point %>%
  gather(-pdens.grd,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = pdens.grd)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Point LAI") +
  theme_bw()

ggsave("pdens.png")
