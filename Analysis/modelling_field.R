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

library(olsrr)

# Import

workdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/2019Nov/"
#workdir="D:/Sync/_Amsterdam/11_AndrasProject/Analysis/2019Nov/"
setwd(workdir)

lidar_wquadratfield=read.csv("lidar_wquadratfield.csv")

# Filter
forFHD_field=subset(lidar_wquadratfield[(lidar_wquadratfield$season == "leafoff" & lidar_wquadratfield$veg_type_sp!="carex" 
                                         & lidar_wquadratfield$fhd_bio!=0), ],select=c(7,8,9,10,14,16,4,15))
# FHD
forFHD_field %>%
  gather(-fhd_bio,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_bio)) +
  geom_point(aes(color=lake)) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat FHD vs. points measurements") +
  theme_bw()

model <- lm(fhd_bio~., data = forFHD_field)
allpossible=ols_step_all_possible(model)
best=ols_step_best_subset(model)

ols_plot_cooksd_bar(model)
ols_test_normality(model)
ols_plot_resid_hist(model)
ols_plot_resid_qq(model)
ols_plot_resid_fit(model)

#ggsave("forFHD_field.png")

forFHD_rao_field %>%
  gather(-fhd_bio_rao,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_bio_rao)) +
  geom_point(aes(color=lake)) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat FHD rao vs. points measurements") +
  theme_bw()

#ggsave("forFHD_rao_field.png")

forbiomass_field %>%
  gather(-total.weight,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = total.weight)) +
  geom_point(aes(color=lake)) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat biomass vs. points measurements") +
  theme_bw()

#ggsave("forbiomass_field.png")

forleafweight_field %>%
  gather(-sum_leaf_weight,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = sum_leaf_weight)) +
  geom_point(aes(color=lake)) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat leaf weight vs. points measurements") +
  theme_bw()

#ggsave("forleafweight_field.png")

forheight_field %>%
  gather(-veg_height_m,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = veg_height_m)) +
  geom_point(aes(color=lake)) +
  geom_smooth(method="lm",colour="black",se=FALSE) +
  stat_cor() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat height vs. points measurements") +
  theme_bw()

#ggsave("forheight_field.png")