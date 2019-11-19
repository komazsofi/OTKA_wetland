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
lidar_wquadratfield_filt=lidar_wquadratfield[lidar_wquadratfield$season == "leafoff", ]
lidar_wquadratfield_filt=lidar_wquadratfield[lidar_wquadratfield$sum_pole_contacts < 60, ]

lidar_wpointsfield=read.csv("lidar_wpointsfield.csv")
lidar_wpointsfield_filt=lidar_wpointsfield[lidar_wpointsfield$season == "leafoff", ]
lidar_wpointsfield_leafon=lidar_wpointsfield[lidar_wpointsfield$season == "leafon", ]
lidar_wpointsfield_bal=lidar_wpointsfield[lidar_wpointsfield$lake == "balaton", ]
lidar_wpointsfield_fer=lidar_wpointsfield[lidar_wpointsfield$lake == "ferto", ]
lidar_wpointsfield_tisza=lidar_wpointsfield[(lidar_wpointsfield$season == "leafoff" & lidar_wpointsfield$lake == "tisza"), ]

# all possible regression
model <- lm(total.weight ~ Amplitudemean.grd.2 + Zstdv.grd + pc_rel2.grd + zsigmaZ.grd, data = lidar_wquadratfield_filt)
ols_step_all_possible(model)
ols_step_best_subset(model)

ols_plot_cooksd_bar(model)
ols_test_normality(model)
ols_plot_resid_hist(model)
ols_plot_resid_qq(model)
ols_plot_resid_fit(model)
