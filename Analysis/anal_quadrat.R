library(dplyr)
library(stringr)

library(FactoMineR)
library(factoextra)

library(ggplot2)
library(gridExtra)
library(GGally)

library(usdm)

workdir="D:/Sync/_Amsterdam/11_AndrasProject/Analysis/2019Nov/"
setwd(workdir)

lidar_wpointsfield=read.csv("lidar_wpointsfield.csv")

