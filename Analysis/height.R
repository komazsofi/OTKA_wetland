library(ggplot2)
library(usdm)

library(ggplot2)
library(gridExtra)
library(ggrepel)
library(grid)

workdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/field_measured_data/"
setwd(workdir)

data=read.csv("lidar_and_field_merged.csv")
data$veg_height_m=data$veg_height/100

p1=ggplot(data =data, aes(x=veg_height_m, y=nDSM)) + geom_point(aes(colour=factor(lake))) + geom_smooth(method='lm') + theme_minimal() + xlab("Vegetation height [m]") + ylab("nDSM [m]") +
  ggtitle("All data (R2=0.42)")

data_sub=subset(data,select=c("class","veg_height_m","nDSM","Amplitude_mean","NormalizedZ_P10","NrOfEchos_max","shannonEntropy","Z_variance","lake"))
data_sub$residual=data_sub$veg_height_m-data_sub$nDSM
data_sub=data_sub[data_sub$residual<2,]

p2=ggplot(data =data_sub, aes(x=veg_height_m, y=nDSM)) + geom_point(aes(colour=factor(lake))) + geom_smooth(method='lm') + theme_minimal() + xlab("Vegetation height [m]") + ylab("nDSM [m]") +
  ggtitle("If the residual is < 2m (R2=0.8)")

model <- lm(veg_height_m ~ nDSM, data = data_sub)
summary(model)

model2 <- lm(veg_height_m ~ nDSM, data = data)
summary(model2)

model <- lm(veg_height_m ~ Amplitude_mean + NormalizedZ_P10 + NrOfEchos_max + shannonEntropy + Z_variance, data = data_sub)
summary(model)

model <- glm(veg_height_m ~ Amplitude_mean + NormalizedZ_P10 + NrOfEchos_max + shannonEntropy + Z_variance, data = data_sub)
summary(model)

fig=grid.arrange(
  p1,
  p2,
  ncol=2,
  nrow=1
)
