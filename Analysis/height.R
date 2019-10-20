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

data_bal=data[data$lake=="balaton",]
data_tisza=data[data$lake=="tisza",]
data_ferto=data[data$lake=="ferto-hun",]


p1=ggplot(data =data, aes(x=veg_height_m, y=nDSM)) + geom_point(aes(colour=factor(lake))) + geom_smooth(method='lm') + theme_minimal() + xlab("Vegetation height [m]") + ylab("nDSM [m]") +
  ggtitle("All data (R2=0.42)")

data_sub=subset(data,select=c("class","point_name","veg_height_m","nDSM","Amplitude_mean","NormalizedZ_P10","NrOfEchos_max","shannonEntropy","Z_variance","lake","coords.x1","coords.x2"))
data_sub$residual=data_sub$veg_height_m-data_sub$nDSM

data_sub2=data_sub[data_sub$residual>2,]

data_sub2_bal=data_sub2[data_sub2$lake=="balaton",]
write.csv(data_sub2_bal,"balaton_wrong_points.csv")

data_sub2_tisza=data_sub2[data_sub2$lake=="tisza",]
write.csv(data_sub2_tisza,"tisza_wrong_points.csv")

data_sub2_ferto=data_sub2[data_sub2$lake=="ferto-hun",]
write.csv(data_sub2_ferto,"ferto_wrong_points.csv")

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

# Corr
gct_lai=data.frame(cor(data[83:106], data$veg_height_m,method = "spearman"))
gct_lai1=data.frame(cor(data_ferto[83:106], data_ferto$veg_height_m,method = "spearman"))
gct_lai2=data.frame(cor(data_bal[83:106], data_bal$veg_height_m,method = "spearman"))
gct_lai3=data.frame(cor(data_tisza[83:106], data_tisza$veg_height_m,method = "spearman"))

gct_lai_all=cbind(gct_lai,gct_lai1,gct_lai2,gct_lai3)
colnames(gct_lai_all)[1] <- "height_all"
colnames(gct_lai_all)[2] <- "height_ferto"
colnames(gct_lai_all)[3] <- "height_tisza"
colnames(gct_lai_all)[4] <- "height_balaton"

write.csv(gct_lai_all,"height_all_corr.csv")
