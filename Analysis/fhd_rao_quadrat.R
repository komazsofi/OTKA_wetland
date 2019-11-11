library(ggplot2)
library(usdm)

library(ggplot2)
library(gridExtra)
library(ggrepel)
library(grid)

library(caret)
library(leaps)
library(MASS)

library(glmnet)
library(randomForest)
library(e1071) 

library(stargazer)

workdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/2019Nov/main/"
setwd(workdir)

lidar_wpointsfield=read.csv("lidar_wquadratfield.csv")

# after multicollinearity check c(28+2,35+2,29+2,25+2,26+2,17+2,16+2,20+2,30+2,12+2)

# LM based on ecological expectation
lm.fit_leafoff1=lm(quadraticEntropy.grd~fhd_bio_rao,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",])
lm.fit_leafoff2=lm(shannonEntropy.grd~fhd_bio_rao,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",])
lm.fit_leafoff3=lm(pc_rel3.grd~fhd_bio_rao,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",])

stargazer(lm.fit_leafoff1,lm.fit_leafoff2,lm.fit_leafoff3, type = "text")

ggplot(data =lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",], aes(x=fhd_bio_rao, y=quadraticEntropy.grd)) + geom_point(aes(colour=factor(lake))) + geom_smooth(method='lm') + theme_minimal() + xlab("Vegetation height [m]") + ylab("ZRange [m]")

# MLM with feature selection
mlm.fit_leafoff=lm(fhd_bio_rao~.,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",c(28+2,35+2,29+2,25+2,26+2,17+2,16+2,20+2,30+2,12+2,11)])
summary(mlm.fit_leafoff)
stargazer(mlm.fit_leafoff,type="text")

# GLM - Gaussian == linear regression
glm.fit_leafoff <- glm(fhd_bio_rao ~.,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",c(28+2,35+2,29+2,25+2,26+2,17+2,16+2,20+2,30+2,12+2,11)], family=gaussian())
summary(glm.fit_leafoff)

# RF
seed(100)
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

rf.fit_leafoff <-  train(fhd_bio_rao ~.,
                         data = lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",c(28+2,35+2,29+2,25+2,26+2,17+2,16+2,20+2,30+2,12+2,11)],
                         method = "rf",
                         metric = "RMSE",
                         importance=TRUE,
                         ntree=100,
                         trControl = trControl)
rf.fit_leafoff
varImpPlot(rf.fit_leafoff[["finalModel"]])

#MLM selection best
mlm.fit_best=lm(veg_height_m ~ Zrange.grd+quadraticEntropy.grd,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",])
summary(mlm.fit_best)
stargazer(mlm.fit_best,type="text")