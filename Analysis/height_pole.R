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

lidar_wpointsfield=read.csv("lidar_wpointsfield.csv")

#RF for recognise most important variables
rf=randomForest(x=lidar_wpointsfield[(lidar_wpointsfield$season=="leafoff"),10:44],y=lidar_wpointsfield[(lidar_wpointsfield$season=="leafoff"),51])
varImpPlot(rf)

#select first 10 most important c(13,28,14,41,12,17,34,18,30,35)
v=vifcor(lidar_wpointsfield[,c(13,28,14,41,12,17,34,18,30,35)],th=0.6)
v

# after multicollinearity check c(13,28,14,41,12,34,18,35)

# LM based on ecological expectation
ggplot(data =lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",], aes(x=pole_height, y=Zrange.grd)) + geom_point(aes(colour=factor(lake))) + geom_smooth(method='lm',aes(group=lake,colour=factor(lake))) + theme_minimal() + xlab("Vegetation height [m]") + ylab("ZRange [m]") + xlim(0,4) + ylim(0,4)

lm.fit_leafoff1=lm(NormalizedZP10.grd~pole_height,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",])
lm.fit_leafoff2=lm(nDSM.grd~pole_height,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",])
lm.fit_leafoff3=lm(Zrange.grd~pole_height,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",])

stargazer(lm.fit_leafoff1,lm.fit_leafoff2,lm.fit_leafoff3, type = "text")

lm.fit_leafon1=lm(NormalizedZP10.grd~pole_height,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafon",])
lm.fit_leafon2=lm(nDSM.grd~pole_height,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafon",])
lm.fit_leafon3=lm(Zrange.grd~pole_height,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafon",])

stargazer(lm.fit_leafon1,lm.fit_leafon2,lm.fit_leafon3, type = "text")

lm.fit_balaton1=lm(NormalizedZP10.grd~pole_height,data=lidar_wpointsfield[lidar_wpointsfield$lake=="balaton",])
lm.fit_balaton2=lm(nDSM.grd~pole_height,data=lidar_wpointsfield[lidar_wpointsfield$lake=="balaton",])
lm.fit_balaton3=lm(Zrange.grd~pole_height,data=lidar_wpointsfield[lidar_wpointsfield$lake=="balaton",])

stargazer(lm.fit_balaton1,lm.fit_balaton2,lm.fit_balaton3, type = "text")

lm.fit_ferto1=lm(NormalizedZP10.grd~pole_height,data=lidar_wpointsfield[lidar_wpointsfield$lake=="ferto",])
lm.fit_ferto2=lm(nDSM.grd~pole_height,data=lidar_wpointsfield[lidar_wpointsfield$lake=="ferto",])
lm.fit_ferto3=lm(Zrange.grd~pole_height,data=lidar_wpointsfield[lidar_wpointsfield$lake=="ferto",])

stargazer(lm.fit_ferto1,lm.fit_ferto2,lm.fit_ferto3, type = "text")

lm.fit_tisza1=lm(NormalizedZP10.grd~pole_height,data=lidar_wpointsfield[(lidar_wpointsfield$lake=="tisza"&lidar_wpointsfield$season=="leafoff"),])
lm.fit_tisza2=lm(nDSM.grd~pole_height,data=lidar_wpointsfield[(lidar_wpointsfield$lake=="tisza"&lidar_wpointsfield$season=="leafoff"),])
lm.fit_tisza3=lm(Zrange.grd~pole_height,data=lidar_wpointsfield[(lidar_wpointsfield$lake=="tisza"&lidar_wpointsfield$season=="leafoff"),])

stargazer(lm.fit_tisza1,lm.fit_tisza2,lm.fit_tisza3, type = "text")

# MLM with feature selection
mlm.fit_leafoff=lm(pole_height ~.,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",c(13,28,14,41,12,34,18,35,51)])
summary(mlm.fit_leafoff)
stargazer(mlm.fit_leafoff,type="text")

mlm.fit_leafon=lm(pole_height ~.,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafon",c(13,28,14,41,12,34,18,35,51)])
summary(mlm.fit_leafon)
stargazer(mlm.fit_leafon,type="text")

mlm.fit_balaton=lm(pole_height ~.,data=lidar_wpointsfield[lidar_wpointsfield$lake=="balaton",c(13,28,14,41,12,34,18,35,51)])
summary(mlm.fit_balaton)
stargazer(mlm.fit_balaton,type="text")

mlm.fit_ferto=lm(pole_height ~.,data=lidar_wpointsfield[lidar_wpointsfield$lake=="ferto",c(13,28,14,41,12,34,18,35,51)])
summary(mlm.fit_ferto)
stargazer(mlm.fit_ferto,type="text")

mlm.fit_tisza=lm(pole_height ~.,data=lidar_wpointsfield[(lidar_wpointsfield$lake=="tisza"&lidar_wpointsfield$season=="leafoff"),c(13,28,14,41,12,34,18,35,51)])
summary(mlm.fit_tisza)
stargazer(mlm.fit_tisza,type="text")

# GLM - Gaussian == linear regression
glm.fit_leafoff <- glm(pole_height ~.,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",c(13,28,14,41,12,34,18,35,51)], family=gaussian())
summary(glm.fit_leafoff)

# RF
seed(100)
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

rf.fit_leafoff <-  train(pole_height ~.,
                         data = lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",c(13,28,14,41,12,34,18,35,51)],
                         method = "rf",
                         metric = "RMSE",
                         importance=TRUE,
                         ntree=100,
                         trControl = trControl)
rf.fit_leafoff
varImpPlot(rf.fit_leafoff[["finalModel"]])


rf.fit_leafon <-  train(pole_height ~.,
                        data = lidar_wpointsfield[lidar_wpointsfield$season=="leafon",c(13,28,14,41,12,34,18,35,51)],
                        method = "rf",
                        metric = "RMSE",
                        importance=TRUE,
                        ntree=100,
                        trControl = trControl)
rf.fit_leafon
varImpPlot(rf.fit_leafon[["finalModel"]])

rf.fit_balaton <-  train(pole_height ~.,
                         data = lidar_wpointsfield[lidar_wpointsfield$lake=="balaton",c(13,28,14,41,12,34,18,35,51)],
                         method = "rf",
                         metric = "RMSE",
                         importance=TRUE,
                         ntree=100,
                         trControl = trControl)
rf.fit_balaton
varImpPlot(rf.fit_balaton[["finalModel"]])

rf.fit_ferto <-  train(pole_height ~.,
                       data = lidar_wpointsfield[lidar_wpointsfield$lake=="ferto",c(13,28,14,41,12,34,18,35,51)],
                       method = "rf",
                       metric = "RMSE",
                       importance=TRUE,
                       ntree=100,
                       trControl = trControl)
rf.fit_ferto
varImpPlot(rf.fit_ferto[["finalModel"]])

rf.fit_tisza <-  train(pole_height ~.,
                       data = lidar_wpointsfield[(lidar_wpointsfield$lake=="tisza"&lidar_wpointsfield$season=="leafoff"),c(13,28,14,41,12,34,18,35,51)],
                       method = "rf",
                       metric = "RMSE",
                       importance=TRUE,
                       ntree=100,
                       trControl = trControl)
rf.fit_tisza
varImpPlot(rf.fit_tisza[["finalModel"]])
