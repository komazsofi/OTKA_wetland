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
rf=randomForest(x=lidar_wpointsfield[(lidar_wpointsfield$season=="leafoff"),10:44],y=lidar_wpointsfield[(lidar_wpointsfield$season=="leafoff"),48])
varImpPlot(rf)

#select first 10 most important c(26,17,16,28,11,12,33,30,25,29)
v=vifcor(lidar_wpointsfield[,c(26,17,16,28,11,12,33,30,25,29)],th=0.6)
v

# after multicollinearity check c(16,11,12,33,25)

# LM based on ecological expectation
ggplot(data =lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",], aes(x=fhd_pole, y=shannonEntropy.grd)) + geom_point(aes(colour=factor(lake))) + geom_smooth(method='lm',aes(group=lake,colour=factor(lake))) + theme_minimal() + xlab("FHD_rao_pole") + ylab("FhD_rao_lidar") 

lm.fit_leafoff1=lm(quadraticEntropy.grd~fhd_pole,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",])
lm.fit_leafoff2=lm(shannonEntropy.grd~fhd_pole,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",])

stargazer(lm.fit_leafoff1,lm.fit_leafoff2, type = "text")

lm.fit_leafon1=lm(quadraticEntropy.grd~fhd_pole,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafon",])
lm.fit_leafon2=lm(shannonEntropy.grd~fhd_pole,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafon",])

stargazer(lm.fit_leafon1,lm.fit_leafon2, type = "text")

lm.fit_balaton1=lm(quadraticEntropy.grd~fhd_pole,data=lidar_wpointsfield[lidar_wpointsfield$lake=="balaton",])
lm.fit_balaton2=lm(shannonEntropy.grd~fhd_pole,data=lidar_wpointsfield[lidar_wpointsfield$lake=="balaton",])

stargazer(lm.fit_balaton1,lm.fit_balaton2,type = "text")

lm.fit_ferto1=lm(quadraticEntropy.grd~fhd_pole,data=lidar_wpointsfield[lidar_wpointsfield$lake=="ferto",])
lm.fit_ferto2=lm(shannonEntropy.grd~fhd_pole,data=lidar_wpointsfield[lidar_wpointsfield$lake=="ferto",])

stargazer(lm.fit_ferto1,lm.fit_ferto2, type = "text")

lm.fit_tisza1=lm(quadraticEntropy.grd~fhd_pole,data=lidar_wpointsfield[(lidar_wpointsfield$lake=="tisza"&lidar_wpointsfield$season=="leafoff"),])
lm.fit_tisza2=lm(shannonEntropy.grd~fhd_pole,data=lidar_wpointsfield[(lidar_wpointsfield$lake=="tisza"&lidar_wpointsfield$season=="leafoff"),])

stargazer(lm.fit_tisza1,lm.fit_tisza2, type = "text")

# MLM with feature selection
mlm.fit_leafoff=lm(fhd_pole ~.,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",c(16,11,12,33,25,48)])
summary(mlm.fit_leafoff)
stargazer(mlm.fit_leafoff,type="text")

mlm.fit_leafon=lm(fhd_pole ~.,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafon",c(16,11,12,33,25,48)])
summary(mlm.fit_leafon)
stargazer(mlm.fit_leafon,type="text")

mlm.fit_balaton=lm(fhd_pole ~.,data=lidar_wpointsfield[lidar_wpointsfield$lake=="balaton",c(16,11,12,33,25,48)])
summary(mlm.fit_balaton)
stargazer(mlm.fit_balaton,type="text")

mlm.fit_ferto=lm(fhd_pole ~.,data=lidar_wpointsfield[lidar_wpointsfield$lake=="ferto",c(16,11,12,33,25,48)])
summary(mlm.fit_ferto)
stargazer(mlm.fit_ferto,type="text")

mlm.fit_tisza=lm(fhd_pole ~.,data=lidar_wpointsfield[(lidar_wpointsfield$lake=="tisza"&lidar_wpointsfield$season=="leafoff"),c(16,11,12,33,25,48)])
summary(mlm.fit_tisza)
stargazer(mlm.fit_tisza,type="text")

# GLM - Gaussian == linear regression
glm.fit_leafoff <- glm(fhd_pole ~.,data=lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",c(16,11,12,33,25,48)], family=poisson())
summary(glm.fit_leafoff)

# RF
seed(100)
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

rf.fit_leafoff <-  train(fhd_pole~.,
                         data = lidar_wpointsfield[lidar_wpointsfield$season=="leafoff",c(16,11,12,33,25,48)],
                         method = "rf",
                         metric = "RMSE",
                         importance=TRUE,
                         ntree=100,
                         trControl = trControl)
rf.fit_leafoff
varImpPlot(rf.fit_leafoff[["finalModel"]])


rf.fit_leafon <-  train(fhd_pole ~.,
                        data = lidar_wpointsfield[lidar_wpointsfield$season=="leafon",c(16,11,12,33,25,48)],
                        method = "rf",
                        metric = "RMSE",
                        importance=TRUE,
                        ntree=100,
                        trControl = trControl)
rf.fit_leafon
varImpPlot(rf.fit_leafon[["finalModel"]])

rf.fit_balaton <-  train(fhd_pole ~.,
                         data = lidar_wpointsfield[lidar_wpointsfield$lake=="balaton",c(16,11,12,33,25,48)],
                         method = "rf",
                         metric = "RMSE",
                         importance=TRUE,
                         ntree=100,
                         trControl = trControl)
rf.fit_balaton
varImpPlot(rf.fit_balaton[["finalModel"]])

rf.fit_ferto <-  train(fhd_pole ~.,
                       data = lidar_wpointsfield[lidar_wpointsfield$lake=="ferto",c(16,11,12,33,25,48)],
                       method = "rf",
                       metric = "RMSE",
                       importance=TRUE,
                       ntree=100,
                       trControl = trControl)
rf.fit_ferto
varImpPlot(rf.fit_ferto[["finalModel"]])

rf.fit_tisza <-  train(fhd_pole ~.,
                       data = lidar_wpointsfield[(lidar_wpointsfield$lake=="tisza"&lidar_wpointsfield$season=="leafoff"),c(16,11,12,33,25,48)],
                       method = "rf",
                       metric = "RMSE",
                       importance=TRUE,
                       ntree=100,
                       trControl = trControl)
rf.fit_tisza
varImpPlot(rf.fit_tisza[["finalModel"]])
