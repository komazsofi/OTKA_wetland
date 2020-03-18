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

#workdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/2019Nov/"
workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis1/"
setwd(workdir)

lidar_wpointsfield=read.csv("lidar_wquadratfield.csv")
lidar_wpointsfield_filt=lidar_wpointsfield[lidar_wpointsfield$season == "leafoff", ]

forFHD=subset(lidar_wpointsfield_filt,select=c(19:38,4,15))
forFHD_rao=subset(lidar_wpointsfield_filt,select=c(19:38,4,17))
forheight=subset(lidar_wpointsfield_filt,select=c(19:38,4,12))
forbiomass=subset(lidar_wpointsfield_filt,select=c(19:38,4,11))
forLAI=subset(lidar_wpointsfield_filt,select=c(19:38,4,13))

#Visualize

forFHD %>%
  gather(-fhd_bio,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_bio,color=lake)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat FHD") +
  theme_bw()

forFHD_rao %>%
  gather(-fhd_bio_rao,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_bio_rao,color=lake)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat FHD Rao") +
  theme_bw()

forheight %>%
  gather(-veg_height_m,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = veg_height_m,color=lake)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat vegetation height") +
  theme_bw()

forLAI %>%
  gather(-sum_leaf_weight,-lake,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = sum_leaf_weight,color=lake)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat leaf weight") +
  theme_bw()

## Filter 
data_sel=subset(lidar_wpointsfield_filt,select=c(19:38,4,15,17,12,11,13))

## VIF
vif_1=vifstep(data_sel[1:10], th=2.5)
vif_2=vifcor(data_sel[1:10], th=0.6, method='spearman')

## Corr
cor=cor(data_sel[1:10],method = "spearman")

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(forFHD_filt[1:10])

corrplot(cor, method = "color",type="upper",addCoef.col = "black", 
         p.mat = p.mat, sig.level = 0.05)

## PCA
data.pca <- PCA(data_sel[1:10], graph = FALSE)

fviz_pca_var(data.pca, label = "var",gradient.cols = c("white", "blue", "red"),
                col.var = "contrib", repel = TRUE) 

pca <- prcomp(data_sel[1:10], retx=TRUE, center=TRUE, scale=TRUE)
expl.var <- round(pca$sdev^2/sum(pca$sdev^2)*100) 

data_pca <- data.frame(predict(data.pca, newdata=data_sel[1:10]))
data_pca=cbind(data_pca[,c(1,2)],data_sel[,c(12)])
colnames(data_pca)[3] <- "fhd_bio"

## Modelling

mlm.fit_fhd_vif1=lm(fhd_bio~.,data=data_sel[,c(2,22)])
summary(mlm.fit_fhd_vif1)
stargazer(mlm.fit_fhd_vif1,type="text")

mlm.fit_fhd_vif2=lm(fhd_bio~.,data=data_sel[,c(3,4,1,10,12)])
summary(mlm.fit_fhd_vif2)
stargazer(mlm.fit_fhd_vif2,type="text")

mlm.fit_fhd_man=lm(fhd_bio~.,data=data_sel[,c(4,8,10,12)])
summary(mlm.fit_fhd_man)
stargazer(mlm.fit_fhd_man,type="text")

mlm.fit_fhd_pca=lm(fhd_bio~.,data=data_pca)
summary(mlm.fit_fhd_pca)
stargazer(mlm.fit_fhd_pca,type="text")

seed(100)
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

rf.fit_fhd <-  train(fhd_bio ~.,
                         data = data_sel[,c(1:10,12)],
                         method = "rf",
                         metric = "RMSE",
                         importance=TRUE,
                         ntree=100,
                         trControl = trControl)
rf.fit_fhd

## Visualize
ggnostic(mlm.fit_fhd_vif1)

