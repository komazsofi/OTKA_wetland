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
library(xtable)
library(Hmisc)
library(caret)

# Import

workdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/2019Nov/"
#workdir="D:/Sync/_Amsterdam/11_AndrasProject/Analysis/2019Nov/"
setwd(workdir)

lidar_wquadratfield=read.csv("lidar_wquadratfield.csv")

# FHD
forFHD_field=subset(lidar_wquadratfield[(lidar_wquadratfield$season == "leafoff" & lidar_wquadratfield$veg_type_sp!="carex" 
                                         & lidar_wquadratfield$fhd_bio!=0), ],select=c(7,8,9,10,14,16,4,15,57))
# FHD
forFHD_field %>%
  gather(-fhd_bio,-lake,-class,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = fhd_bio)) +
  geom_point(aes(shape=lake,color=class)) +
  geom_smooth(method="lm",colour="black",se=TRUE) +
  stat_regline_equation(aes(label = paste(..rr.label.., sep = "~~~"))) +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Quadrat FHD vs. pole-contact measurements") +
  labs(x="Pole-contact measures",y="Quadrat based FHD")+
  theme_bw()

# What could I include in the model?
res2<-rcorr(as.matrix(forFHD_field[,c(1:6,8)]))

model <- lm(fhd_bio~fhd_pole+lai, data = forFHD_field)
allpossible=ols_step_all_possible(model)
best=ols_step_best_subset(model)

ols_plot_cooksd_bar(model)
ols_test_normality(model)
ols_plot_resid_hist(model)
ols_plot_resid_qq(model)
ols_plot_resid_fit(model)

# CV
set.seed(123)
train.control <- trainControl(method = "cv", number=5)
# Train the model
model <- train(fhd_bio~fhd_pole+gap_fraction+sum_pole_contacts, data = forFHD_field, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
