"
@author: Zsofia Koma, UvA
Aim: process wetland OTKA project data step : analyse field data
"

library(readxl)
library(data.table)
library(rgdal)

library(ggplot2)
library(scales)
library(gridExtra)

workingdir="D:/Sync/_Amsterdam/11_AndrasProject/field_measured_data/"
setwd(workingdir)

fielddata_all <- read_excel("quadrat_forR.xlsx",sheet = 1)

# Closing error of plot harvested measurements (total)

totalweight=fielddata_all[c("0-0.5 total weight","0-0.5_1_total_weight","1_1.5_total_weight","1.5_2_total_weight","2_2.5_total_weight","2.5_3_total_weight","3_3.5_total_weight","3.5_4_total_weight","4_4.5_total_weight","4.5_5_total_weight","total weight")]

totalweight$calc_totalweight=rowSums(totalweight[,1:9])

totalweight[which(totalweight$calc_totalweight==0),] = NA
totalweight[which(totalweight$`total weight`==0),] = NA
totalweight<-totalweight[complete.cases(totalweight),]

fit=lm(`total weight`~calc_totalweight,data=totalweight)
summary(fit)

equation = function(x) {
  lm_coef <- list(a = round(coef(x)[1], digits = 2),
                  b = round(coef(x)[2], digits = 2),
                  r2 = round(summary(x)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}

ggplot(data =totalweight, aes(x = `total weight`, y = calc_totalweight)) + geom_point() + geom_smooth(method='lm') + theme_minimal() + xlab("Measured total weight [g]")+ylab("Total weight as sum of height layers [g]") +
  ggtitle("Cosing error of field measured total weight") + annotate("text", x = 20000, y = 500, label = equation(fit), parse = TRUE)
ggsave("totalweight_closingerror.png",width = 20, height = 10)

# Closing error of plot harvested measurements (per layers)

layer_0_05_weight=fielddata_all[c("0-0.5_only leaf","0-0.5_only_dry","0-0.5_only_stalk","0-0.5 total weight")]
layer_0_05_weight$calc=rowSums(layer_0_05_weight[,1:3])
layer_0_05_weight[which(layer_0_05_weight$calc==0),] = NA
layer_0_05_weight[which(layer_0_05_weight$`0-0.5 total weight`==0),] = NA
layer_0_05_weight<-layer_0_05_weight[complete.cases(layer_0_05_weight),]

fit=lm(`0-0.5 total weight`~calc,data=layer_0_05_weight)
summary(fit)

layer_05_1_weight=fielddata_all[c("0.5_1_only_stalk","0.5_1_only_dry","0.5_1_only_leaf","0-0.5_1_total_weight")]
layer_05_1_weight$calc=rowSums(layer_05_1_weight[,1:3])
layer_05_1_weight[which(layer_05_1_weight$calc==0),] = NA
layer_05_1_weight[which(layer_05_1_weight$`0-0.5_1_total_weight`==0),] = NA
layer_05_1_weight<-layer_05_1_weight[complete.cases(layer_05_1_weight),]

fit=lm(`0-0.5_1_total_weight`~calc,data=layer_05_1_weight)
summary(fit)

layer_1_15_weight=fielddata_all[c("1-1.5_only_stalk","11-5_only_leaf","1-1.5_only_dry","1_1.5_total_weight")]
layer_1_15_weight$calc=rowSums(layer_1_15_weight[,1:3])
layer_1_15_weight[which(layer_1_15_weight$calc==0),] = NA
layer_1_15_weight[which(layer_1_15_weight$`1_1.5_total_weight`==0),] = NA
layer_1_15_weight<-layer_1_15_weight[complete.cases(layer_1_15_weight),]

fit=lm(`1_1.5_total_weight`~calc,data=layer_1_15_weight)
summary(fit)

layer_15_2_weight=fielddata_all[c("1.5_2_only_stalk","1.5_2_only_leaf","1.5_2_only_dry","1.5_2_total_weight")]
layer_15_2_weight$calc=rowSums(layer_15_2_weight[,1:3])
layer_15_2_weight[which(layer_15_2_weight$calc==0),] = NA
layer_15_2_weight[which(layer_15_2_weight$`1.5_2_total_weight`==0),] = NA
layer_15_2_weight<-layer_15_2_weight[complete.cases(layer_15_2_weight),]

fit=lm(`1.5_2_total_weight`~calc,data=layer_15_2_weight)
summary(fit)

layer_2_25_weight=fielddata_all[c("2_2.5_only_dry","2_2.5_only_stalk","2_2.5_only_leaf","2_2.5_total_weight" )]
layer_2_25_weight$calc=rowSums(layer_2_25_weight[,1:3])
layer_2_25_weight[which(layer_2_25_weight$calc==0),] = NA
layer_2_25_weight[which(layer_2_25_weight$`2_2.5_total_weight`==0),] = NA
layer_2_25_weight<-layer_2_25_weight[complete.cases(layer_2_25_weight),]

fit=lm(`2_2.5_total_weight`~calc,data=layer_2_25_weight)
summary(fit)

layer_25_3_weight=fielddata_all[c("2.5_3_only_dry","2.5_3_only_stalk","2.5_3_only_leaf","2.5_3_total_weight" )]
layer_25_3_weight$calc=rowSums(layer_25_3_weight[,1:3])
layer_25_3_weight[which(layer_25_3_weight$calc==0),] = NA
layer_25_3_weight[which(layer_25_3_weight$`2.5_3_total_weight`==0),] = NA
layer_25_3_weight<-layer_25_3_weight[complete.cases(layer_25_3_weight),]

fit=lm(`2.5_3_total_weight`~calc,data=layer_25_3_weight)
summary(fit)

layer_3_35_weight=fielddata_all[c("3_3.5_only_dry","3_3.55_only_stalk","3_3.5_only_leaf","3_3.5_total_weight" )]
layer_3_35_weight$calc=rowSums(layer_3_35_weight[,1:3])
layer_3_35_weight[which(layer_3_35_weight$calc==0),] = NA
layer_3_35_weight[which(layer_3_35_weight$`3_3.5_total_weight`==0),] = NA
layer_3_35_weight<-layer_3_35_weight[complete.cases(layer_3_35_weight),]

fit=lm(`3_3.5_total_weight`~calc,data=layer_3_35_weight)
summary(fit)

layer_35_4_weight=fielddata_all[c("3.5_4_only_dry","3.5_4_only_stalk","3.5_4_only_leaf" ,"3.5_4_total_weight" )]
layer_35_4_weight$calc=rowSums(layer_35_4_weight[,1:3])
layer_35_4_weight[which(layer_35_4_weight$calc==0),] = NA
layer_35_4_weight[which(layer_35_4_weight$`3.5_4_total_weight`==0),] = NA
layer_35_4_weight<-layer_35_4_weight[complete.cases(layer_35_4_weight),]

fit=lm(`3.5_4_total_weight`~calc,data=layer_35_4_weight)
summary(fit)

layer_4_45_weight=fielddata_all[c("4_4.5_only_dry","4_4.5_only_stalk","4_4.5_only_leaf" ,"4_4.5_total_weight" )]
layer_4_45_weight$calc=rowSums(layer_4_45_weight[,1:3])
layer_4_45_weight[which(layer_4_45_weight$calc==0),] = NA
layer_4_45_weight[which(layer_4_45_weight$`4_4.5_total_weight`==0),] = NA
layer_4_45_weight<-layer_4_45_weight[complete.cases(layer_4_45_weight),]

fit=lm(`4_4.5_total_weight`~calc,data=layer_4_45_weight)
summary(fit)

layer_45_5_weight=fielddata_all[c("4.5_5_only_dry","4.5_5_only_stalk","4.5_5_only_leaf" ,"4.5_5_total_weight" )]
layer_45_5_weight$calc=rowSums(layer_45_5_weight[,1:3])
layer_45_5_weight[which(layer_45_5_weight$calc==0),] = NA
layer_45_5_weight[which(layer_45_5_weight$`4.5_5_total_weight`==0),] = NA
layer_45_5_weight<-layer_45_5_weight[complete.cases(layer_45_5_weight),]

fit=lm(`4.5_5_total_weight`~calc,data=layer_45_5_weight)
summary(fit)