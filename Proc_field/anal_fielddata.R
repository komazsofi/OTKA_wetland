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

totalweight=fielddata_all[c("0-0.5 total weight","1_1.5_total_weight","1.5_2_total_weight","2_2.5_total_weight","2.5_3_total_weight","3_3.5_total_weight","3.5_4_total_weight","4_4.5_total_weight","4.5_5_total_weight","total weight")]

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