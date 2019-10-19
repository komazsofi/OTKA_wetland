library(ggplot2)

workdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Analysis/lidar_anal/"
setwd(workdir)

ferto=read.csv("ferto_lidar.csv")
balaton=read.csv("balaton_lidar.csv")
tisza=read.csv("tisza_leafoff_lidar.csv")

ferto$loc<- "Ferto"
balaton$loc<- "Balaton"
tisza$loc<- "Tisza"

colnames(tisza)[45] <- "gap_fracti"

ferto_sub=subset(ferto, select=c("OBJNAME","coords.x1","coords.x2","class","Amplitude_entropy","Amplitude_max","Amplitude_mean","Amplitude_rms",
                             "DEM","nDSM","NormalizedZ_P10","NormalizedZ_P20","NormalizedZ_P30","NormalizedZ_P40","NormalizedZ_P50","NormalizedZ_P60",
                             "NormalizedZ_P70","NormalizedZ_P80","NormalizedZ_P90","NrOfEchos_max","pcount","pdens","quadraticEntropy","shannonEntropy",
                             "Z_entropy","Z_range","Z_stdv","Z_variance","gct_lai","gap_fracti","loc"))

tisza_sub=subset(tisza, select=c("OBJNAME","coords.x1","coords.x2","class","Amplitude_entropy","Amplitude_max","Amplitude_mean","Amplitude_rms",
                                 "DEM","nDSM","NormalizedZ_P10","NormalizedZ_P20","NormalizedZ_P30","NormalizedZ_P40","NormalizedZ_P50","NormalizedZ_P60",
                                 "NormalizedZ_P70","NormalizedZ_P80","NormalizedZ_P90","NrOfEchos_max","pcount","pdens","quadraticEntropy","shannonEntropy",
                                 "Z_entropy","Z_range","Z_stdv","Z_variance","gct_lai","gap_fracti","loc"))

balaton_sub=subset(balaton, select=c("OBJNAME","coords.x1","coords.x2","class","Amplitude_entropy","Amplitude_max","Amplitude_mean","Amplitude_rms",
                                 "DEM","nDSM","NormalizedZ_P10","NormalizedZ_P20","NormalizedZ_P30","NormalizedZ_P40","NormalizedZ_P50","NormalizedZ_P60",
                                 "NormalizedZ_P70","NormalizedZ_P80","NormalizedZ_P90","NrOfEchos_max","pcount","pdens","quadraticEntropy","shannonEntropy",
                                 "Z_entropy","Z_range","Z_stdv","Z_variance","gct_lai","gap_fracti","loc"))

forlai=rbind(ferto_sub,tisza_sub,balaton_sub)

gct_lai=data.frame(cor(forlai[5:28], forlai$gct_lai,method = "spearman"))
gct_lai1=data.frame(cor(ferto_sub[5:28], ferto_sub$gct_lai,method = "spearman"))
gct_lai2=data.frame(cor(tisza_sub[5:28], tisza_sub$gct_lai,method = "spearman"))
gct_lai3=data.frame(cor(balaton_sub[5:28], balaton_sub$gct_lai,method = "spearman"))

gct_fract=data.frame(cor(forlai[5:28], forlai$gap_fracti,method = "spearman"))
gct_fract1=data.frame(cor(ferto_sub[5:28], ferto_sub$gap_fracti,method = "spearman"))
gct_fract2=data.frame(cor(tisza_sub[5:28], tisza_sub$gap_fracti,method = "spearman"))
gct_fract3=data.frame(cor(balaton_sub[5:28], balaton_sub$gap_fracti,method = "spearman"))

gct_lai_all=cbind(gct_lai,gct_lai1,gct_lai2,gct_lai3,gct_fract,gct_fract1,gct_fract2,gct_fract3)
colnames(gct_lai_all)[1] <- "gct_lai_all"
colnames(gct_lai_all)[2] <- "gct_lai_ferto"
colnames(gct_lai_all)[3] <- "gct_lai_tisza"
colnames(gct_lai_all)[4] <- "gct_lai_balaton"

colnames(gct_lai_all)[5] <- "gap_fracti_all"
colnames(gct_lai_all)[6] <- "gap_fracti_ferto"
colnames(gct_lai_all)[7] <- "gap_fracti_tisza"
colnames(gct_lai_all)[8] <- "gap_fracti_balaton"

write.csv(gct_lai_all,"cover_all_corr.csv")

ggplot(data =forlai, aes(x=gct_lai, y=pdens,colour=factor(loc))) + geom_point(aes(colour=factor(loc))) + geom_smooth(method='lm') + theme_minimal()
ggplot(data =forlai, aes(x=gct_lai, y=pcount,colour=factor(loc))) + geom_point(aes(colour=factor(loc))) + geom_smooth(method='lm') + theme_minimal()
