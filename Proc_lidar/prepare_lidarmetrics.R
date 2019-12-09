library(gdalUtils)
library(rgdal)
library(raster)
library(dplyr)
library(sdm)
library(stringr)

workingdir="C:/Koma/00_AndrasProjekt/lidar/balaton/"
#workingdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Dataset/lidar/tiszaToLeafOff-20191026T193315Z-001/tiszaToLeafOff/"
#workingdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Dataset/lidar/tiszaToLeafOn-20191026T205134Z-001/tiszaToLeafOn/"
#workingdir="C:/Koma/Sync/_Amsterdam/11_AndrasProject/Dataset/lidar/balaton-20191026T153059Z-001/balaton/"
setwd(workingdir)

crs_proj="+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"
#crs_proj="+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs"

shp=readOGR(".","w_point_balaton")
#shp=readOGR(".","w_point")
#shp=readOGR(".","tisza_full")
shp.df <- as(shp, "data.frame")

shp_sel=subset(shp.df, select=c("coords.x1","coords.x2","class","OBJNAME"))

coordinates(shp_sel)=~coords.x1+coords.x2
proj4string(shp_sel)<- CRS(crs_proj)

filelist=list.files(pattern = "*.tif")

id=sub("_.*", "", filelist)
id=unique(id)

feaname=substring(filelist, 10)

feaname=str_remove(feaname, "[1_]")

feaname=unique(feaname)

for (i in id) {
  print(i)
  
  rastlist=list.files(pattern=paste("^",i,".*\\.tif$",sep=""))
  raster_example=raster(rastlist[1],crs=crs_proj)
  
  for (k in 1:length(rastlist)){
    r <- raster(rastlist[k], crs=crs_proj)
    rp <- projectRaster(from = r, to = raster_example,
                        filename = file.path("./crop", rastlist[k]),
                        method = "bilinear",
                        format = "raster",
                        overwrite = TRUE)
  }
  
  rastlist2=list.files(path="./crop",pattern=paste("^",i,".*\\.grd$",sep=""),full.names = TRUE)
  rastlist3=list.files(path="./crop",pattern=paste("^",i,".*\\.grd$",sep=""),full.names = FALSE)
  
  feaname=substring(rastlist3, 10)
  feaname=str_remove(feaname, "[1_]")
  feaname=str_remove(feaname, "[_]")
  feaname=str_remove(feaname, "[_]")
  
  rasters=stack(rastlist2)
  
  crs(rasters) <- crs_proj
  names(rasters) <- feaname
  
  writeRaster(rasters,paste(i,"merged.grd",sep=""),overwrite=TRUE)
  
}

grdlist=list.files(pattern = "*.grd")

for (j in grdlist) {
  print(j)
  
  raster=stack(j)
  print(dim(raster))
  
  possibleError=tryCatch(sdmData(class~.,train=shp_sel,predictors = raster), error = function(e) e)
  
  if(inherits(possibleError, "error")) next
  
  d <- sdmData(class~.,train=shp_sel,predictors = raster)
  data=d@features
  
  intersect_data=merge(x = data, y = shp.df, by = c("OBJNAME"), all.x = TRUE)
  
  name=sub('\\..*', "", j)
  write.csv(intersect_data,paste(name,"intersected.csv",sep=""))
}

files <- list.files(pattern = "*intersected.csv")

allcsv <- lapply(files,function(g){
  read.csv(g, header=TRUE)
})

allcsv_df <- do.call(rbind.data.frame, allcsv)

#write.csv(allcsv_df,"ferto_lidar.csv")
#write.csv(allcsv_df,"tisza_leafoff_lidar.csv")
#write.csv(allcsv_df,"tisza_leafon_lidar.csv")
write.csv(allcsv_df,"balaton_lidar.csv")