library(gdalUtils)
library(rgdal)
library(raster)

file.names <- c("pt000024merged.grd","pt000025merged.grd")
output.vrt <- "pt_24_25.vrt"
gdalbuildvrt(gdalfile=paste("C:/Koma/Sync/_Amsterdam/11_AndrasProject/Dataset/lidar/balaton-20191026T153059Z-001/balaton/",file.names,sep=""),output.vrt=output.vrt)

gdal_translate(src_dataset = "pt_24_25.vrt", 
               dst_dataset = "pt_24_25.tif", 
               output_Raster = TRUE,
               options = c("BIGTIFF=YES", "COMPRESSION=LZW"))

raster=stack("C:/Koma/Sync/_Amsterdam/11_AndrasProject/Dataset/lidar/balaton-20191026T153059Z-001/balaton/pt_24_25.tif")
raster2=stack("C:/Koma/Sync/_Amsterdam/11_AndrasProject/Dataset/lidar/balaton-20191026T153059Z-001/balaton/pt000024merged.grd")

names(raster) <- names(raster2)

writeRaster(raster,"C:/Koma/Sync/_Amsterdam/11_AndrasProject/Dataset/lidar/balaton-20191026T153059Z-001/balaton/pt_24_25_merged.grd",overwrite=TRUE)

setwd("C:/Koma/Sync/_Amsterdam/11_AndrasProject/Dataset/lidar/fertoTo_HU_AT-20191026T210011Z-001/fertoTo_HU_AT/")

file.names <- c("30256275merged.grd","30256250merged.grd")
output.vrt <- "30256275_250.vrt"
gdalbuildvrt(gdalfile=paste("C:/Koma/Sync/_Amsterdam/11_AndrasProject/Dataset/lidar/fertoTo_HU_AT-20191026T210011Z-001/fertoTo_HU_AT/",file.names,sep=""),output.vrt=output.vrt)

gdal_translate(src_dataset = "30256275_250.vrt", 
               dst_dataset = "30256275_250.tif", 
               output_Raster = TRUE,
               options = c("BIGTIFF=YES", "COMPRESSION=LZW"))

raster=stack("C:/Koma/Sync/_Amsterdam/11_AndrasProject/Dataset/lidar/fertoTo_HU_AT-20191026T210011Z-001/fertoTo_HU_AT/30256275_250.tif")
raster2=stack("C:/Koma/Sync/_Amsterdam/11_AndrasProject/Dataset/lidar/balaton-20191026T153059Z-001/balaton/pt000024merged.grd")

names(raster) <- names(raster2)

writeRaster(raster,"C:/Koma/Sync/_Amsterdam/11_AndrasProject/Dataset/lidar/fertoTo_HU_AT-20191026T210011Z-001/fertoTo_HU_AT/30256275_250_merged.grd",overwrite=TRUE)