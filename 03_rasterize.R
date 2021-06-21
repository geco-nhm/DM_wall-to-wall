## ---------------------------
##
##
## Author: Peter Horvath
##
## Date : 2021-10-02
##
##
## ---------------------------
##
## Notes:
## this script is the third stage in the process of producing a WALL-to-wall map of Vegetation types
## here we rasterize evaluation datasets AR18, AR9 and AR_veg from polygon (vector formats) and also mask the three assembled maps
## 
##
## ---------------------------

library(raster)
library(rgdal)
in_folder <- "C:/DATA_fast_processing/"
out_folder <- "E:/Project_1.5/OUTPUT_agri/"
git_folder <- "C:/Users/peterhor/Documents/GitHub_UiO/Project_1.5/"
# read in DM *preliminary results from 3 DM attempts
#A method
DM_AUC <- raster(paste0(in_folder,"DM_AUC_fill.tif"))
#B method
DM_preval <- raster(paste0(in_folder,"DM_preval_fill.tif"))
#C method
DM_maxval <- raster(paste0(in_folder,"DM_maxval.tif"))

# AR 18x18
ar <- readOGR(dsn = paste0(in_folder, "AR18x18"), layer = "n18x18")
ar_dissolved <- readOGR(dsn = paste0(in_folder, "AR18x18"), layer = "ar18x18_dissolved")
table(ar$VEG1)
#load in translation rules
transl <- read.csv(paste0(git_folder, "transl_VT_DMvt.csv"), sep = ";", dec = ".")
transl <- rename(transl, VEG1 = VT_code)
str(transl)
library(rgdal)
library(dplyr)
ar_sub@data <- left_join(ar_sub@data, transl, by="VEG1")
head(ar_sub@data)
# crop and mask raster ar18x18
plot(DM_maxval)
plot(ar_dissolved, add=TRUE)
DM_maxval_mask <- mask(DM_maxval, ar_dissolved)
DM_preval_mask <- mask(DM_preval, ar_dissolved)
DM_AUC_mask <- mask(DM_AUC, ar_dissolved)
writeRaster(DM_maxval_mask, filename = paste0(out_folder, "DM_maxval_mask.tif"), format="GTiff", overwrite=TRUE)
writeRaster(DM_preval_mask, filename = paste0(out_folder, "DM_preval_mask.tif"), format="GTiff", overwrite=TRUE)
writeRaster(DM_AUC_mask, filename = paste0(out_folder, "DM_AUC_mask.tif"), format="GTiff", overwrite=TRUE)


# crop and mask raster ar 9x9
ar9 <- readOGR(dsn = paste0(in_folder, "AR9x9"), layer = "AR_9xFortett_WGS")
ar9_dissolved <- readOGR(dsn = paste0(in_folder, "AR18x18"), layer = "ar18x18_dissolved")
DM_maxval_mask9 <- mask(DM_maxval, ar9_dissolved)
writeRaster(DM_maxval_mask9, filename = paste0(out_folder, "DM_maxval/DM_maxval_mask9.tif"), format="GTiff", overwrite=TRUE)
DM_preval_mask9 <- mask(DM_preval, ar9_dissolved)
writeRaster(DM_preval_mask9, filename = paste0(out_folder, "DM_preval/DM_preval_mask9.tif"), format="GTiff", overwrite=TRUE)
DM_AUC_mask9 <- mask(DM_AUC, ar9_dissolved)
writeRaster(DM_AUC_mask9, filename = paste0(out_folder, "DM_AUC/DM_AUC_mask9.tif"), format="GTiff", overwrite=TRUE)

## crop and mask raster ar VEG Maps
ar_veg <- readOGR(dsn = paste0(in_folder, "Veg_maps"), layer = "Vegetasjonskart1997-2015_WGS")
ar_veg_dissolved <- readOGR(dsn = paste0(in_folder, "Veg_maps"), layer = "Vegetasjonskart1997-2015_dissolved")
DM_maxval_mask_veg <- mask(DM_maxval, ar_veg_dissolved)
writeRaster(DM_maxval_mask_veg, filename = paste0(out_folder, "DM_maxval/DM_maxval_mask_veg.tif"), format="GTiff", overwrite=TRUE)
DM_preval_mask_veg <- mask(DM_preval, ar_veg_dissolved)
writeRaster(DM_preval_mask_veg, filename = paste0(out_folder, "DM_preval/DM_preval_mask_veg.tif"), format="GTiff", overwrite=TRUE)
DM_AUC_mask_veg <- mask(DM_AUC, ar_veg_dissolved)
writeRaster(DM_AUC_mask_veg, filename = paste0(out_folder, "DM_AUC/DM_AUC_mask_veg.tif"), format="GTiff", overwrite=TRUE)

# rasterize
head(ar_veg@data)
table(ar_veg@data$Veg1)
as.data.frame(table(ar_veg$Veg1))
#load in translation rules
library(rgdal)
library(dplyr)
transl <- read.csv(paste0(git_folder, "transl_VT_DMvt_vegmaps.csv"), sep = ";", dec = ".")
transl <- rename(transl, Veg1 = VT_code)
ar_veg@data <- left_join(ar_veg@data, transl, by="Veg1")
head(ar_veg@data)
# overlap with the evaluation data

ar_veg_raster <- rasterize(ar_veg, DM_maxval, field="DMVT_ID")
writeRaster(ar_veg_raster, filename = paste0(out_folder, "ar_veg_rasterized.tif"), format="GTiff", overwrite=TRUE)
