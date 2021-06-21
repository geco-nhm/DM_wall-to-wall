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
## this script is the first stage in preparing DM data for assembly of a WALL-to-wall map of Vegetation types
## we use AR50 dataset to mask out four land cover classes cities, glaciers, lakes and agricultural areas
## (original name of the script: Mask_AR50_glacier_city_lake_agri)
##
## ---------------------------

# mask out GLACIER, CITY, LAKE
# subset of raster, based on value (multiple values) 
# set all other values to NA
out_data <- "E:/Project_1.5/AR_50_mask_glacier_city_lake/"
ar_50 <- raster("E:/Project_1_FINAL_layers/MASKED_TIFF/ar50_artype.tif")
unique(ar_50)
ar_50[ar_50!=10] = NA
writeRaster(ar_50, filename = paste0(out_data,"/AR_50_city.tiff"), format='GTiff', overwrite=TRUE )
ar_50 <- raster("E:/Project_1_FINAL_layers/MASKED_TIFF/ar50_artype.tif")
ar_50[ar_50!=70] = NA
writeRaster(ar_50, filename = paste0(out_data,"/AR_50_glacier.tiff"), format='GTiff', overwrite=TRUE )
ar_50 <- raster("E:/Project_1_FINAL_layers/MASKED_TIFF/ar50_artype.tif")
ar_50[ar_50!=81] = NA
writeRaster(ar_50, filename = paste0(out_data,"/AR_50_lake.tiff"), format='GTiff', overwrite=TRUE )
ar_50 <- raster("E:/Project_1_FINAL_layers/MASKED_TIFF/ar50_artype.tif")
ar_50[ar_50==128] = NA
writeRaster(ar_50, filename = paste0(out_data,"/AR_50_sea.tiff"), format='GTiff', overwrite=TRUE )
#calc(ar_50, fun=function(x){x[x!=c(10,70,81)] <- NA; return(x)})

# writeRaster(ar_50, filename = paste0(out_data,"/AR_50_inverse_mask.tiff"), format='GTiff', overwrite=TRUE )
# ar_50_inv_mask <- raster(paste0(out_data,"/AR_50_inverse_mask.tif"))
# AR_50_MASK <- mask(ar_50, ar_50_inv_mask, inverse=TRUE, filename = paste0(out_data,"/AR_50_mask.tiff"), format='GTiff', overwrite=TRUE )

#represent agriculture - arable land 24, pasture 25 (https://www.nibio.no/tjenester/nedlasting-av-kartdata/dokumentasjon/ar50)
ar_50_arable <- raster("E:/Project_1_GIS_backup/Antropo/AR_50_agricult.tif")
#unique(ar_50_agri)
#(ar_50_agri)
ar_50_arable[ar_50_arable!=24] = NA
writeRaster(ar_50_arable, filename = paste0(out_data,"/AR_50_arable.tiff"), format='GTiff', overwrite=TRUE )
#ar_50_arable <- raster(paste0(out_data,"/AR_50_arable.tiff"))

#pasture 25 - THIS SHOULD STAY AVAILABLE FOR MODELLING *(VT 11b - pastures)
ar_50_pasture <- raster("E:/Project_1_GIS_backup/Antropo/AR_50_agricult.tif")
ar_50_pasture[ar_50_pasture!=25] = NA
writeRaster(ar_50_pasture, filename = paste0(out_data,"/AR_50_pasture.tiff"), format='GTiff', overwrite=TRUE )
#ar_50_pasture <- raster(paste0(out_data,"/AR_50_arable.tiff"))

#add 24 to the mask raster
ar_50_mask <- raster("E:/Project_2/LAND COVER DATA/AR_50_mask_glacier_city_lake/AR_50_mask.tif")
ar_50_mask_agri <- merge(ar_50_mask, ar_50_arable)
plot(ar_50_mask_agri)
writeRaster(ar_50_mask_agri, filename = paste0(out_data,"/AR_50_mask_agri.tiff"), format='GTiff', overwrite=TRUE )
#zoom(ar_50_mask_agri)
#unique(ar_50_mask_agri)
#freq(ar_50_mask_agri)


# mask out GLACIER, CITY, LAKE
# ar_50 <- raster("E:/Project_1_FINAL_layers/MASKED_TIFF/ar50_artype.tif")
# unique(ar_50)
# # subset of raster, based on value (multiple values) 
# # set all other values to NA
# ar_50[ar_50==10] = NA
# ar_50[ar_50==70] = NA
# ar_50[ar_50==81] = NA
# ar_50[ar_50==128] = NA
# #calc(ar_50, fun=function(x){x[x!=c(10,70,81)] <- NA; return(x)})
# plot(ar_50)
# writeRaster(ar_50, filename = paste0(out_data,"/AR_50_inverse_mask.tiff"), format='GTiff', overwrite=TRUE )
# ar_50_inv_mask <- raster(paste0(out_data,"/AR_50_inverse_mask.tif"))
# AR_50_MASK <- mask(ar_50, ar_50_inv_mask, inverse=TRUE, filename = paste0(out_data,"/AR_50_mask.tiff"), format='GTiff', overwrite=TRUE )

# mask and output all DM by GLACIER, CITY, LAKE, AGRI
out_data <- "E:/Project_1.5/AR_50_mask_glacier_city_lake"
out_data_DM <- "E:/Project_1.5/DM_masked_with_AR50_agri"
in_probab_raster <- "E:/Project_1_RUNS/new_MODEL_RUN/04_predict_raster/"
r.list <- list.files(in_probab_raster, pattern="tif$", full.names=TRUE)
master_stack <- stack(r.list)
names(master_stack)
# mask out GLACIER, CITY, LAKE

for(i in 1:nlayers(master_stack)){
  print(names(master_stack[[i]]))
  master_stack_masked <- mask(master_stack[[i]], ar_50_mask_agri, inverse=TRUE) #AR_50_MASK
  writeRaster(master_stack_masked, filename = paste0(out_data_DM,"/",names(master_stack[[i]])), format='GTiff', overwrite=TRUE )
  print(Sys.time())
}

# create layer where only 4 binary rasters are left, (# 4b, 7a, 8cd, 11b)
# these are used to calculate MAX_Value and substitute into empty cells from Max_preval and Max_AUC
#load in the new files
r.list <- list.files(out_data_DM, pattern="tif$", full.names=TRUE)
master_stack <- stack(r.list)
#master_stack4 <-master_stack[[c(11,18,23,29)]] 
master_stack4 <-master_stack[[c("X4b_predict_GLM", "X7a_predict_GLM", "X8cd_predict_GLM", "X11b_predict_GLM")]] 
plot(master_stack4)
max_val_4 <- which.max(master_stack4)
plot(max_val_4)
#this is raster with original values from 1 to 4
writeRaster(max_val_4, filename = paste0(out_data,"/maxval_4ras.tiff"), format='GTiff', overwrite=TRUE )
# change values from number in raster stack to real value of VT
max_val_4[max_val_4==1] <- 11
max_val_4[max_val_4==2] <- 18
max_val_4[max_val_4==3] <- 23
max_val_4[max_val_4==4] <- 29
plot(max_val_4)
# save as a raster to fill out the remaining cells for DM_AUC and DM_preval
writeRaster(max_val_4, filename = paste0(out_data,"/maxval_4b_7a_8cd_11b.tiff"), format='GTiff', overwrite=TRUE )
