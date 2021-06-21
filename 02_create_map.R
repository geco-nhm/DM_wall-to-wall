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
## this script is the second stage in the assembly process of a WALL-to-wall map of Vegetation types
## here we combine the 31 VTs using three different approaches MAXVAL, AUC and PREVAL
## 
##
## ---------------------------

#### load libraries ####
library(raster)
library(maptools)
library(sf)
library(rgdal)
library(rgeos) 
library(sp)
library(ggplot2)
library(data.table) 
library(dplyr)
library(gtools)

# load lists of VT codes and names
vt_list <- list(
  "1ab","1c", "2a", "2b","2c","2d","2ef","2g",
  "3ab","4a","4b","4c","4e", "4g",
  "5ab","6a","6b","7a","7b","7c",
  "8a","8b","8cd","9ad","9bc","9e",
  "10ab","10c","11b","12b", "12c"
)
vt_names <- list("Moss snowbed / Sedge and grass snowbed", 	"Frozen ground, leeward", 	"Frozen ground, ridge", 	"Dry grass heath", 	"Lichen heath", 	"Mountain avens heath", 	
                 # "Dwarf shrub heath", 	"Alpine calluna heath", 
                 "Dwarf shrub / alpine calluna heath",
                 "Alpine damp heath", 	"Low herb / forb meadow", 	"Lichen and heather birch forest", 	"Bilberry birch forest", 	"Meadow birch forest", 	"Alder forest", 	"Pasture land forest", 	"Poor / Rich broadleaf deciduous forest", 	"Lichen and heather pine forest", 	"Bilberry pine forest", 	"Lichen & heather spruce forest", 	"Bilberry spruce forest", 	"Meadow spruce forest", 	"Damp forest", 	"Bog forest", 	"Poor / rich swamp forest", 	"Bog / Mud-bottom fen and bog", 	"Deer-grass fen / fen", 	"Sedge marsh", 	"Coastal heath / Coastal calluna heath", 	"Damp heath", 	"Pastures", 
                 #"Barren land",
                 "Boulder field", 	"Exposed bedrock"
                  )
#### create map where highest value for each raster cell wins and therein the VT code is passed in as a attribute

#### choose LOCAL or SERVER ####
#### Define where to run scripts and thus change paths 
# all AR18x18 dissolved data
local_or_server <- "NHM" # other option is "GEOHYD"
if (local_or_server == "METOS") {
  print("METOS")
  drive_letter <- "D:/"
  } else if (local_or_server == "GEOHYD") {
  print("GEOHYD")
  drive_letter <- "Z:/"
  } else if (local_or_server == "NHM") {
    print("NHM")
    drive_letter <- "E:/"
  } else {print("error: wrong machine")}

# load paths
proj_folder <- "C:/Users/peterhor/Documents/GitHub/Project_1.5/"
in_binary_raster <- paste0(drive_letter,"Project_1_RUNS/new_MODEL_RUN/07_proportion_raster/Binary_raster/")
#in_probab_raster <- paste0(drive_letter,"Project_1_RUNS/new_MODEL_RUN/04_predict_raster/")
out_data <- paste0(drive_letter,"Project_1.5/AR_50_mask_glacier_city_lake")
out_data_DM <- paste0(drive_letter,"Project_1.5/DM_masked_with_AR50_agri")
out_folder <- paste0(drive_letter,"Project_1.5/OUTPUT_agri/")
out_folder_maxval <- paste0(out_folder, "DM_maxval/")
out_folder_preval <- paste0(out_folder, "DM_preval/")
out_folder_AUC <- paste0(out_folder, "DM_AUC/")
# dir.create(file.path(out_folder),showWarnings=F)

### Garbage collector squat ####
# empty memory within a loop/function
gcs <- function() {
  for(i in 1:10){
    gc()
  }
}
gcs()

####################### FUNCTION INDEXING MODEL ####
### function that returns indexes of N highest probabilites within the vector 
# with corresponding values

getNIndexesFromRaster <- function(rast, num, name) {
  trashold = -sort(-values(rast), partial = num)[num]
  rast[rast<trashold] = NA
  rast[rast>0] = name
  return(rast)
  
}
# # Test
# A = getNIndexesFromRaster(A, 6, 1)
# B = getNIndexesFromRaster(B, 6, 2)
# plot(A)
# plot(B)
# plot(merge(A,B))
####################### FUNCTION GENERATE MODEL ####
generateModel <- function(rasterStack, ratios){
  res <- rasterStack[[1]]
  values(res) <- NA
  #res <- raster(ncol=ncol(rasterStack[[1]]), nrow=nrow(rasterStack[[1]]))
  #projection(res) <-"+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  for (i in 1:nrow(ratios)){
    time_iteration_start <- Sys.time()
    print(paste(Sys.time(), " == starting", i ))
    stackIndex = rownames(ratios)[i]
    print(stackIndex)
    thisRaster = rasterStack[[noquote(stackIndex)]]
    nCels = ratios$N_cells[i]
    
    partialRaster = getNIndexesFromRaster(thisRaster, nCels, ratios$ID[i])
    print(paste(Sys.time(), " == finished indexing", i))
    gcs()
    res <- merge(res, partialRaster)
    gcs()
    print(paste(Sys.time(), " == finished overwriting values", i))
    if (i != nrow(ratios)){
      rasterStack[[rownames(ratios)[i+1]]] <- mask(rasterStack[[rownames(ratios)[i+1]]], res, inverse=TRUE)
      print(paste(Sys.time(), " == finished masking", i))
    }
    
    pdf(file = paste0(getwd(),"/", i,".pdf"))
    plot(res)
    title(main = paste("test", i, sep=" "))
    dev.off()
    time_iteration_end <- Sys.time()
    iteration_time <- time_iteration_end-time_iteration_start 
    print( paste(Sys.time(), " == Iteration finished in", iteration_time, sep=" "))
  }
  return(res)
}



# DM MAX VALUE ####


# load all rasters into a stack
print("loading RASTERS")

# read all raster files inside HOME folder and add them to a list
r.list <- list.files(out_data_DM, pattern="tif$", full.names=TRUE)
r.list <- mixedsort(r.list)
r.stack <- stack(r.list)
  # r.list <- list.files(in_probab_raster, pattern="tif$", full.names=FALSE)
  # r.list.path <- list.files(in_probab_raster, pattern="tif$", full.names=TRUE)
names(r.stack)
basename(r.list)
crs(r.stack)
# # for small extent see bottom 

## checking where are the maximum values
# i <- which.max(r.stack[[3]])
# xy <- xyFromCell(r.stack, i)
# plot(r.stack[[3]])
# points(head(xy))
# values(r.stack[[3]], head(xy))

# all norway
time_pred_start <- Sys.time()
max_val <- which.max(r.stack)
writeRaster(max_val, filename = paste0(out_folder_maxval, "DM_maxval.tif"), format="GTiff", overwrite=TRUE)
# max_val <- raster(paste0(out_folder_maxval, "DM_maxval.tif"))
# max_val <- raster(paste0(in_dm_data,"DMtest_whole_WGS_UTM32.tif"))
time_pred_end <- Sys.time()
pred_time <- (time_pred_end-time_pred_start)
print( paste( "calculation finished in", pred_time, sep=" "))
plot(max_val)
# r.freq <- table(getValues(max_val))
raster_freq <- freq(max_val)
write.csv2(raster_freq, paste0(out_folder_maxval,"DM_maxval_freq.csv"))
max_val <- as.factor(max_val)
raster_freq_names <- cbind(raster_freq, vt_names)
plot(max_val)
table(getValues(max_val))


### plot into file!#
library(RColorBrewer)
#hi-res PDF into default folder
pdf(file = paste0(out_folder_maxval,"DM_maxVal.pdf"))
plot(max_val) #c("grey",rev(rainbow(28)))); ,col= brewer.pal(28, "Spectral")
title(main = paste("Maximum Value - unique category"))
dev.off()

#low res TIFF
tiff(filename = paste0(out_folder_maxval,"/DM_maxVal_lowres.tiff",
     height = 27, width = 17, units = 'cm', res=300))
plot(max_val,col= c("grey",rev(rainbow(9))))
title(main = paste("Maximum Value - unique category"))
dev.off()

#ggmap()


# DM BINARY ####
r.list.binary <- list.files(in_binary_raster, pattern="tif$", full.names=FALSE)
r.list.binary.path <- list.files(in_binary_raster, pattern="tif$", full.names=TRUE)
library(gtools)
r.list.binary <- mixedsort(r.list.binary.path)
# read in raster layers and stack them into raster stack
r.stack.binary <- stack(r.list.binary)
basename(r.list)
crs(r.stack.binary)
# add values for all 31 VTs to calculate overlapping cells
# cells with higher count = multiple overlapping layers
binary_count <- cellStats(r.stack.binary, stat = "sum")
barplot(binary_count)

r.stack.binary_sum <- sum(r.stack.binary)
plot(r.stack.binary_sum, col=heat.colors(9))
writeRaster(r.stack.binary_sum, filename = paste0(out_folder,"/DM_binary_sum"), format="GTiff", overwrite=TRUE)
# r.stack.binary_sum <- raster(paste0(out_folder,"/DM_binary_sum.tif"))
# how many cells are empty, or classified by more than one VEG type. 
raster_binary_stats <- freq(r.stack.binary_sum)

### plot!#
#hi-res PDF
pdf(file = paste0(out_folder,"DM_binary_sum.pdf"))
plot(r.stack.binary_sum,col= c("grey",rev(rainbow(9))))
title(main = paste("Binary_overlap"))
dev.off()

#2 - hi-res TIFF
tiff(filename = paste0(out_folder,"DM_binary_sum.tiff",
     height = 17, width = 17, units = 'cm', res=300))
plot(r.stack.binary_sum,col= c("grey",rev(rainbow(9))))
title(main = paste("Binary_overlap"))
dev.off()


#### DM PREVALENCE ####
r.list <- list.files(out_data_DM, pattern="tif$", full.names=TRUE)
r.list <- mixedsort(r.list)
# exclude binary misfits
# 4b, 7a, 8cd, 11b
master_stack <- stack(r.list[-c(11,18,23,29)])
names(master_stack)

  # mask out GLACIER, CITY, LAKE this is done in other file "Mask_AR50_glacier_city_lake.R'
  # master_stack_masked <- mask(master_stack, AR_50_MASK, inverse=TRUE)
# how many raster cells are not NA (land area)
ncell(master_stack[[1]][master_stack[[1]]>0]) 
#result 29740230

#read in table with data about total coverage (%) in norway (assign prevalence)
proportions <- read.csv(paste0(drive_letter,"Project_1.5/NORGE_veg_type_PROPORTION.csv"))
df_prev <- proportions
colnames(df_prev)[4] <- "percentage"
colnames(df_prev)[3] <- "area_cover"
df_prev$percentage <- as.numeric(as.character(proportions[, 4]))
#df_prev$percentage <- df_prev$percentage/100
df_prev$N_cells <- as.integer(df_prev$area_cover*100) # area in km2 to pixels or N_cells
df_prev$ID <- as.integer(rownames(df_prev))
  # check sum(df_prev$percentage)
  # sum(df_prev$N_cells)
# exclude binary misfits
# 4b, 7a, 8cd, 11b
df_prev <- df_prev[-c(11,18,23,29),]
rownames(df_prev) <- names(master_stack)
df_prev <- df_prev[order(df_prev$percentage), ]
NROW(df_prev)
write.csv(df_prev, paste0(out_folder_preval,"df_preval.csv"))


max_preval <- generateModel(master_stack, df_prev)
plot(max_preval)
writeRaster(max_preval, filename = paste0(out_folder_preval,"DM_preval.tiff"), format='GTiff', overwrite=TRUE )
# max_preval <- raster(paste0(out_folder_preval,"DM_preval_fill.tif"))

### plot into file!#
library(RColorBrewer)
#hi-res PDF into default folder
pdf(file = paste0(out_folder_preval,"DM_preval.pdf"))
plot(max_preval,col= brewer.pal(28, "Spectral")) #c("grey",rev(rainbow(28))))
title(main = paste("ranged by Prevalence"))
dev.off()

# low-res TIFF
tiff(filename = paste0(out_folder_preval,"DM_preval_lowres.tiff"),
     height = 27, width = 17, units = 'cm', res=300)
plot(max_preval,col= c("grey",rev(rainbow(9))))
title(main = paste("ranged by Prevalence"))
dev.off()


# #### assign VT values to raster
# max_preval_r <- ratify(max_preval)
# rat <- levels(max_preval_r)[[1]]
# rat <- cbind(rat,master_prev[,c(1:2,4)])
# colnames(rat)
# levels(max_preval_r) <- rat
# plot(max_preval_r, main="ranged by Prevalence")
# writeRaster(max_preval, filename = paste0(out_folder,"/DM_max_preval_RAT"), format='GTiff', overwrite=TRUE )
# write.csv(rat, file = paste0(out_folder,"/DM_max_preval_RAT.csv"), row.names = F)
# writeRaster(max_preval, filename = paste0(out_folder,"/DM_max_preval_RAT.grd"),  overwrite=TRUE )
# #check stats for coverage - does it fit with expected coverage stats?
# max_preval_freq <- freq(max_preval_r)
# write.csv(max_preval_freq, file = paste0(out_folder,"/DM_max_preval_freq.csv"), row.names = F)



#### DM AUC VALUE ####
#read in table with data about total coverage (%) in norway
prop_AUC <- read.csv(paste0(drive_letter,"Project_1.5/NORGE_veg_type_PROP_AUC.csv"))
df_AUC <- prop_AUC
colnames(df_AUC)[4] <- "percentage"
colnames(df_AUC)[3] <- "area_cover"
df_AUC$percentage <- as.numeric(as.character(df_AUC[, 4]))
#master_AUC$percentage <- master_AUC$percentage/100
df_AUC$N_cells <- as.integer(df_AUC$area_cover*100)
df_AUC$ID <- as.integer(rownames(df_AUC))
# check sum(master_AUC$percentage)
# sum(master_AUC$N_cells)
# exclude binary misfits
# 4b, 7a, 8cd, 11b
df_AUC <- df_AUC[-c(11,18,23,29),]
rownames(df_AUC) <- names(master_stack)
df_AUC <- df_AUC[order(df_AUC$AUC, decreasing = T), ]
NROW(df_AUC)
write.csv(df_AUC, paste0(out_folder_AUC,"df_AUC.csv"))

max_AUC <- generateModel(master_stack, df_AUC)
plot(max_AUC, main="max AUC")
writeRaster(max_AUC, filename = paste0(out_folder_AUC,"DM_AUC.tif"), format='GTiff', overwrite=TRUE )
# max_AUC <- raster(paste0(out_folder_AUC,"DM_AUC.tif"))

### plot into file!#
library(RColorBrewer)
#hi-res PDF into default folder
pdf(file = paste0(out_folder_AUC,"DM_AUC.pdf"))
plot(max_AUC,col= brewer.pal(28, "Spectral")) #c("grey",rev(rainbow(28))))
title(main = paste("ranged by AUC"))
dev.off()

# low-res TIFF
tiff(filename = paste0(out_folder_AUC,"DM_AUC_lowres.tiff"),
     height = 27, width = 17, units = 'cm', res=300)
plot(max_AUC,col= c("grey",rev(rainbow(9))))
title(main = paste("ranged by AUC"))
dev.off()


# #### assign VT values to raster
# max_AUC_r <- ratify(max_AUC)
# rat <- levels(max_AUC_r)[[1]]
# rat <- cbind(rat,master_AUC[,c(1:2,4)])
# colnames(rat)
# levels(max_AUC_r) <- rat
# plot(max_AUC_r)
# writeRaster(max_probab, filename = paste0(out_folder,"/DM_AUC_RAT"), format='GTiff', overwrite=TRUE )
# write.csv(rat, file = paste0(out_folder,"/DM_AUC_RAT.csv"), row.names = F) 
# 
# max_AUC_freq <- freq(max_AUC_r)
# write.csv(max_AUC_freq, file = paste0(out_folder,"/DM_max_AUC_freq.csv"), row.names = F)

# this is still not working in QGIS
#https://gis.stackexchange.com/questions/203962/how-to-symbolize-raster-with-field-values-in-qgis-or-grass-gis

#### fill in values from 4 VT with binary rasters ####
# load missing 4 binary (vt7a still not represented - due to low probability of presence)
missing4 <- raster(paste0(out_data,"/maxval_4b_7a_8cd_11b.tif"))
# load and merge maxPreval
max_preval <- raster(paste0(out_folder_preval,"DM_preval.tif"))
max_preval_fill <- merge(max_preval, missing4)
writeRaster(max_preval_fill, filename = paste0(out_folder_preval,"DM_preval_fill.tif"), format='GTiff', overwrite=TRUE)
max_preval_fill_freq <- freq(max_preval_fill)
write.csv2(max_preval_fill_freq, file = paste0(out_folder_preval,"DM_preval_freq.csv"))
# max_preval_fill <- raster(paste0(out_folder,"/DM_max_preval/DM_preval_fill.tif"))

# load and merge maxAUC
max_AUC <- raster(paste0(out_folder_AUC,"DM_AUC.tif"))
max_AUC_fill <- merge(max_AUC, missing4)
writeRaster(max_AUC_fill, filename = paste0(out_folder_AUC,"DM_AUC_fill.tif"), format='GTiff', overwrite=TRUE)
max_AUC_fill_freq <- freq(max_AUC_fill)
write.csv2(max_AUC_fill_freq, file = paste0(out_folder_AUC,"DM_AUC_freq.csv"))
# max_AUC_fill <- raster(paste0(out_folder_AUC,"DM_AUC_fill.tif"))

DM_methods_freq <- list(raster_freq, max_AUC_fill_freq, max_preval_fill_freq)



