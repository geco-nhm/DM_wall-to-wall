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
## this script is the fourth stage in the process of producing a WALL-to-wall map of Vegetation types
## here we evaluate the three assembled maps using kappa
## 
##
## ---------------------------

library(raster)
library(rgdal)
in_folder <- "E:/Project_1.5/OUTPUT_agri"
out_folder <- "E:/Project_1.5/OUTPUT_agri"
git_folder <- "C:/Users/peterhor/Documents/GitHub_UiO/Project_1.5/"
out_folder_maxval <- paste0(out_folder, "/DM_maxval/")
out_folder_preval <- paste0(out_folder, "/DM_preval/")
out_folder_AUC <- paste0(out_folder, "/DM_AUC/")
# read in DM *preliminary results from 3 DM attempts
#A method
DM_AUC <- raster(paste0(in_folder,"/DM_AUC/DM_AUC_fill.tif"))
#B method
DM_preval <- raster(paste0(in_folder,"/DM_preval/DM_preval_fill.tif"))
#C method
DM_maxval <- raster(paste0(in_folder,"/DM_maxVal/DM_maxval.tif"))


  
  #### Eval 1 #### 
# # compare overlap between 3 methods
# extent(DM_AUC)
#### OVERLAY VT - small extent ####
# test_ext <- extent(c(-2000,10000,6600000,6610000))
# #plot(DM_maxval)
# #plot(test_ext, add=TRUE)
# DM_AUC_test <- crop(DM_AUC, test_ext)
# DM_Maxval_test <- crop(DM_maxval, test_ext)
# DM_preval_test <- crop(DM_preval, test_ext)
# 
# DM_AUC_test <- as.factor(DM_AUC_test)
# DM_Maxval_test <- as.factor(DM_Maxval_test)
# DM_preval_test <- as.factor(DM_preval_test)
# 
# par(mfrow=c(1,2))
# plot(DM_AUC_test)
# plot(DM_Maxval_test)
#   
# #spatialEco package 
# 
# library(spatialEco)
# r.kappa <- raster.change(DM_AUC_test,DM_Maxval_test)
# par(mfrow=c(1,3))
# plot(DM_AUC_test)
# plot(DM_Maxval_test)
# plot(r.kappa)
# table(values(DM_AUC_test))
# table(values(DM_Maxval_test))
# r1<- overlay(DM_AUC_test,DM_Maxval_test, fun=function(x, y){ x - y} )
# par(mfrow=c(1,3))
# plot(DM_AUC_test)
# plot(DM_Maxval_test)
# breakpoints <- c(-100,-0.1,0.1, 100)
# colors <- c("black","red", "black")
# plot(r1, col=colors, breaks=breakpoints)
# table(values(r1))
# sum(table(values(r1)))
# 
# class.comparison(DM_AUC_test,DM_Maxval_test, x.idx = 8, y.idx = 8, stat="both") #does not work with rasters
# # try diffeR package
# library(diffeR)
# 
# crosstabm(DM_AUC_test,DM_Maxval_test)
# MADscatterplot(DM_AUC_test,DM_Maxval_test)
# categoryComponentsPlot(DM_AUC_test,DM_Maxval_test)
# categorySourcesPlot(DM_AUC_test,DM_Maxval_test)
# differenceMR(DM_AUC_test,DM_Maxval_test, fact = 2)
# diffTablej(crosstabm(DM_AUC_test,DM_Maxval_test))
# exchangeDij(crosstabm(DM_AUC_test,DM_Maxval_test))
# exchangeDj(crosstabm(DM_AUC_test,DM_Maxval_test))
# overallAllocD(crosstabm(DM_AUC_test,DM_Maxval_test))
# overallComponentsPlot(DM_AUC_test,DM_Maxval_test)
# overallSourcesPlot(DM_AUC_test,DM_Maxval_test)
# overallDiffCatj(crosstabm(DM_AUC_test,DM_Maxval_test))
# # find intersection of all three rasters
# r3<- overlay(DM_AUC_test,DM_Maxval_test,DM_preval_test, fun=function(x, y, z){(x==y)*(x==z)*(y==z)} )
# plot(r3)
# table(values(r3))

#### OVERLAY VT whole raster ####
#first testing subtraction
# r_whole <- overlay(DM_AUC,DM_maxval, fun=function(x, y){ x - y} )
# par(mfrow=c(1,3))
# plot(DM_AUC)
# plot(DM_maxval)
# breakpoints <- c(-100,-0.1,0.1, 100)
# colors <- c("black","red", "black")
# plot(r_whole, col=colors, breaks=breakpoints)
# but better with binary tests
# table(values(DM_AUC))
# table(values(DM_preval))
# table(values(DM_maxval))
rAB <- overlay(DM_AUC,DM_preval, fun=function(x, y){(x==y)} )
rAC <- overlay(DM_AUC,DM_maxval, fun=function(x, y){(x==y)} )
rBC <- overlay(DM_preval, DM_maxval, fun=function(x, y){(x==y)} )
rABC <- overlay(DM_AUC,DM_preval,DM_maxval, fun=function(x, y, z){(x==y)*(x==z)*(y==z)} )
par(mfrow=c(2,2))
plot(rAB, main="AUC vs Preval")
plot(rAC, main="AUC vs Maxval")
plot(rBC, main="Preval vs Maxval")
plot(rABC, main="AUC vs Preval vs Maxval")
# create table to output EVALUATION data into 
nrows <- length(32)
comp_ABC <- data.frame(AB_TRUE=integer(nrows), AB_FALSE=integer(nrows), AC_TRUE=integer(nrows),
                     AC_FALSE=integer(nrows),BC_TRUE=integer(nrows), BC_FALSE=integer(nrows), 
                     ABC_TRUE =integer(nrows), ABC_FALSE=integer(nrows))

comp_ABC[1,2:1] <- t(table(values(rAB)))
comp_ABC[1,4:3] <- t(table(values(rAC)))
comp_ABC[1,6:5] <- t(table(values(rBC)))
comp_ABC[1,8:7] <- t(table(values(rABC)))

#percentage agreement vs total
comp_ABC[2,2:1]<-comp_ABC[1,1]/(comp_ABC[1,1]+comp_ABC[1,2])
comp_ABC[2,4:3]<-comp_ABC[1,3]/(comp_ABC[1,3]+comp_ABC[1,4])
comp_ABC[2,6:5]<-comp_ABC[1,5]/(comp_ABC[1,5]+comp_ABC[1,6])
comp_ABC[2,8:7]<-comp_ABC[1,7]/(comp_ABC[1,7]+comp_ABC[1,8])

write.csv(comp_ABC, paste0(out_folder, "overlay_3methods.csv"))

# tab <- data.frame(ncol(4), nrow(3))
# tab <- data.frame(AB=integer(3), AC=integer(nrows), BC=integer(nrows), ABC =integer(nrows))
# tab[2:1,1] <- t(table(values(rAB)))
# tab[2:1,2] <- t(table(values(rAC)))
# tab[2:1,3] <- t(table(values(rBC)))
# tab[2:1,4] <- table(values(rABC))

# SAVE OUTPUTS
writeRaster(rAB, filename = paste0(out_folder,"compare_AUC_preval.tiff"),format = "GTiff")
writeRaster(rAC, filename = paste0(out_folder,"compare_AUC_maxval.tiff"),format = "GTiff")
writeRaster(rBC, filename = paste0(out_folder,"compare_preval_maxval.tiff"),format = "GTiff")
writeRaster(rABC, filename = paste0(out_folder,"compare_all3.tiff"),format = "GTiff")
# rAB <- raster(paste0(out_folder, "Overlay_3Methods/","DM_AUC_preval_agreement.tif"))
# rAC <- raster(paste0(out_folder,"Overlay_3Methods/","DM_AUC_maxval_agreement.tif"))
# rBC <- raster(paste0(out_folder,"Overlay_3Methods/","DM_preval_maxval_agreement.tif"))
# rABC <- raster(paste0(out_folder, "Overlay_3Methods/","DM_all3_agreement.tif"))

# https://rdrr.io/github/environmentalinformatics-marburg/Rsenal/man/kstat.html





##### Eval 2 ####
#prepare evaluation data#
library(rgdal)
library(dplyr)
in_shp <- "D:/Spatial_DATA/NIBIO_vegetation_mapping/"
# AR 18x18
ar <- readOGR(dsn = paste0(in_shp, "AR18x18"), layer = "n18x18")
table(ar$VEG1)
#load in translation rules
transl <- read.csv(paste0(git_folder, "transl_VT_DMvt.csv"), sep = ";", dec = ".")
transl <- rename(transl, VEG1 = VT_code)
str(transl)

# 
# 
# ### Eval 2 - small extent ####
# ar_sub <- subset(ar, ar@data$FLATE_NR %in% 509)
# #how many polygons of each VT
# table(ar_sub$VEG1)
# #how large area of each VT
# tapply(ar_sub$AREA,ar_sub$VEG1, sum)
# ar_sub@data <- left_join(ar_sub@data, transl, by="VEG1")
# head(ar_sub@data)
# 
# # transl <- matrix(nrow= 31, ncol = 2)
# # # make into reclassify matrix # can also to like this cbind(transl_rule_DM$VT_ID, transl_rule_DM$PFT_ID)
# # for(i in 1:NROW(transl_rule_DM)){
# #   transl[i,] <- c(transl_rule_DM$VT_ID[i], transl_rule_DM$PFT_ID[i])
# #   # print(transl)
# # }
# 
# 
# # overlap with the evaluation data
# plot(DM_Maxval_test)
# plot(ar_sub, add=TRUE)
# # adjust all to cover the same area
# DM_AUC_509 <- crop(DM_AUC_test, ar_sub)
# extent(DM_AUC_509)
# plot(DM_AUC_509)
# #rasterize the 509 test polygon
# ar_sub_r <- rasterize(ar_sub, DM_AUC_509, field="DMVT_ID")
# 
# 
# par(mfrow=c(3,1))
# #par(mfrow=c(2,1))
# plot(ar_sub_r)
# plot(ar_sub, add=TRUE)
# polygonsLabel(ar_sub, ar_sub$VEG1, method = "buffer", cex=.8)
# plot(ar_sub_r)
# text(ar_sub_r)
# plot(DM_AUC_509)
# text(DM_AUC_509)
# # Using library spatialEco for calculating kappa
# library(spatialEco)
# r1 <- DM_AUC_509
# r2 <- ar_sub_r
# s = 3
# r.kappa <- raster.change(r1, r2, d = s, mask = TRUE)
# r.ttest <- raster.change(r1, r2, d = s, stat="t.test", mask = TRUE)
# r.ent <- raster.change(r1, r2, d = s, stat="entropy", mask = TRUE)
# r.cor <- raster.change(r1, r2, d = s, stat="cor", mask = TRUE)
# r.ce <- raster.change(r1, r2, d = s, stat = "cross-entropy", mask = TRUE)
# r.kl <- raster.change(r1, r2, d = s, stat = "divergence", mask = TRUE)
# par(mfrow=c(3,2))
# plot(r.kappa, main="Kappa")
# plot(r.ttest[[1]], main="Paired t-test")
# plot(r.ent, main="Delta Entropy")
# plot(r.cor, main="Rank Correlation")
# plot(r.kl, main="Kullback-Leibler")
# plot(r.ce, main="cross-entropy")
# 
# # save
# writeRaster(ar_sub_r, filename = paste0(out_folder, "ref_plot_509.tif"), format="GTiff", overwrite=TRUE)
# writeRaster(DM_AUC_509, filename = paste0(out_folder, "mod_plot_509.tif"), format="GTiff", overwrite=TRUE)
# writeRaster(ar_sub_r, filename = paste0(out_folder, "ref_plot_509.asc"), format="ascii", overwrite=TRUE)
# writeRaster(DM_AUC_509, filename = paste0(out_folder, "mod_plot_509.asc"), format="ascii", overwrite=TRUE)

####
####
### MASK OUT EVALUATION - whole extent####
# AR 18x18 ####
#  mask raster AR18x18
fast_processing <- "C:/DATA_fast_processing/"
ar_dissolved <- readOGR(dsn = paste0(fast_processing, "AR18x18"), layer = "ar18x18_dissolved") # dissolved in QGIS for better efficiency of masking
extent(ar_dissolved)
plot(DM_maxval)
plot(ar_dissolved, add=TRUE)
#mask the three methods by the AR18 dataset
AR18_DM_maxval <- mask(DM_maxval, ar_dissolved)
writeRaster(AR18_DM_maxval, filename = paste0(out_folder_maxval, "AR18_DM_maxval.tif"), format="GTiff", overwrite=TRUE)
AR18_DM_preval <- mask(DM_preval, ar_dissolved)
writeRaster(AR18_DM_preval, filename = paste0(out_folder_preval, "AR18_DM_preval.tif"), format="GTiff", overwrite=TRUE)
AR18_DM_AUC <- mask(DM_AUC, ar_dissolved)
writeRaster(AR18_DM_AUC, filename = paste0(out_folder_AUC, "AR18_DM_AUC.tif"), format="GTiff", overwrite=TRUE)
#library(rgdal)
#AR18_DM_maxval <- raster(paste0(out_folder, "AR18_DM_maxval.tif"))


# rasterize reference dataset based on a rule from reclassification table
library(dplyr)
#load in reclassification rules
transl <- read.csv(paste0(git_folder, "transl_VT_DMvt.csv"), sep = ";", dec = ".")
transl <- rename(transl, VEG1 = VT_code)
str(transl)
table(ar$VEG1)
ar@data <- left_join(ar@data, transl, by="VEG1")
head(ar@data)
#rasterize reference dataset AR18
AR18_ref <- rasterize(ar, DM_maxval, field="DMVT_ID")
writeRaster(AR18_ref, filename = paste0(out_folder, "AR18_ref.tif"), format="GTiff", overwrite=TRUE)
#AR18_ref <- raster(paste0(out_folder, "AR18_ref.tif"))

####
####
# AR 9X9 ####
# similar procedure as above
# crop and mask raster AR9x9
ar9_dissolved <- readOGR(dsn = paste0(fast_processing, "AR9x9"), layer = "AR_9x9_dissolved")
DM_maxval_mask9 <- mask(DM_maxval, ar9_dissolved)
writeRaster(DM_maxval_mask9, filename = paste0(out_folder_maxval, "AR9_DM_maxval.tif"), format="GTiff", overwrite=TRUE)
DM_preval_mask9 <- mask(DM_preval, ar9_dissolved)
writeRaster(DM_preval_mask9, filename = paste0(out_folder_preval, "AR9_DM_preval.tif"), format="GTiff", overwrite=TRUE)
DM_AUC_mask9 <- mask(DM_AUC, ar9_dissolved)
writeRaster(DM_AUC_mask9, filename = paste0(out_folder_AUC, "AR9_DM_AUC.tif"), format="GTiff", overwrite=TRUE)
# rasterize AR9x9 Maps
ar9 <- readOGR(dsn = paste0(in_folder, "AR9x9"), layer = "AR_9xFortett_WGS")
head(ar9@data)
as.data.frame(table(ar9$VEG1))
#load in translation rules
transl <- read.csv(paste0(git_folder, "transl_VT_DMvt_ar9.csv"), sep = ";", dec = ".")
transl <- rename(transl, VEG1 = VT_code)
table(ar9$VEG1)
ar9@data <- left_join(ar9@data, transl, by="VEG1")
head(ar9@data)
# rasterize reference dataset raster ar18x18 
ar9_raster <- rasterize(ar9, DM_maxval, field="DMVT_ID")
writeRaster(ar9_raster, filename = paste0(out_folder, "AR9_ref.tif"), format="GTiff", overwrite=TRUE)

####
####
# AR VEG maps ####
## crop and mask raster ar VEG Maps
ar_veg_dissolved <- readOGR(dsn = paste0(fast_processing, "Veg_maps"), layer = "Vegetasjonskart1997-2015_dissolved")
DM_maxval_mask_veg <- mask(DM_maxval, ar_veg_dissolved)
writeRaster(DM_maxval_mask_veg, filename = paste0(out_folder_maxval, "VEG_DM_maxval.tif"), format="GTiff", overwrite=TRUE)
DM_preval_mask_veg <- mask(DM_preval, ar_veg_dissolved)
writeRaster(DM_preval_mask_veg, filename = paste0(out_folder_preval, "VEG_DM_preval.tif"), format="GTiff", overwrite=TRUE)
DM_AUC_mask_veg <- mask(DM_AUC, ar_veg_dissolved)
writeRaster(DM_AUC_mask_veg, filename = paste0(out_folder_AUC, "VEG_DM_AUC.tif"), format="GTiff", overwrite=TRUE)
# rasterize VEG Maps
ar_veg <- readOGR(dsn = paste0(in_folder, "Veg_maps"), layer = "Vegetasjonskart1997-2015_WGS")
transl <- read.csv(paste0(git_folder, "transl_VT_DMvt_vegmaps.csv"), sep = ";", dec = ".")
transl <- rename(transl, Veg1 = VT_code)
ar_veg@data <- left_join(ar_veg@data, transl, by="Veg1")
head(ar_veg@data)
#writeOGR(obj=ar_veg, dsn = paste0(in_folder, "Veg_maps"), layer = "Veg_map_DMVT_ID", driver="ESRI Shapefile" )
ar_veg_raster <- rasterize(ar_veg, DM_maxval, field="DMVT_ID")
writeRaster(ar_veg_raster, filename = paste0(out_folder, "VEG_ref.tif"), format="GTiff", overwrite=TRUE)

# ### Merge AR9 and VEG_maps ####
#
# 
# # for simplification of results and procedures, merging ar9 & veg maps for
# #1. DM_maxval, 2. DM_preval, 3. DM_auc, 4. reference of AR9&VEG
# AR
# x <- list(r1, r2)
# names(x) <- c("x", "y")
# x$filename <- 'test.tif'
# x$overwrite <- TRUE
# m <- do.call(merge, x)
###



#### 1 Cohens kappa ####

library(diffeR)
library(spatialEco)
library(rfUtilities)

#final kappa for the whole matrix
kappa.own <- function(m) {
  N <- sum(m)
  No <- sum(diag(m))
  Ne <- 1 / N * sum(colSums(m) * rowSums(m))
  return( (No - Ne) / (N - Ne) )
}
#kappa.own(conf.matrix)

# output all the pairs of rasters 
# REF vs DM AUC, DM preval, DM maxval
# create table to output EVALUATION data into 
nrows <- length(vt_list)
df_kappa <- data.frame(tot.obs=numeric(nrows),
                       tot.cor=numeric(nrows),
                       tot.cor.perc=numeric(nrows),
                       kappa=numeric(nrows)
                       )
# VT RASTERS ####
#read in raster, then reference raster, then do crosstabMATRIX and finally create kappa
#across all the extents
  # AR18
ras18 <- list(raster(paste0(out_folder, "AR18_ref.tif")),
       raster(paste0(out_folder_maxval, "AR18_DM_maxval.tif")),
       raster(paste0(out_folder_AUC   , "AR18_DM_AUC.tif")),
       raster(paste0(out_folder_preval, "AR18_DM_preval.tif"))
       )
ras9 <- list(raster(paste0(out_folder, "AR9_ref.tif")),
      raster(paste0(out_folder_maxval, "AR9_DM_maxval.tif")),
      raster(paste0(out_folder_AUC   , "AR9_DM_AUC.tif")),
      raster(paste0(out_folder_preval, "AR9_DM_preval.tif"))
      
      )
rasVEG <- list(raster(paste0(out_folder, "VEG_ref.tif")),
      raster(paste0(out_folder_maxval, "VEG_DM_maxval.tif")),
      raster(paste0(out_folder_AUC   , "VEG_DM_AUC.tif")),
      raster(paste0(out_folder_preval, "VEG_DM_preval.tif"))
      )
ras<-list(ras18,ras9,rasVEG)
ras  
mod=1
i=1
# for each area that is evaluated
for(mod in 1:3){
  print(mod)
  # so far testing without the upper loop  
  # for each model pair that is evaluated at each area
    for(i in 1:3){
      print(i)
      s <- stack(ras[[mod]][[1]],ras[[mod]][[i+1]])
      # create confusion matrices
      c.mat<-crosstabm(s[[1]],s[[2]])
      write.csv(c.mat)
      # user's/ modelled accuracy 
      UA <- diag(c.mat)/colSums(c.mat)*100
      # producer's/ reference accuracy
      PA <- diag(c.mat)/rowSums(c.mat)*100
      
      # add data to the table matrix
      df_kappa$tot.obs[i] <- sum(c.mat)# total observations
      df_kappa$tot.cor[i] <- sum(diag(c.mat)) # total correct observations
      df_kappa$tot.cor.perc[i] <- sum(diag(c.mat))/sum(c.mat)*100 #Total correct observations in %
      df_kappa$kappa[i] <- kappa.own(c.mat) # kappa
      }
  row=nrow(df_kappa)+1
  
}
names(df_kappa) <- c("DM_maxval", "DM_AUC", "DM_preval")
rownames(df_kappa) <- c("kappa_AR18", "kappa_AR9", "kappa_VEG")

#adjusting kappa function 
#https://blogs.fu-berlin.de/reseda/accuracy-statistics-in-r/
# #individual kappa for each class called Fleiss' kappa
# library(irr)
# m<-c.mat_AR18_maxval
# reduce.matrix <- function(m){
#   for i in m
#   rowSums(m)[i]-i
#   
#   
# }
# (m[1,1]-(rowSums(m)[1]*colSums(m)[1]))/(rowSums(m)[1]+colSums(m)[1])/(2-(rowSums(m)[1]*colSums(m)[1]))
# (m[2,2]-(rowSums(m)[2]*colSums(m)[2]))/(rowSums(m)[2]+colSums(m)[2])/(2-(rowSums(m)[2]*colSums(m)[2]))
# 
# 
# kappam.fleiss(c.mat_AR18_maxval, detail = TRUE)
# 
# kappa.ind <- function(n){
#   #using confusion matrix here
#   #individual entry for each row-total for each row * total for each column / ((total for each row+total for column)/2-total for each row * total for each column )
#   ki=(n[,i])
# }
# (1619-5027*4867)/((5027+4867)/2-(5027*4867))
# summary.accuracy(c.mat_AR18_preval)
# doesn't work yet
#do  manually instead


# VT - total kappa ####
AR18_ref <-       raster(paste0(out_folder, "AR18_ref.tif"))
AR18_DM_maxval <- raster(paste0(out_folder_maxval, "AR18_DM_maxval.tif"))
AR18_DM_AUC <-    raster(paste0(out_folder_AUC   , "AR18_DM_AUC.tif"))
AR18_DM_preval <- raster(paste0(out_folder_preval, "AR18_DM_preval.tif"))

# create confusion matrices
VT_c.mat_AR18_maxval <- crosstabm(AR18_ref,AR18_DM_maxval)
VT_c.mat_AR18_AUC <- crosstabm(AR18_ref,AR18_DM_AUC)
VT_c.mat_AR18_preval <- crosstabm(AR18_ref,AR18_DM_preval)

# calculate kappa
VT_kappa <- data.frame()
VT_kappa[1,1] <- kappa.own(VT_c.mat_AR18_maxval)
VT_kappa[2,1] <- sum(VT_c.mat_AR18_maxval) # total observations
VT_kappa[3,1] <- sum(diag(VT_c.mat_AR18_maxval)) # total correct observations
VT_kappa[4,1] <- sum(diag(VT_c.mat_AR18_maxval))/sum(VT_c.mat_AR18_maxval)*100 #Total correct observations in %

VT_kappa[1,2] <- kappa.own(VT_c.mat_AR18_AUC)
VT_kappa[2,2] <- sum(VT_c.mat_AR18_AUC) # total observations
VT_kappa[3,2] <- sum(diag(VT_c.mat_AR18_AUC)) # total correct observations
VT_kappa[4,2] <- sum(diag(VT_c.mat_AR18_AUC))/sum(VT_c.mat_AR18_AUC)*100 #Total correct observations in %

VT_kappa[1,3] <- kappa.own(VT_c.mat_AR18_preval)
VT_kappa[2,3] <- sum(VT_c.mat_AR18_preval) # total observations
VT_kappa[3,3] <- sum(diag(VT_c.mat_AR18_preval)) # total correct observations
VT_kappa[4,3] <- sum(diag(VT_c.mat_AR18_preval))/sum(VT_c.mat_AR18_preval)*100 #Total correct observations in %


  # AR9
AR9_ref <-        raster(paste0(out_folder, "AR9_ref.tif"))
AR9_DM_maxval <-  raster(paste0(out_folder_maxval, "AR9_DM_maxval.tif"))
AR9_DM_AUC <-     raster(paste0(out_folder_AUC   , "AR9_DM_AUC.tif"))
AR9_DM_preval <-  raster(paste0(out_folder_preval, "AR9_DM_preval.tif"))

# create confusion matrices
VT_c.mat_AR9_maxval <- crosstabm(AR9_ref,AR9_DM_maxval)
VT_c.mat_AR9_preval <- crosstabm(AR9_ref,AR9_DM_preval)
VT_c.mat_AR9_AUC <- crosstabm(AR9_ref,AR9_DM_AUC)
# calculate kappa
VT_kappa[5,1] <- kappa.own(VT_c.mat_AR9_maxval)
VT_kappa[6,1] <- sum(VT_c.mat_AR9_maxval) # total observations
VT_kappa[7,1] <- sum(diag(VT_c.mat_AR9_maxval)) # total correct observations
VT_kappa[8,1] <- sum(diag(VT_c.mat_AR9_maxval))/sum(VT_c.mat_AR9_maxval)*100 #Total correct observations in %

VT_kappa[5,2] <- kappa.own(VT_c.mat_AR9_AUC)
VT_kappa[6,2] <- sum(VT_c.mat_AR9_AUC) # total observations
VT_kappa[7,2] <- sum(diag(VT_c.mat_AR9_AUC)) # total correct observations
VT_kappa[8,2] <- sum(diag(VT_c.mat_AR9_AUC))/sum(VT_c.mat_AR9_AUC)*100 #Total correct observations in %

VT_kappa[5,3] <- kappa.own(VT_c.mat_AR9_preval) 
VT_kappa[6,3] <- sum(VT_c.mat_AR9_preval) # total observations 
VT_kappa[7,3] <- sum(diag(VT_c.mat_AR9_preval)) # total correct observations 
VT_kappa[8,3] <- sum(diag(VT_c.mat_AR9_preval))/sum(VT_c.mat_AR9_preval)*100 #Total correct observations in % 
 
 # VEG
VEG_ref <-        raster(paste0(out_folder, "VEG_ref.tif"))
VEG_DM_maxval <-  raster(paste0(out_folder_maxval, "VEG_DM_maxval.tif"))
VEG_DM_AUC <-     raster(paste0(out_folder_AUC   , "VEG_DM_AUC.tif"))
VEG_DM_preval <-  raster(paste0(out_folder_preval, "VEG_DM_preval.tif"))

# create confusion matrices
VT_c.mat_VEG_maxval <- crosstabm(VEG_ref,VEG_DM_maxval)
VT_c.mat_VEG_AUC <- crosstabm(VEG_ref,VEG_DM_AUC)
VT_c.mat_VEG_preval <- crosstabm(VEG_ref,VEG_DM_preval)

# calculate kappa
VT_kappa[9,1]  <- kappa.own(VT_c.mat_VEG_maxval)
VT_kappa[10,1] <- sum(VT_c.mat_VEG_maxval) # total observations
VT_kappa[11,1] <- sum(diag(VT_c.mat_VEG_maxval)) # total correct observations
VT_kappa[12,1] <- sum(diag(VT_c.mat_VEG_maxval))/sum(VT_c.mat_VEG_maxval)*100 #Total correct observations in %

VT_kappa[9,2]  <- kappa.own(VT_c.mat_VEG_AUC)
VT_kappa[10,2] <- sum(VT_c.mat_VEG_AUC) # total observations
VT_kappa[11,2] <- sum(diag(VT_c.mat_VEG_AUC)) # total correct observations
VT_kappa[12,2] <- sum(diag(VT_c.mat_VEG_AUC))/sum(VT_c.mat_VEG_AUC)*100 #Total correct observations in %

VT_kappa[9,3]  <- kappa.own(VT_c.mat_VEG_preval)
VT_kappa[10,3] <- sum(VT_c.mat_VEG_preval) # total observations
VT_kappa[11,3] <- sum(diag(VT_c.mat_VEG_preval)) # total correct observations
VT_kappa[12,3] <- sum(diag(VT_c.mat_VEG_preval))/sum(VT_c.mat_VEG_preval)*100 #Total correct observations in %

names(VT_kappa) <- c("DM_maxval", "DM_AUC", "DM_preval")
rownames(VT_kappa) <- c("kappa_AR18",
                         "tot.obs_AR18",
                         "tot.obs.corr_AR18",
                         "perc.obs.corr_AR18",
                         "kappa_AR9", 
                         "tot.obs_AR9", 
                         "tot.obs.corr_AR9", 
                         "perc.obs.corr_AR9",
                         "kappa_VEG", 
                         "tot.obs_VEG",
                         "tot.obs.corr_VEG",
                         "perc.obs.corr_VEG"
                         )
round(VT_kappa, digits = 2)
VT_kappa_ord <- VT_kappa[order(VT_kappa$DM_maxval),]
write.csv(VT_kappa, file= paste0(out_folder,"VT_kappa.csv"))
write.csv(VT_kappa_ord, file= paste0(out_folder,"VT_kappa_ord.csv"))
#AR18
write.csv(VT_c.mat_AR18_maxval, file= paste0(out_folder,"VT_c.mat_AR18_maxval.csv"))
write.csv(VT_c.mat_AR18_AUC, file= paste0(out_folder,"VT_c.mat_AR18_AUC.csv"))
write.csv(VT_c.mat_AR18_preval, file= paste0(out_folder,"VT_c.mat_AR18_preval.csv"))

#AR9
write.csv(VT_c.mat_AR9_maxval, file= paste0(out_folder,"VT_c.mat_AR9_maxval.csv"))
write.csv(VT_c.mat_AR9_AUC, file= paste0(out_folder,"VT_c.mat_AR9_AUC.csv"))
write.csv(VT_c.mat_AR9_preval, file= paste0(out_folder,"VT_c.mat_AR9_preval.csv"))

#VEG
write.csv(VT_c.mat_VEG_maxval, file= paste0(out_folder,"VT_c.mat_VEG_maxval.csv"))
write.csv(VT_c.mat_VEG_AUC, file= paste0(out_folder,"VT_c.mat_VEG_AUC.csv"))
write.csv(VT_c.mat_VEG_preval, file= paste0(out_folder,"VT_c.mat_VEG_preval.csv"))





#calculate kappa and individual kappa using the Rossiter functions ####
# VT - individual kappa####
# *AR18 ####
c.mat.AR18.vt <- list(VT_c.mat_AR18_maxval, VT_c.mat_AR18_AUC, VT_c.mat_AR18_preval)
c.mat.AR18.vt.kappa <- lapply(c.mat.AR18.vt, kappa)
#subset 1:5
AR18.vt.tocsv.tot <- sapply(c.mat.AR18.vt.kappa,"[",c(1:5))
colnames(AR18.vt.tocsv.tot) <- c("AR18_DM_maxval", "AR18_DM_AUC", "AR18_DM_preval") 
#write.csv(AR18.vt.tocsv.tot, file= paste0(out_folder,"VT_kappa_total_AR18.csv"))
#subset 6:11
AR18.vt.tocsv <- t(as.data.frame(lapply(c.mat.AR18.vt.kappa, function(x) {x[c(6:8,9)]})))
row.names(AR18.vt.tocsv) <- paste0(row.names(AR18.vt.tocsv), c(rep(c("_AR18_DM_maxval", "_AR18_DM_AUC", "_AR18_DM_preval"), each=4))) 
rownames(AR18.vt.tocsv)
#output to csv
write.csv(AR18.vt.tocsv, file= paste0(out_folder,"VT_kappa_individ_AR18.csv"))

# *AR9 ####
c.mat.AR9.vt <- list(VT_c.mat_AR9_maxval, VT_c.mat_AR9_AUC[1:29, 1:29], VT_c.mat_AR9_preval)
c.mat.AR9.vt.kappa <- lapply(c.mat.AR9.vt, kappa)
#subset 1:5
AR9.vt.tocsv.tot <- sapply(c.mat.AR9.vt.kappa,"[",c(1:5))
colnames(AR9.vt.tocsv.tot) <- c("AR9_DM_maxval", "AR9_DM_AUC", "AR9_DM_preval") 
#write.csv(AR9.vt.tocsv.tot, file= paste0(out_folder,"VT_kappa_total_AR9.csv"))
#subset 6:11
#sapply(c.mat.AR9.vt.kappa,"[",c(6:11))
AR9.vt.tocsv <- t(as.data.frame(lapply(c.mat.AR9.vt.kappa, function(x) {x[c(6:8,9)]})))
row.names(AR9.vt.tocsv) <- paste0(row.names(AR9.vt.tocsv), c(rep(c("_AR9_DM_maxval", "_AR9_DM_AUC", "_AR9_DM_preval"), each=4))) 
rownames(AR9.vt.tocsv)
#output to csv
write.csv(AR9.vt.tocsv, file= paste0(out_folder,"VT_kappa_individ_AR9.csv"))

# *VEG ####
c.mat.VEG.vt <- list(VT_c.mat_VEG_maxval, VT_c.mat_VEG_AUC, VT_c.mat_VEG_preval)
c.mat.VEG.vt.kappa <- lapply(c.mat.VEG.vt, kappa)
#subset 1:5
VEG.vt.tocsv.tot <- sapply(c.mat.VEG.vt.kappa,"[",c(1:5))
colnames(VEG.vt.tocsv.tot) <- c("VEG_DM_maxval", "VEG_DM_AUC", "VEG_DM_preval") 
#write.csv(VEG.vt.tocsv.tot, file= paste0(out_folder,"VT_kappa_total_VEG.csv"))
#subset 6:11
sapply(c.mat.VEG.vt.kappa,"[",c(6:11))
VEG.vt.tocsv <- t(as.data.frame(lapply(c.mat.VEG.vt.kappa, function(x) {x[c(6:8,9)]})))
row.names(VEG.vt.tocsv) <- paste0(row.names(VEG.vt.tocsv), c(rep(c("_VEG_DM_maxval", "_VEG_DM_AUC", "_VEG_DM_preval"), each=4))) 
rownames(VEG.vt.tocsv)
#output to csv
write.csv(VEG.vt.tocsv, file= paste0(out_folder,"VT_kappa_individ_VEG.csv"))

# COMBINE VTs into one dataframe
VT_kappa_all <- cbind(AR18.vt.tocsv.tot, AR9.vt.tocsv.tot, VEG.vt.tocsv.tot)
VT_kappa_all[c(seq(5,45, by=5))] <- as.numeric(VT_kappa_all[c(seq(5,45, by=5))]) # correcting the extra information in the list
write.csv(VT_kappa_all, file= paste0(out_folder,"VT_kappa_all.csv"))


# 
# 
# create.conf.mat <- function(x){
#     conf.matrix<-res_VEG_maxvalXref
#     # user's/ modelled accuracy 
#     UA <- diag(conf.matrix)/colSums(conf.matrix)*100
#     # producer's/ reference accuracy
#     PA <- diag(conf.matrix)/rowSums(conf.matrix)*100
#     # overall accuracy
#     OA <- sum(diag(conf.matrix))/sum(conf.matrix)
#     # add data to the conf matrix
#     conf.matrix.ext <- addmargins(conf.matrix)
#     conf.matrix.ext <- rbind(conf.matrix.ext, "Users" = c(PA, NA))
#     conf.matrix.ext <- cbind(conf.matrix.ext, "Producers" = c(UA, NA, OA))
#     colnames(conf.matrix.ext) <- c(1:31, "Sum", "PA")
#     rownames(conf.matrix.ext) <- c(1:31, "Sum", "UA") #levels(as.factor(shp.train$classes))
#     dimnames(conf.matrix.ext) <- list("Prediction" = colnames(conf.matrix.ext),
#                                  "Reference" = rownames(conf.matrix.ext))
#     #class(conf.matrix.ext) <- "table"
#     conf.matrix.ext <- round(conf.matrix.ext, digits = 1)
#     #conf.matrix.ext[1:31,1:31] <- round(conf.matrix.ext[1:31,1:31], digits = 0)
#     return(conf.matrix.ext)
#     # SAVE TO CSV file
#     # write.csv(tab_kappa, file= paste0(out_folder,names(pred.obs[[1]]),"_x_",names(pred.obs[[2]]),"conf.matrix.csv"))
#     }
# 
# c.mat.ext_VEG_maxval <- create.conf.mat(res_VEG_maxvalXref)
# 
# 
# 
# 
# sign <- binom.test(x = sum(diag(conf.matrix)),
#                    n = sum(conf.matrix),
#                    alternative = c("two.sided"),
#                    conf.level = 0.95)
# sign$conf.int[1:2]
# 
# 
# conf.matrix <- rbind(res_VEG_maxvalXref,colSums(res_VEG_maxvalXref))
# conf.matrix <- cbind(conf.matrix,rowSums(res_VEG_maxvalXref))
# 
# 
# 
#   
# raster_VEG_maxvalXref <- raster.change(VEG_ref,VEG_DM_maxval, d = c(3, 3))
# 
# 
# 
# MADscatterplot(DM_AUC_test,DM_Maxval_test)
# categoryComponentsPlot(DM_AUC_test,DM_Maxval_test)
# categorySourcesPlot(DM_AUC_test,DM_Maxval_test)
# library(SDMTools)
# class(res_VEG_maxvalXref)
# Kappa(res_VEG_maxvalXref)
# 
# 
# ### 2 running window  kappa ####
# ### jeffrey Evans on github
# #https://gis.stackexchange.com/questions/103616/map-accuracy-assessment-by-moving-window-in-r
# require(raster)
# require(asbio)
# 
# setwd("D:/TEST")
# 
# ws <- 3   # window size
# p=0.95   # probability threshold
# 
# # Create example data
# pred <- raster(ncol=100, nrow=100)
# pred[pred] <- runif(length(pred[pred]),0,1)    
# obs <- pred 
# obs[obs] <- runif(length(pred[pred]),0,1) 
# pred2 <- AR18_DM_maxval
# obs2 <- AR18_ref
# pred.obs <- stack(pred2, obs2)
# #pred.obs <- stack(obs,pred)
# names(pred.obs) <- c("pred", "obs")        
# 
# 
# 
# # Loop to read raster in blocks using getValuesFocal  
#   genrate.comparison <- function(pred.obs){
#     ws <- 3   # window size
#     p=0.95   # probability threshold  
#     # Create new on-disk raster
#       s <- writeStart(pred.obs[[1]], "Kappa.img", overwrite=TRUE)  
#       tr <-  blockSize(pred.obs)
#       options(warn=-1)
#       
#       for (i in 1:tr$n) {
#         # Get focal values as list matrix object
#         v <- getValuesFocal(pred.obs, row=tr$row[i], nrows=tr$nrows[i], 
#                             ngb=ws, array=FALSE)                
#         # reclassify data to [0,1] using lapply                       
#         v <- lapply(v, FUN=function(x) {
#           if( length(x[is.na(x)]) == length(x) ) {
#             return( NA ) 
#           } else { 
#             #print(x)
#             return( ifelse(x >= p, 1, 0) ) 
#           }
#         }
#         )   
#         # Loop to calculate Kappa and assign to new raster using writeValues
#         r <- vector() 
#         for( j in 1:dim(v[[1]])[1]) {
#           Obs <- v[[1]][j,]
#           Obs <- Obs[!is.na(Obs)]       
#           Pred <- v[[2]][j,]
#           Pred <- Pred[!is.na(Pred)] 
#           #print(paste('Pred=', Pred))
#           #print(paste('OBS=', Obs))
#           if( length(Obs) >= 2 && length(Obs) == length(Pred) ) {
#             r <- append(r, Kappa(Pred, Obs)$khat)
#           } else {
#             r <- append(r, NA)
#           } 
#         }
#         writeValues(s, r, tr$row[i])
#   }
#   s <- writeStop(s)       
#   k <- raster("Kappa.img")
#   plot(k)
#   return(k)
#   }
# #test1 <- pred.obs
# plot(pred.obs)
# # create table to store kappa values
# tab_kappa <- data.frame(nrow(31),ncol(2))
# # define path to save output
# subfolder<-"AR18_kappa/"
# ### loop through VTs ####
# 
# for(i in 1:31){
#   print(i)
#   #i=11
#   # Split the values of the currently running VT into binary 0 and 1 and NA
#   pred.obs.veg = stackApply(pred.obs,indices = c(1,2), na.rm = FALSE, fun= function(x,...) {
#     if (is.na(x)){
#       return(NA)
#     }
#     
#     if (x==i){
#       return(1)
#     }
#     else{
#       return(0)
#     }
#   })
#   writeRaster(pred.obs.veg, filename = paste0(out_folder,subfolder,names(pred.obs[[1]]),"_x_",names(pred.obs[[2]]),"_VT_",i ,"_binary.tiff"),
#               format="GTiff", overwrite=TRUE)
#   print(paste("stackApply finished for", i))
#   # run the Kappa function on the current VT
#   pred.obs.veg.out <- genrate.comparison(pred.obs.veg)
#   # save output
#   writeRaster(pred.obs.veg.out, filename = paste0(out_folder,subfolder,names(pred.obs[[1]]),"_x_",names(pred.obs[[2]]),"_VT_",i ,"_kappa.tiff"),
#               format="GTiff", overwrite=TRUE)
#   print(paste("generate.comparison finished for", i))
#   # fill table with the kappa data
#   tab_kappa[i,1] <- i
#   veg.kappa.mean <- mean(values(pred.obs.veg.out),na.rm=TRUE)
#   tab_kappa[i,2] <- veg.kappa.mean
#   if (is.na(veg.kappa.mean))
#     next()
#   
#   # thumbnail image (redundant in full extent)
#   tiff(filename = paste0(out_folder,subfolder,names(pred.obs[[1]]),"_x_",names(pred.obs[[2]]),"_VT_",i ,".tiff"),
#        height = 20, width = 40, units = 'cm', res=100)
#     par(mfrow=c(3,1))
#   plot(pred.obs.veg[[1]], main="Prediction")
#   plot(pred.obs.veg[[2]], main="Observation")
#   plot(pred.obs.veg.out, main="kappa")
#   text(pred.obs.veg.out)
#   dev.off()
# }
# 
# # save table tab_kappa
# write.csv(tab_kappa, file= paste0(out_folder,subfolder,names(pred.obs[[1]]),"_x_",names(pred.obs[[2]]),"tab_kappa.csv"))
# 
# 
# par(mfrow=c(3,1))
# plot(test1[[1]])
# plot(test1[[2]])
# plot(test1.out)
# text(test1.out)
# 
# #check values
# table(values(ar_sub_r))
# table(values(DM_AUC_509))
# # eval polygons
# 
# 
# http://greenbrown.r-forge.r-project.org/man/CompareClassification.html
# https://www.rdocumentation.org/packages/spatialEco/versions/1.2-0/topics/class.comparison

### Eval 3 -Ecosytem groups ####
# test kappa not on VT but on groups of VT (according to BRYN 2018 - there are 12 groups)
#### reclassify 
#load in translation rules
# "GR" also stands for GROUP or ECOSYSTEM GROUP
transl_vt_groups <- read.csv(paste0(git_folder, "transl_VT_VEGgroup.csv"), sep = ";", dec = ".")
transl_GR <- matrix(nrow= 31, ncol = 2)
# make into reclassify matrix # can also to like this cbind(transl_rule_DM$VT_ID, transl_rule_DM$PFT_ID)
for(i in 1:NROW(transl_vt_groups)){
  transl_GR[i,] <- c(transl_vt_groups$VT_ID[i], transl_vt_groups$GRP[i])
  # print(transl)
}
# these are the masked datasets *ar18, ar9 and VEG
#transl <- cbind(transl_rule_RS$RS_code, transl_rule_RS$PFT_ID)
# lapply(ras18, function(x) names(x))
AR18_GR <- lapply(ras18, function(x) {reclassify(x, transl_GR, include.lowest=TRUE, overwrite=TRUE,
                           filename = paste0(out_folder,"EG/","EG_", names(x),".tif"))})
AR9_GR <- lapply(ras9, function(x) {reclassify(x, transl_GR, include.lowest=TRUE, overwrite=TRUE,
                                                 filename = paste0(out_folder,"EG/","EG_",names(x),".tif"))})
ARVEG_GR <- lapply(rasVEG, function(x) {reclassify(x, transl_GR, include.lowest=TRUE, overwrite=TRUE,
                                               filename = paste0(out_folder,"EG/","EG_",names(x),".tif"))})
# for later excercise, reclassify also full rasters, not just masked

# reclassify also the full datasets. 
DM_vt_fullextent <- list(DM_maxval, DM_AUC, DM_preval)
DM_eg_fullextent <- lapply(DM_vt_fullextent, function(x) {reclassify(x, transl_GR, include.lowest=TRUE, overwrite=TRUE,
                                                 filename = paste0(out_folder,"EG/","EG_full",names(x),".tif"))})

# Load in specific rasters
library(diffeR)
AR18_ref       <- AR18_GR[[1]]
AR18_DM_maxval <- AR18_GR[[2]]
AR18_DM_AUC    <- AR18_GR[[3]]
AR18_DM_preval <- AR18_GR[[4]]
# create confusion matrices
EG_c.mat_AR18_maxval <- crosstabm(AR18_ref,AR18_DM_maxval)
EG_c.mat_AR18_AUC <- crosstabm(AR18_ref,AR18_DM_AUC)
EG_c.mat_AR18_preval <- crosstabm(AR18_ref,AR18_DM_preval)

# calculate kappa
EG_kappa <- data.frame()
EG_kappa[1,1] <- kappa.own(EG_c.mat_AR18_maxval)
EG_kappa[2,1] <- sum(EG_c.mat_AR18_maxval) # total observations
EG_kappa[3,1] <- sum(diag(EG_c.mat_AR18_maxval)) # total correct observations
EG_kappa[4,1] <- sum(diag(EG_c.mat_AR18_maxval))/sum(EG_c.mat_AR18_maxval)*100 #Total correct observations in %

EG_kappa[1,2] <- kappa.own(EG_c.mat_AR18_AUC)
EG_kappa[2,2] <- sum(EG_c.mat_AR18_AUC) # total observations
EG_kappa[3,2] <- sum(diag(EG_c.mat_AR18_AUC)) # total correct observations
EG_kappa[4,2] <- sum(diag(EG_c.mat_AR18_AUC))/sum(EG_c.mat_AR18_AUC)*100 #Total correct observations in %

EG_kappa[1,3] <- kappa.own(EG_c.mat_AR18_preval)
EG_kappa[2,3] <- sum(EG_c.mat_AR18_preval) # total observations
EG_kappa[3,3] <- sum(diag(EG_c.mat_AR18_preval)) # total correct observations
EG_kappa[4,3] <- sum(diag(EG_c.mat_AR18_preval))/sum(EG_c.mat_AR18_preval)*100 #Total correct observations in %

# AR9
AR9_ref       <- AR9_GR[[1]]
AR9_DM_maxval <- AR9_GR[[2]]
AR9_DM_AUC    <- AR9_GR[[3]]
AR9_DM_preval <- AR9_GR[[4]]
# create confusion matrices
EG_c.mat_AR9_maxval <- crosstabm(AR9_ref,AR9_DM_maxval)
EG_c.mat_AR9_AUC    <- crosstabm(AR9_ref,AR9_DM_AUC)
EG_c.mat_AR9_preval <- crosstabm(AR9_ref,AR9_DM_preval)

# calculate kappa
EG_kappa[5,1] <- kappa.own(EG_c.mat_AR9_maxval)
EG_kappa[6,1] <- sum(EG_c.mat_AR9_maxval) # total observations
EG_kappa[7,1] <- sum(diag(EG_c.mat_AR9_maxval)) # total correct observations
EG_kappa[8,1] <- sum(diag(EG_c.mat_AR9_maxval))/sum(EG_c.mat_AR9_maxval)*100 #Total correct observations in %

EG_kappa[5,2] <- kappa.own(EG_c.mat_AR9_AUC)
EG_kappa[6,2] <- sum(EG_c.mat_AR9_AUC) # total observations
EG_kappa[7,2] <- sum(diag(EG_c.mat_AR9_AUC)) # total correct observations
EG_kappa[8,2] <- sum(diag(EG_c.mat_AR9_AUC))/sum(EG_c.mat_AR9_AUC)*100 #Total correct observations in %

EG_kappa[5,3] <- kappa.own(EG_c.mat_AR9_preval)
EG_kappa[6,3] <- sum(EG_c.mat_AR9_preval) # total observations
EG_kappa[7,3] <- sum(diag(EG_c.mat_AR9_preval)) # total correct observations
EG_kappa[8,3] <- sum(diag(EG_c.mat_AR9_preval))/sum(EG_c.mat_AR9_preval)*100 #Total correct observations in %

# VEG
VEG_ref       <- ARVEG_GR[[1]]
VEG_DM_maxval <- ARVEG_GR[[2]]
VEG_DM_AUC    <- ARVEG_GR[[3]]
VEG_DM_preval <- ARVEG_GR[[4]]

# create confusion matrices
EG_c.mat_VEG_maxval <- crosstabm(VEG_ref,VEG_DM_maxval)
EG_c.mat_VEG_AUC <- crosstabm(VEG_ref,VEG_DM_AUC)
EG_c.mat_VEG_preval <- crosstabm(VEG_ref,VEG_DM_preval)

# calculate kappa
EG_kappa[9, 1] <- kappa.own(EG_c.mat_VEG_maxval)
EG_kappa[10,1] <- sum(EG_c.mat_VEG_maxval) # total observations
EG_kappa[11,1] <- sum(diag(EG_c.mat_VEG_maxval)) # total correct observations
EG_kappa[12,1] <- sum(diag(EG_c.mat_VEG_maxval))/sum(EG_c.mat_VEG_maxval)*100 #Total correct observations in %

EG_kappa[9, 2] <- kappa.own(EG_c.mat_VEG_AUC)
EG_kappa[10,2] <- sum(EG_c.mat_VEG_AUC) # total observations
EG_kappa[11,2] <- sum(diag(EG_c.mat_VEG_AUC)) # total correct observations
EG_kappa[12,2] <- sum(diag(EG_c.mat_VEG_AUC))/sum(EG_c.mat_VEG_AUC)*100 #Total correct observations in %

EG_kappa[9, 3] <- kappa.own(EG_c.mat_VEG_preval)
EG_kappa[10,3] <- sum(EG_c.mat_VEG_preval) # total observations
EG_kappa[11,3] <- sum(diag(EG_c.mat_VEG_preval)) # total correct observations
EG_kappa[12,3] <- sum(diag(EG_c.mat_VEG_preval))/sum(EG_c.mat_VEG_preval)*100 #Total correct observations in %

names(EG_kappa) <- c("DM_maxval", "DM_AUC", "DM_preval")
rownames(EG_kappa) <- c("kappa_AR18",
                         "tot.obs_AR18",
                         "tot.obs.corr_AR18",
                         "perc.obs.corr_AR18",
                         "kappa_AR9", 
                         "tot.obs_AR9", 
                         "tot.obs.corr_AR9", 
                         "perc.obs.corr_AR9",
                         "kappa_VEG", 
                         "tot.obs_VEG",
                         "tot.obs.corr_VEG",
                         "perc.obs.corr_VEG"
)
round(EG_kappa, digits = 2)
EG_kappa_ord <- EG_kappa[order(EG_kappa$DM_maxval),]
write.csv(EG_kappa, file= paste0(out_folder,"EG_kappa.csv"))
write.csv(EG_kappa_ord, file= paste0(out_folder,"EG_kappa_ord.csv"))

#AR18
write.csv(EG_c.mat_AR18_maxval, file= paste0(out_folder,"EG_c.mat_AR18_maxval.csv"))
write.csv(EG_c.mat_AR18_AUC, file= paste0(out_folder,"EG_c.mat_AR18_AUC.csv"))
write.csv(EG_c.mat_AR18_preval, file= paste0(out_folder,"EG_c.mat_AR18_preval.csv"))

#AR9
write.csv(EG_c.mat_AR9_maxval, file= paste0(out_folder,"EG_c.mat_AR9_maxval.csv"))
write.csv(EG_c.mat_AR9_AUC, file= paste0(out_folder,"EG_c.mat_AR9_AUC.csv"))
write.csv(EG_c.mat_AR9_preval, file= paste0(out_folder,"EG_c.mat_AR9_preval.csv"))

#VEG
write.csv(EG_c.mat_VEG_maxval, file= paste0(out_folder,"EG_c.mat_VEG_maxval.csv"))
write.csv(EG_c.mat_VEG_AUC, file= paste0(out_folder,"EG_c.mat_VEG_AUC.csv"))
write.csv(EG_c.mat_VEG_preval, file= paste0(out_folder,"EG_c.mat_VEG_preval.csv"))



#calculate kappa and individual kappa using the Rossiter functions ####
# EG - individual kappa ####
# *AR18 ####
c.mat.AR18.eg <- list(EG_c.mat_AR18_maxval, EG_c.mat_AR18_AUC, EG_c.mat_AR18_preval)
c.mat.AR18.eg.kappa <- lapply(c.mat.AR18.eg, kappa)
#subset 1:5
AR18.eg.tocsv.tot <- sapply(c.mat.AR18.eg.kappa,"[",c(1:5))
colnames(AR18.eg.tocsv.tot) <- c("AR18_DM_maxval", "AR18_DM_AUC", "AR18_DM_preval") 
#write.csv(AR18.eg.tocsv.tot, file= paste0(out_folder,"EG_kappa_total_AR18.csv"))
#subset 6:11
AR18.eg.tocsv <- t(as.data.frame(lapply(c.mat.AR18.eg.kappa, function(x) {x[c(6:8,9)]})))
row.names(AR18.eg.tocsv) <- paste0(row.names(AR18.eg.tocsv), c(rep(c("_AR18_DM_maxval", "_AR18_DM_AUC", "_AR18_DM_preval"), each=4))) 
rownames(AR18.eg.tocsv)
#output to csv
write.csv(AR18.eg.tocsv, file= paste0(out_folder,"EG_kappa_individ_AR18.csv"))

# *AR9 ####
c.mat.AR9.eg <- list(EG_c.mat_AR9_maxval, EG_c.mat_AR9_AUC, EG_c.mat_AR9_preval)
c.mat.AR9.eg.kappa <- lapply(c.mat.AR9.eg, kappa)
#subset 1:5
AR9.eg.tocsv.tot <- sapply(c.mat.AR9.eg.kappa,"[",c(1:5))
colnames(AR9.eg.tocsv.tot) <- c("AR9_DM_maxval", "AR9_DM_AUC", "AR9_DM_preval") 
#write.csv(AR9.eg.tocsv.tot, file= paste0(out_folder,"EG_kappa_total_AR9.csv"))
#subset 6:11
#sapply(c.mat.AR9.eg.kappa,"[",c(6:11))
AR9.eg.tocsv <- t(as.data.frame(lapply(c.mat.AR9.eg.kappa, function(x) {x[c(6:8,9)]})))
row.names(AR9.eg.tocsv) <- paste0(row.names(AR9.eg.tocsv), c(rep(c("_AR9_DM_maxval", "_AR9_DM_AUC", "_AR9_DM_preval"), each=4))) 
rownames(AR9.eg.tocsv)
#output to csv
write.csv(AR9.eg.tocsv, file= paste0(out_folder,"EG_kappa_individ_AR9.csv"))

# *VEG ####
c.mat.VEG.eg <- list(EG_c.mat_VEG_maxval, EG_c.mat_VEG_AUC, EG_c.mat_VEG_preval)
c.mat.VEG.eg.kappa <- lapply(c.mat.VEG.eg, kappa)
#subset 1:5
VEG.eg.tocsv.tot <- sapply(c.mat.VEG.eg.kappa,"[",c(1:5))
colnames(VEG.eg.tocsv.tot) <- c("VEG_DM_maxval", "VEG_DM_AUC", "VEG_DM_preval") 
#write.csv(VEG.eg.tocsv.tot, file= paste0(out_folder,"EG_kappa_total_VEG.csv"))
#subset 6:11
sapply(c.mat.VEG.eg.kappa,"[",c(6:11))
VEG.eg.tocsv <- t(as.data.frame(lapply(c.mat.VEG.eg.kappa, function(x) {x[c(6:8,9)]})))
row.names(VEG.eg.tocsv) <- paste0(row.names(VEG.eg.tocsv), c(rep(c("_VEG_DM_maxval", "_VEG_DM_AUC", "_VEG_DM_preval"), each=4))) 
rownames(VEG.eg.tocsv)
#output to csv
write.csv(VEG.eg.tocsv, file= paste0(out_folder,"EG_kappa_individ_VEG.csv"))

# COMBINE VTs into one dataframe
EG_kappa_all <- cbind(AR18.eg.tocsv.tot, AR9.eg.tocsv.tot, VEG.eg.tocsv.tot)
EG_kappa_all[c(seq(5,45, by=5))] <- as.numeric(EG_kappa_all[c(seq(5,45, by=5))]) # correcting the extra information in the list
write.csv(EG_kappa_all, file= paste0(out_folder,"EG_kappa_all.csv"))


# OVERLAY - EG ####
# read in DM *preliminary results from 3 DM attempts
#A method
DM_AUC <- raster(paste0(out_folder,"/EG/EG_fullDM_AUC_fill.tif"))
#B method
DM_preval <- raster(paste0(out_folder,"/EG/EG_fullDM_preval_fill.tif"))
#C method
DM_maxval <- raster(paste0(out_folder,"/EG/EG_fullDM_maxval.tif"))

rAB <- overlay(DM_AUC,DM_preval, fun=function(x, y){(x==y)} )
rAC <- overlay(DM_AUC,DM_maxval, fun=function(x, y){(x==y)} )
rBC <- overlay(DM_preval, DM_maxval, fun=function(x, y){(x==y)} )
rABC <- overlay(DM_AUC,DM_preval,DM_maxval, fun=function(x, y, z){(x==y)*(x==z)*(y==z)} )
par(mfrow=c(2,2))
plot(rAB, main="AUC vs Preval")
plot(rAC, main="AUC vs Maxval")
plot(rBC, main="Preval vs Maxval")
plot(rABC, main="AUC vs Preval vs Maxval")
# create table to output EVALUATION data into 
nrows <- length(32)
comp_ABC <- data.frame(AB_TRUE=integer(nrows), AB_FALSE=integer(nrows), AC_TRUE=integer(nrows),
                       AC_FALSE=integer(nrows),BC_TRUE=integer(nrows), BC_FALSE=integer(nrows), 
                       ABC_TRUE =integer(nrows), ABC_FALSE=integer(nrows))

comp_ABC[1,2:1] <- t(table(values(rAB)))
comp_ABC[1,4:3] <- t(table(values(rAC)))
comp_ABC[1,6:5] <- t(table(values(rBC)))
comp_ABC[1,8:7] <- t(table(values(rABC)))

#percentage agreement vs total
comp_ABC[2,2:1]<-comp_ABC[1,1]/(comp_ABC[1,1]+comp_ABC[1,2])
comp_ABC[2,4:3]<-comp_ABC[1,3]/(comp_ABC[1,3]+comp_ABC[1,4])
comp_ABC[2,6:5]<-comp_ABC[1,5]/(comp_ABC[1,5]+comp_ABC[1,6])
comp_ABC[2,8:7]<-comp_ABC[1,7]/(comp_ABC[1,7]+comp_ABC[1,8])

write.csv(comp_ABC, paste0(out_folder, "EG_overlay_3methods.csv"))

# tab <- data.frame(ncol(4), nrow(3))
# tab <- data.frame(AB=integer(3), AC=integer(nrows), BC=integer(nrows), ABC =integer(nrows))
# tab[2:1,1] <- t(table(values(rAB)))
# tab[2:1,2] <- t(table(values(rAC)))
# tab[2:1,3] <- t(table(values(rBC)))
# tab[2:1,4] <- table(values(rABC))

# SAVE OUTPUTS
writeRaster(rAB, filename = paste0(out_folder,"EG_compare_AUC_preval.tiff"),format = "GTiff")
writeRaster(rAC, filename = paste0(out_folder,"EG_compare_AUC_maxval.tiff"),format = "GTiff")
writeRaster(rBC, filename = paste0(out_folder,"EG_compare_preval_maxval.tiff"),format = "GTiff")
writeRaster(rABC, filename = paste0(out_folder,"EG_compare_all3.tiff"),format = "GTiff")


#### maps of ECOSYSTEM GROUPS for 3 methods ####
#reclassify VT based on groups of VT (according to BRYN 2018 - there are 12 groups)

#load in translation rules
transl_vt_groups <- read.csv(paste0(git_folder, "transl_VT_VEGgroup.csv"), sep = ";", dec = ".")
#write.csv(transl_vt_groups,paste0(git_folder, "transl_VT_VEGgroup2.csv"))
transl_GR <- matrix(nrow= 31, ncol = 2)
# make into reclassify matrix # can also to like this cbind(transl_rule_DM$VT_ID, transl_rule_DM$PFT_ID)
for(i in 1:NROW(transl_vt_groups)){
  transl_GR[i,] <- c(transl_vt_groups$VT_ID[i], transl_vt_groups$GRP[i])
  # print(transl)
}
DM_VT_3methods_list <- list(DM_maxval,DM_preval, DM_AUC)
DM_GRP_3methods_list <- lapply(DM_VT_3methods_list, function(x) {reclassify(x, transl_GR, include.lowest=TRUE, overwrite=TRUE,
                                      filename = paste0(out_folder,"VT_groups/",names(x),"GRP_NORGE.tif"))})

#**********************************************#
#### aggregate areas of VT to Ecosystem ####
ET <- read.csv(paste0(git_folder, "VT_3methods_freq.csv"),  sep = ";", dec = ".")
#columns that consist of area percentages are aggregated across groups, producing summed percentage
ET_area <- aggregate(ET[,c(4:11)], by=list(GRP=ET$GRP), FUN=sum)
#save
write.csv(ET_area, file= paste0(git_folder,"EG_3methods_freq.csv"))






######### functions for KAPPA ####
# from http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.77.247&rep=rep1&type=pdf
# REF: Rossiter, D.: Technical Note: Statistical methods for accuracy assessment of classified thematic maps. Department of Earth Systems Analysis, International Institute for Geo-information Science & Earth Observation (ITC): Enschede, The Netherlands, 2004.

kappa <- function(CM) {
  #convert both data frames and vectors to matrices
  cmx<-as.matrix(CM)
  #try to convert a vector to a square matrix
  if (ncol(cmx) == 1)
    cmx<-matrix(cmx, byrow=TRUE, nrow=sqrt(nrow(cmx)))
  nr<-nrow(cmx); nc<-ncol(cmx)
  if (nr != nc)
  { print("Error: matrix is not square"); break }
  n<-sum(cmx)
  d<-diag(cmx); dsum<-sum(d); th1<-dsum/n
  th1v<-((th1*(1-th1))/n)
  csum<-apply(cmx,2,sum); rsum<-apply(cmx,1,sum)
  ua<-d/rsum; pa<-d/csum
  th2 <- sum(rsum*csum) / n^2; kh <- (th1-th2)/(1-th2)
  th3 <- sum( (csum + rsum) * d ) / n^2;
  th4 <- 0; for (i in 1:nr) for (j in 1:nc)
    th4 <- th4 + (cmx[i,j] * ((csum[i] + rsum[j])^2));
  th4 <- th4 / n^3;
  th1c <- 1 - th1; th2c <- 1 - th2;
  khv <- 1/n *
    ( ( ( th1 * th1c ) / th2c^2 )
      + ( ( 2 * th1c * ((2*th1*th2) - th3) ) / th2c^3 )
      + ( ( th1c^2 * ( th4 - (4 * th2^2 ) ) ) / th2c^4 )
    )
  #per-class kappa, user’s accuracy...
  p <- cmx/n; uap <- apply(p,1,sum); pap <- apply(p,2,sum); dp<-diag(p);
  kpu <- (dp/uap - pap)/(1 - pap);
  #...and its variance
  t1 <- uap-dp; t2 <- (pap*uap)-dp; t3 <- dp*(1 - uap - pap + dp);
  kpuv <- ( (t1/(uap^3 * (1-pap)^3)) * ((t1*t2) + t3) )/n;
  #per-class kappa, producer’s reliability...
  kpp <- (dp/pap - uap)/(1 - uap);
  #...and its variance
  t1 <- (pap-dp);
  kppv <- ( (t1/(pap^3 * (1-uap)^3)) * ((t1*t2) + t3) )/n;
  #return all statistics as a list
  list(sum.n=n, sum.naive=th1, sum.var=th1v, sum.kappa=kh, sum.kvar=khv,
       user.naive=ua, prod.naive=pa,
       user.kappa=kpu, user.kvar=kpuv, prod.kappa=kpp, prod.kvar=kppv)
}

summary.kappa <- function(kappa, alpha=0.05) {
  ciw<-function(var, n) {
    qnorm(1-(alpha/2))*sqrt(var) + (1/(2*n))
  }
  print(paste("Number of observations:", kappa$sum.n), quote=F)
  print("Summary of naive statistics", quote=F)
  print(paste(
    "Overall accuracy, stdev, CV%:",
    round(kappa$sum.naive, 4), ",",
    round(sqrt(kappa$sum.var), 4), ",",
    round((sqrt(kappa$sum.var)/kappa$sum.naive)*1000,0)/10),
    quote=F)
  w<-ciw(kappa$sum.var, kappa$sum.n)
  print(paste(
    round((1-alpha)*100,0),"% confidence limits for accuracy:",
    round((kappa$sum.naive-w),4),"...",
    round((kappa$sum.naive+w),4)), quote=F, sep="")
  print("User’s accuracy", quote=F); print(round(kappa$user.naive,4));
  print("Producer’s reliability:", quote=F); print(round(kappa$prod.naive,4));
  print("Summary of kappa statistics", quote=F)
  print(paste("Overall kappa, stdev, & CV%:",
              round(kappa$sum.kappa,4), ",",
              round(sqrt(kappa$sum.kvar),4), ",",
              round((sqrt(kappa$sum.kvar)/kappa$sum.kappa)*1000,0)/10), quote=F)
  w<-ciw(kappa$sum.kvar, kappa$sum.n)
  print(paste(
    round((1-alpha)*100,0),"% confidence limits for kappa:",
    round((kappa$sum.kappa-w),4),"...",
    round((kappa$sum.kappa+w),4)), quote=F, sep="")
  print("Per-class kappa, stdev, & CV%, for user’s accuracy:", quote=F)
  print(round(kappa$user.kappa,4), quote=F);
  print(round(sqrt(kappa$user.kvar),4), quote=F);
  print(round((sqrt(kappa$user.kvar)/kappa$user.kappa)*1000,0)/10, quote=F);
  print("Per-class kappa, stdev, & CV%, for producer’s reliability:", quote=F)
  print(round(kappa$prod.kappa,4), quote=F);
  print(round(sqrt(kappa$prod.kvar),4), quote=F);
  print(round((sqrt(kappa$prod.kvar)/kappa$prod.kappa)*1000,0)/10, quote=F);
}

tau <- function(CM, P) {
  #convert both data frames and vectors to matrices
  cmx<-as.matrix(CM)
  #try to convert a vector to a square matrix
  if (ncol(cmx) == 1)
    cmx<-matrix(cmx, byrow=TRUE, nrow=sqrt(nrow(cmx)))
  nr<-nrow(cmx); nc<-ncol(cmx)
  if (nr != nc)
  { print("Error: matrix is not square"); break }
  #check P and create if necessary
  if (missing(P))
    P<-rep(1/nr, nr)
  if (length(P) != nc)
  { print("Error: prior probabilities vector has wrong length"); break }
  if (abs(1-sum(P)) > 0.0001)
  { print("Error: prior probabilities must sum to 1"); break }
  n<-sum(cmx)
  d<-diag(cmx); dsum<-sum(d); th1<-dsum/n
  csum<-apply(cmx,2,sum); th2<-(csum%*%P)/n
  tau<-(th1-th2)/(1-th2);
  th3<-sum( (csum + (P*n)) * diag(cmx) ) / n^2;
  rsum<-apply(cmx,1,sum)
  ua<-d/rsum; pa<-d/csum
  th4 <- 0; for (i in 1:nr) for (j in 1:nc)
    th4 <- th4 + (cmx[i,j] * ((csum[i] + P[j]*n)^2));
  th4 <- th4 / n^3;
  th1c <- 1 - th1; th2c <- 1 - th2;
  tv <- 1/n *
    ( ( ( th1 * th1c ) / th2c^2 )
      + ( ( 2 * th1c * ((2*th1*th2) - th3) ) / th2c^3 )
      + ( ( th1c^2 * ( th4 - (4 * th2^2 ) ) ) / th2c^4 )
    )
  list(prior=P, obs=rsum, ref=csum, n=n, tau=tau, tvar=tv, coeff=c(th1, th2, th3, th4))
}

summary.tau <- function(tau, alpha=0.05) {
  ciw<-function(var, n) {
    qnorm(1-(alpha/2))*sqrt(var) + (1/(2*n))
  }
  print(paste("Number of observations:", tau$n), quote=F)
  print("Prior class probabilities:", quote=F)
  print(tau$prior, quote=F)
  print("Observed class proportions:", quote=F)
  print(round(tau$obs/tau$n,4), quote=F)
  print("Reference class proportions:", quote=F)
  print(round(tau$ref/tau$n,4), quote=F)
  print(paste("Tau, stdev, & CV%:",
              round(tau$tau,4), ",",
              round(sqrt(tau$tvar),4), ",",
              round((sqrt(tau$tvar)/tau$tau)*1000,0)/10), quote=F)
  w<-ciw(tau$tvar, tau$n)
  print(paste(round((1-alpha)*100,0),"% confidence limits for tau:",
              round((tau$tau-w),4), "...", round((tau$tau+w),4), sep=""), quote=F)
}




#Test swapping crosstabm
AR18_DM_maxval<-raster("E:/Project_1.5/OUTPUT_agri/EG/EG_AR18_DM_maxval.tif")
AR18_ref<-raster("E:/Project_1.5/OUTPUT_agri/EG/EG_AR18_ref.tif")
ct.wrong <- crosstabm(AR18_ref,AR18_DM_maxval, percent = TRUE)
ct.correct <- crosstabm(AR18_DM_maxval, AR18_ref, percent = TRUE)

x<-kappa(ct.wrong); summary.kappa(x)
y<-kappa(ct.correct); summary.kappa(y)
