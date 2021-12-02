# DM_wall-to-wall
Aggregating individual DMs into a wall-to-wall map. 

there are five main parts to the code (01-05) where we 
* first mask out redundant areas,
* then we assemble the maps using three methods
* we rasterize reference data and mask out evaluation areas
* we compute confusion matrices for the evaluation
* and last we evaluate using proportional dissimilarity (bray-curtis)

accompanying csv files (transl*.csv) contain the reclassification key for harmonizing the reference data used in step 3 (rasterize)

Excel files contain results in raw form, later imported into the final manuscript

Output raster datasets for the three methods are available through the [ZENODO repository](https://doi.org/10.5281/zenodo.5024741)
* probability-based method
* performance-based method
* prevalence-based method
