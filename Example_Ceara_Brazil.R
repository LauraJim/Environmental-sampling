# WORKED EXAMPLE

# Read R code ----------------
# These three scripts contain the functions needed for the analyses
source("E-space-functions.R")
source("Hutchinson-functions.R")
source("Post-track-function.R")

# Load packages -------------
library (gatepoints)
library (raster)
library (rgdal)
library (maptools)
library (plyr)

# Set global parameters
# worldwide shapefile 
data("wrld_simpl", package = "maptools")

# Read data -----------------

#' categorized_models: models categorized using four thresholds as described in the main text
#' Minimum training presence, E= 5%, E= 10%, and E= 20% 
cat_models = stack(list.files('./categorized_models', full.names = T))
# number of layers
length(cat_models@layers)

#' environemntal_variables: The set of environmental variables selected in the modeling process
all_envs = stack(list.files('./environmental_variables', full.names = T))
# number of layers
length(all_envs@layers)

#' uncertainty_models: Interquartile range of the bootstrap models using the selected parameters
uncert_models = stack(list.files ('./uncertainty_models', full.names = T))
# number of layers
length(uncert_models@layers)

# EXAMPLE: Ceara, Brazil ---------------------

# Read shapefiles and select region of interest

# World shapefiles from Natural Earth: 
provs = readOGR('./shapefiles2/ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp')

# Select specific region: CEARA
#   Provinces of Brazil
BR = provs@data[which(provs@data$admin =='Brazil'),] 
#   Look for the specifi province
BR$name
#  Select specific province by subsetting the data frame
cear = subset (provs, name == 'Ceará')
# Create a buffer around the region of interest
# this is to guarantee that all pixels in the raster layers are captured
cear_buf = buffer(cear, width = 0.2, dissolve = T)
# you can used different buffer sizes
cear_buf2 = buffer(cear, width = 0.5, dissolve = T) 
# Plot regions to verify our selection
x11()
plot(cear)
plot(cear_buf,add=T)
plot(cear_buf2,add=T)

# Crop and mask rasters -----------------

# Environmental variables
cear_envs = crop(all_envs, cear_buf)
cear_envs = mask(cear_envs, cear_buf)
# plot one of the layers to verify that the process was done correctly
plot (cear_envs[[1]])

# Categorized rasters
# Note: cear_mods contains 3 raster files corresponding to categorized models 
# using the no extrapolation (NE), extrapolation (E), and extrapolation and 
# clamping (EC) features from Maxent to transfer models to a given area. 
# We will work with the third raster which corresponds to NE. 
cear_mods = crop(cat_models, cear_buf)
cear_mods= mask(cear_mods, cear_buf)
# plot one of the layers to verify that the process was done correctly
plot (cear_mods[[3]]) 

# Uncertainty maps
# The uncertainty plots are available for all the transfered rasters including 
# NE, E, and EC. We will use the NE layer throughout this example. 
cear_unc = crop(uncert_models, cear_buf)
cear_unc = mask(cear_unc, cear_buf)
# plot one of the layers to verify that the process was done correctly
plot(cear_unc[[3]])

# Using functions -------------------------------------

# Select color ramp to be used in the visualizations
col <- c("blueviolet", "springgreen3")

#Function 1: e_space(
# Displaying the environmental variables in E-space
# and the region covered by the layers in G-space
f1_cear = e_space(stck = cear_envs,pflag = T)

#Function 2: e_space_cat()
# Displaying the environmental variables in E-space using different colors for different
# suitability categories and the sites that cover the region of interest also labeled
# according to its suitability category
f2_cear = e_space_cat(stck = cear_envs, ctgr = cear_mods[[3]], pflag = T, col.use = col)

#Function 3: e_space_cat_back()
# The output plots are similar to the ones obtained with the function e_space_cat()
# except that in this case there is a new category of points called Background
# Background points are inside the region of interest but they have no suitability
# value assigned by the model
f3_cear = e_space_cat_back(stck = cear_envs, ctgr = cear_mods[[3]],
                           bck = cear_envs2, pflag = T, col.use = col)

#Function 4: hutchinson_e_g(). Sampling from E-space, maximizing suitability classes
#' Arguments for this function include: 
#' - data = Data frame obtained with e_space_categorical function.
#' - calls = Columns with environmental values that should be used for the plots.
#' - pgly = Polygon to depict G-Space.
#' - ntr = Number of selections that should be done in the plot.

names (f2_cear) #select the columns for the calls argument, use the environemtnal values
#f4_cear_tmp_hum = hutchinson_e_g(f2_cear, c(6, 4), cear, 3)
#f4_cear_tmp_soil = hutchinson_e_g(f2_cear, c(6, 5), cear, 3)
#f4_cear_hum_soil = hutchinson_e_g(f2_cear, c(4, 5), cear, 3)
cear_tmp_hum = hutchinson_e_g(data=f2_cear, calls=c(6, 4), cear, 3,col1)

#' Here the excercise of sampling has the idea to maximize different suitability 
#' classess in the environmental dimensions selected, considering pair-wise associations.
#' Because principal component 1 recovers the majority of the variability in the environmental  
#' sets used, we sample in three plots considering PC1 for temperature and PC1 for humidity 
#' PC1 for temperature and PC1 for soils and PC1 for humidity and PC1 from soils. 

#combining the dataframes: 
cear_sampling = rbind (f4_cear_tmp_hum, f4_cear_tmp_soil, f4_cear_hum_soil)

#' By combining all the dataframes we have all the pixels considering the different 
#' tracks selected in three different environmental scenarios. 

#Function 6: post_track(). Checking uncertainty in selected environmental tracks: 
#' Arguments in this function include: 
#' - data_track = Dataframe obtained using function 4 or 5 with different track categories. 
#' - uncert_ras = Raster containing measures of uncertainty for the model assessed. 
#' - pgly = Shape file to depict G-space.

f6_cear = post_track(cear_sampling, cear_unc[[3]], cear)

#' Because different environmental tracks were selected using different environemtnal 
#' values, some information might be repeated, for that, post_track functions eliminates
#' duplicates for producing its final object. 

dim(cear_sampling)
dim(f6_cear)

#Writing final results tables for further manipulation and figure! 
write.csv (f2_cear, './ceara_df1.csv', row.names = F)
write.csv (f6_cear, './ceara_res.csv', row.names = F)


#Function 5: hutchinson_g_e(). Sampling from G-space, maximizing suitability classes: 
#' Arguments in this function include: 
#' - data = Data frame obtained with e_space_categorical function.
#' - calls = Columns with environmental values that should be used for the plots.
#' - pgly = Polygon to depict G-Space.
#' - ntr = Number of selections that should be done in the plot.

names (f2_cear) #select the columns for the calls argument, use the environemtnal values
f5_cear = hutchinson_g_e(f2_cear, c(5, 4), cear, 3)

#' This function has the same ability as function 4 but allows to create transects 
#' in the geographical space and then examined the transects in the desired environments 
#' 

#
# Daniel Romero-Alvarez & Laura Jimenez, 2020