# WORKED EXAMPLES
### This script contains a worked example on how to use all the functions created to design
### a survey of potential sites inside a region of interest. The selection is based on the
### suitability values from a Species Distribution Model (SDM) and can be focused in either
### covering all the range of suitability values, if the goal is to improve the estimation
### of the SDM, or covering the regions of high suitability, if the goal is to select sites
### where the likelihood of detecting the species is high.

# Read R code ----------------
# These three scripts contain the functions needed for the analyses
source("E-space-functions.R")
source("Hutchinson-functions.R")
source("Post-track-function.R")

# Load packages -------------
library(gatepoints)
library(raster)
library(rgdal)
library(maptools)
library(plyr)
library(sp)

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

# WORKED EXAMPLE 1, FULLY COMMENTED: Ceara, Brazil ---------------------

# Read shapefiles and select region of interest

# World shapefiles from Natural Earth: 
provs = readOGR('./shapefiles2/ne_10m_admin_1_states_provinces.shp')

# Select specific region: CEARA
#   Provinces of Brazil
BR = provs@data[which(provs@data$admin =='Brazil'),] 
#   Look for the specifi province
BR$name
#  Select specific province by subsetting the data frame
cear = subset (provs, name == 'Ceará')
# Create a buffer around the region of interest. Warnings are related with the planar projection. 
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

# FUNCTION 1: e_space

# Displaying the environmental variables in E-space
# and the region covered by the layers in G-space
f1_cear = e_space(stck = cear_envs,pflag = T)

# FUNCTION 2: e_space_cat

# Displaying the environmental variables in E-space using different colors for different
# suitability categories and the sites that cover the region of interest also labeled
# according to its suitability category
f2_cear = e_space_cat(stck = cear_envs, ctgr = cear_mods[[3]], pflag = T, col.use = col)

# FUNCTION 3: e_space_cat_back
# The output plots are similar to the ones obtained with the function e_space_cat()
# except that in this case there is a new category of points called Background
# Background points are inside the region of interest but they have no suitability
# value assigned by the model
f3_cear = e_space_cat_back(stck = cear_envs, ctgr = cear_mods[[3]],
                           bck = cear_envs, pflag = T, col.use = col)

# FUNCTION 4: hutchinson

# Option 1: from E-space to G-space
# Select the columns that contain the enviromental variables to be plotted
# and use them as the 'calls' argument of this function
names (f2_cear) 
# temperature & humidity
cear_tmp_hum = hutchinson(EtoG=T, data=f2_cear, calls=c(6,4), plyg=cear, ntr=3, col.use=col)
# temperature & soil
cear_tmp_soil = hutchinson(EtoG=T, data=f2_cear, calls=c(6,5), plyg=cear, ntr=3, col.use=col)
# humidity & soil
cear_hum_soil = hutchinson(EtoG=T, data=f2_cear, calls=c(4,5), plyg=cear, ntr=3, col.use=col)

# Option 2: from G-space to E-space
# temperature & humidity
cear2_tmp_hum = hutchinson(EtoG=F, data=f2_cear, calls=c(6,4), plyg=cear, ntr=3, col.use=col)
# temperature & soil
cear2_tmp_soil = hutchinson(EtoG=F, data=f2_cear, calls=c(6,5), plyg=cear, ntr=3, col.use=col)
# humidity & soil
cear2_hum_soil = hutchinson(EtoG=F, data=f2_cear, calls=c(4,5), plyg=cear, ntr=3, col.use=col)

# The sampling exercise has the goal of maximizing the selection of different suitability
# categories with the selection of different transects in either E-space or G-space.
# Therefore, once the transects are selected, a final step is needed to check the levels
# of uncertainty in the selected sites:

# FUNCTION 5: post_track()

# Combine the dataframes if your are willing to include more than two environmental variables
cear_sampling = rbind(cear_tmp_hum, cear_tmp_soil, cear_hum_soil)
dim(cear_sampling)

# Select uncertainty layer and apply function
uncer_check = post_track(cear_sampling, cear_unc[[3]], cear, col.use=col)

#' Because different environmental tracks were selected using different environmental 
#' variables, some information might be repeated, however, the post_track function
#' eliminates duplicates
dim(uncer_check)

# Save resulting tables for further analyses and visualizations
write.csv(f2_cear, './ceara_df1.csv', row.names = F)
write.csv(uncer_check, './ceara_res.csv', row.names = F)

# WORKED EXAMPLE 2: Texas, US ---------------------

# Read shapefiles and select region of interest

# World shapefiles from Natural Earth: 
provs = readOGR('./shapefiles2/ne_10m_admin_1_states_provinces.shp')

# Select specific region: TEXAS
#   STATES OF THE US
US = provs@data[which(provs@data$admin =='United States of America'),] 
#   Look for the specifi province
US$name
#  Select specific province by subsetting the data frame
tex1 = subset (provs, name == 'Texas')

#buffer
tex1_buf = buffer(tex1, width = 0.2, dissolve = T)

#plotting
plot(tex1)
plot(tex1_buf,add=T)


# Preparing data

# Environmental variables
tex1_envs = crop(all_envs, tex1_buf)
tex1_envs = mask(tex1_envs, tex1_buf)
# plot one of the layers to verify that the process was done correctly
plot (tex1_envs[[1]])

# Categorized rasters
tex1_mods = crop(cat_models, tex1_buf)
tex1_mods= mask(tex1_mods, tex1_buf)
# plot one of the layers to verify that the process was done correctly
plot (tex1_mods[[3]]) 

# Uncertainty maps
tex1_unc = crop(uncert_models, tex1_buf)
tex1_unc = mask(tex1_unc, tex1_buf)
# plot one of the layers to verify that the process was done correctly
plot(tex1_unc[[3]])

# Using functions 

# Select color ramp to be used in the visualizations
col <- c("blueviolet", "springgreen3")

# FUNCTION 1: e_space

f1_tex1 = e_space(stck = tex1_envs,pflag = T)

# FUNCTION 2: e_space_cat

f2_tex1 = e_space_cat(stck = tex1_envs, ctgr = tex1_mods[[3]], pflag = T, col.use = col)

# FUNCTION 3: e_space_cat_back

f3_tex1 = e_space_cat_back(stck = tex1_envs, ctgr = tex1_mods[[3]],
                           bck = tex1_envs, pflag = T, col.use = col)

# FUNCTION 4: hutchinson

# Option 1: from E-space to G-space
# Select the columns that contain the enviromental variables to be plotted
# and use them as the 'calls' argument of this function
names (f2_tex1) 
# temperature & humidity
tex1_tmp_hum = hutchinson(EtoG=T, data=f2_tex1, calls=c(6,4), plyg=tex1, ntr=3, col.use=col)
# temperature & soil
tex1_tmp_soil = hutchinson(EtoG=T, data=f2_tex1, calls=c(6,5), plyg=tex1, ntr=3, col.use=col)
# humidity & soil
tex1_hum_soil = hutchinson(EtoG=T, data=f2_tex1, calls=c(4,5), plyg=tex1, ntr=3, col.use=col)

# Option 1: from G-space to E-space
# temperature & humidity
tex12_tmp_hum = hutchinson(EtoG=F, data=f2_tex1, calls=c(6,4), plyg=tex1, ntr=3, col.use=col)
# temperature & soil
tex12_tmp_soil = hutchinson(EtoG=F, data=f2_tex1, calls=c(6,5), plyg=tex1, ntr=3, col.use=col)
# humidity & soil
tex12_hum_soil = hutchinson(EtoG=F, data=f2_tex1, calls=c(4,5), plyg=tex1, ntr=3, col.use=col)

# The sampling exercise has the goal of maximizing the selection of different suitability
# categories with the selection of different transects in either E-space or G-space.
# Therefore, once the transects are selected, a final step is needed to check the levels
# of uncertainty in the selected sites:

# FUNCTION 5: post_track()

# Combine the dataframes if your are willing to include more than two environmental variables
tex1_sampling = rbind(tex1_tmp_hum, tex1_tmp_soil, tex1_hum_soil)
dim(tex1_sampling)

# Select uncertainty layer and apply function
uncer_check = post_track(tex1_sampling, tex1_unc[[3]], tex1, col.use=col)

#' Because different environmental tracks were selected using different environmental 
#' variables, some information might be repeated, however, the post_track function
#' eliminates duplicates
dim(uncer_check)

# Save resulting tables for further analyses and visualizations
write.csv(f2_tex1, './tex1_df1.csv', row.names = F)
write.csv(uncer_check, './tex1_res.csv', row.names = F)


#
# Daniel Romero-Alvarez & Laura Jimenez, 2020