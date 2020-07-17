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
#Available data (folders): 

#' categorized_models: models categorized using four thresholds as described in the main text
#' Minimum training presence, E= 5%, E= 10%, and E= 20% 
#' environemntal_variables: The set of environmental variables selected in the modeling process
#' shapefiles: Shapefiles to depict results presented 
#' uncertainty_models: Interquartile range of the bootstrap models using the selected parameters

#reading environmental variables (n = 4) 
all_envs = stack (list.files('./environmental_variables', full.names = T))

#reading categorized rasters (n = 3)
cat_models = stack (list.files ('./categorized_models', full.names = T))

#reading uncertainty rasters (n = 3)
uncert_models = stack (list.files ('./uncertainty_models', full.names = T))

#####EXAMPLE 0: CEARA, BRAZIL, FULLY COMMENTED#####

#####CEARA, BRAZIL##### 
#shapefiles from Natural Earth: 
provs = readOGR('./shapefiles2/ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp')

#subset for specific area: 
BR = provs@data[which (provs@data$admin =='Brazil'),] #provinces of a country 
BR$name #Check names of the province, in this case Ceará

cear = subset (provs, name == 'Ceará')
cear_buf = buffer (cear, width = 0.2, dissolve = T) 
cear_buf2 = buffer (cear, width = 0.5, dissolve = T) 

#' we create a buffer to assure that all pixels from the environmnetal rasters,  
#' categorized models and uncertainty maps are captured

#plot (cear)
#plot (cear_buf)

#Crop and mask environmental variables
cear_envs = crop (all_envs, cear_buf)
cear_envs = mask (cear_envs, cear_buf)
cear_envs2 = crop (all_envs, cear_buf2)
cear_envs2 = mask (cear_envs2, cear_buf2)
#plot (cear_envs[[1]])

#Crop and mask categorized rasters: 
cear_mods = crop (cat_models, cear_buf)
cear_mods= mask (cear_mods, cear_buf)
#plot (cear_mods[[3]]) 

#' cear_mods contains 3 raster files corresponding to categorized models 
#' using the no extrapolation (NE), extrapolation (E), and extrapolation and 
#' clamping (EC) maxent functions to transfer models to a desired area. 
#' We are selecting the third raster which corresponds to NE. 

#Crop and mask uncertainty maps: 
cear_unc = crop (uncert_models, cear_buf)
cear_unc = mask (cear_unc, cear_buf)
#plot(cear_unc[[3]])

#' Uncertainty plots are available for all the transfered rasters including 
#' NE, E, and EC. We are selecting the uncertainty for NE which is the raster
#' we are using for this excercise. 

#Using functions: 

#Function 1: e_space(). Checking environments available: 
#' Arguments for this function include: 
#' - stck = Stack of environmental variables. 
col1 <- c('#fde0dd', '#c51b8a')
f1_cear = e_space(stck = cear_envs,pflag = T,col.use = col1)

#' G-space is showing only partial borders since wrld_simpl lacks information 
#' for provinces and only has information at the country level. 

#Function 2: e_space_categorical(). Creating dataframe for environemntal sampling: 
#' Arguments for this function include: 
#' - stck = Stack of environmental variables. 
#' - ctgr = Stack of categorized models or individual rasters. 
col2 <- c('#AF8DC3', '#7FBF7B')
f2_cear = e_space_cat(stck = cear_envs, ctgr = cear_mods[[3]], pflag = T, col.use = col2)

#' The object created is the dataframe that will be used for environmental sampling. 
#' We are using the raster corresponding to the not-extrapolation result (NE), 
#' therefore, just the third object from the raster stack cear_mods

#Function 3: e_space_cat_back(). Suitability vs background environemnts: 
#' Arguments for this function include: 
#' - stck = Stack of environmental variables. 
#' - ctgr = Stack of categorized models or individual rasters. 
#' - bck = Stack of environemntal variables to use as background. 
col3 <- c('#AF8DC3', '#7FBF7B')
f3_cear = e_space_cat_back(stck = cear_envs, ctgr = cear_mods[[3]], bck = cear_envs2, pflag = T, col.use = col2)

#' This function allows to check the presence of regions not used by the model 
#' but that are present in the studied area (grey points). Because the majority of Ceará presents 
#' a suitable category, grey areas are shown only for Humidity and Soil comparisons.
#' The dataframe produced here should not be used for the other functions since has 
#' an extra category corresponding to background does not corresponds to suitable 
#' environments as determined by the model. 

#Function 4: hutchinson_e_g(). Sampling from E-space, maximizing suitability classes
#' Arguments for this function include: 
#' - data = Data frame obtained with e_space_categorical function.
#' - calls = Columns with environmental values that should be used for the plots.
#' - pgly = Polygon to depict G-Space.
#' - ntr = Number of selections that should be done in the plot.

names (f2_cear) #select the columns for the calls argument, use the environemtnal values
f4_cear_tmp_hum = hutchinson_e_g(f2_cear, c(6, 4), cear, 3)
f4_cear_tmp_soil = hutchinson_e_g(f2_cear, c(6, 5), cear, 3)
f4_cear_hum_soil = hutchinson_e_g(f2_cear, c(4, 5), cear, 3)

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