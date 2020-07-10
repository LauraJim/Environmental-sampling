# Functions 'e_space', 'e_space_cat' and 'e_space_cat_back' ----------
### These three functions ...
### ...
#
#' @param stck raster stack containing the environmental variables of interest
#'   cropped to the study area, it can also work as the background in function
#'   e_space_cat_back.
#' @param ctgr categorized raster, it could be a binary or multiple-threshold map
#'   that assigns ascending values of suitability to each pixel.
#' @param bck raster stack with environmental variables clipped to the background
#'   area.
#' @param pflag logic, indicating if the results should be plotted.
#' @param col.use vector of lenght two with the colors to be used in plots
#' @param wrld_crs coordinate reference system to be used in the plots.
#' The default is using the CRS of the data wrld_simpl.
#' 
#' @return
#' \code{e_space} returns a dataframe with the extracted values that can be used
#' for other kinds of visualizations.
#' \code{e_space_cat} returns a dataframe necessary for applying the environmental
#' sampling functions.
#' \code{e_space_cat_back} returns a dataframe that includes the background
#' category added as a new category in the examination. 
#' 
#' @describeIn e_space transforms a raster stack of environmental variables into
#' a dataframe that contains the geographic coordinates and environmental values
#' as extracted from the raster file; it also displays the points into E-space and
#' G-space.
# CODE e_space ---------
# Dependencies: maptools, wrld_simpl, raster 
e_space = function(stck,pflag=F,col.use=NULL,wrld_crs=crs(wrld_simpl)){
  # transform raster into referenced points and into a data frame
  pts1 = data.frame(rasterToPoints(stck, fun = NULL))
  # Plotting if TRUE
  if(pflag){
    # transform values into spatial points to plot on map
    pts_sp = SpatialPointsDataFrame(pts1[,1:2], pts1, proj4string = wrld_crs)
    # color function between two colors
    pal5 = colorRampPalette(col.use) 
    # vector of colors using gradient according to one environmental column
    vct_cols = pal5(10)[as.numeric(cut(pts1[,3],breaks = 10))]
    # E-space
    dev.new()
    # scatter plots of all the environmental combinations
    pairs(pts1[,3:length(pts1)], lower.panel = NULL, col = vct_cols, main = 'E-space',
          cex = 0.5, pch = 16)
    # G-space
    dev.new()
    # plot the points that cover the area of interest
    plot(pts_sp, col = 'grey', main = 'G-space') 
    # add the boundary of the area
    plot(wrld_simpl, xlim = c(pts_sp@bbox[1,]), ylim = c(pts_sp@bbox[2,]), add = T)
  }
  return (pts1) # return dataframe 
}
#
#' @describeIn e_space_cat first transforms a raster stack of environmental
#' variables into a dataframe that contains the geographic coordinates and
#' environmental values as extracted from the raster file, and then assigns
#' the suitability value of each spatial point as indicated in the ctgr file;
#' it also displays the points into E-space and G-space.
# CODE e_space_cat ---------
# Dependencies: maptools, wrld_simpl, raster 
e_space_cat = function(stck,ctgr,pflag=F,col.use=NULL,wrld_crs=crs(wrld_simpl)){
  
  #1. obtain world shape and projections: 
  data("wrld_simpl", package = "maptools") #obtain in-built world shape
  WGS84 = crs(wrld_simpl) #obtain correct projection 
  
  #2. create full dataframe divided by available categories: 
  rr = list() #empty list 
  for (i in 1:ctgr@data@max){ #obtain the number of categories in the raster (e.g., binary, thresholded)
    pre_ras = rasterToPoints(ctgr, fun = function(x){x == i}) #transform in loops by category
    pre_vals = data.frame(extract (stck, pre_ras[,1:2])) #extract values from the stack
    pre_df = cbind(pre_ras, pre_vals) #combine dataframes
    rr[[length(rr)+1]] = pre_df #add dataframes to the empty list 
  }
  def_df = ldply(rr, data.frame) #create a dataframe from all the elemenst of the list 
  
  #3. e-space
  dev.new() #open figure space 
  pal5 = colorRampPalette(col.use)
  pairs (def_df[,4:length(def_df)], lower.panel = NULL, #write all the environmental combinations
         pch = 1+def_df[,3], col = pal5(length(unique(def_df[,3])))[def_df[,3]], cex = 0.5) #HERE MAYBE ADD SAME SYMBOLS AS IN THE MAP! 
  
  #4. g-space
  #create SpatialPointsDataframe to obtain extent
  pts_sp = SpatialPointsDataFrame (def_df[,1:2],def_df, proj4string = WGS84) #transform values into spatial point dataframe for extent 
  dev.new() #open second figure space
  plot(pts_sp, col = pal5(length(unique(def_df[,3])))[def_df[,3]], pch = 1+def_df[,3], cex = 0.5, main = 'G-space') #plot the points 
  plot(wrld_simpl, xlim = c(pts_sp@bbox[1,]), ylim = c(pts_sp@bbox[2,]), add = T) #add the corresponding shape 
  suit_class = paste("Suitability value",unique(def_df[,3]))
  legend('bottomleft', legend=suit_class, pch = 1+unique(def_df[,3]), cex = 0.7,
         col = pal5(length(unique(def_df[,3]))))
  return (def_df) #complete dataframe
}
#
#' @describeIn e_space  allows the comparison of all the environmental spaces vs
#' a background defined as the a raster stack provided by the user. It can be the
#' same environmental variables of the studied area to depict how much of the
#' environemnts of the area are actually used/not used by the model. If other
#' environments are used, it depicts how different environemnts of the models
#' are in comparison to the selected background.
# CODE e_space_cat_back ---------
# Dependencies: maptools, wrld_simpl, raster, plyr 
e_space_cat_back = function (stck, ctgr, bck){
  #1. obtain world shape and projections: 
  data("wrld_simpl", package = "maptools") #obtain in-built world shape
  WGS84 = crs(wrld_simpl) #obtain correct projection 
  
  #2. create full dataframe divided by available categories: 
  rr = list () #empty list 
  for (i in 1:ctgr@data@max){ #obtain the number of categories in the raster (e.g., binary, thresholded)
    pre_ras = rasterToPoints(ctgr, fun = function (x){x == i}) #transform in loops by category
    pre_vals = data.frame (extract (stck, pre_ras[,1:2])) #extract values from the stack
    pre_df = cbind (pre_ras, pre_vals) #combine dataframes
    rr[[length(rr)+1]] = pre_df #add dataframes to the empty list 
  }
  def_df = ldply (rr, data.frame) #create a dataframe from all the elemenst of the list 
  
  #3. Background variables
  bck_ras = data.frame(rasterToPoints(bck, fun = NULL))
  bck_ras = cbind (bck_ras[,1:2],rep(length(unique (def_df[,3]))+1,length(bck_ras[,1])), #add here the rep for the total categories +1 otherwise could cause an error
                   bck_ras[,3:length(bck_ras)])
  names (bck_ras)[3] = names (ctgr)
  def_df2 = rbind (bck_ras, def_df) #add the background category to the dataframe 
  
  #4. e-space
  dev.new () #open figure space 
  #mycols = c(brewer.pal(n = length(unique(def_df[,3])), name = 'PRGn'), 'grey')
  pal5 = colorRampPalette(c('#AF8DC3', '#7FBF7B'))
  pairs (def_df2[,4:length(def_df2)], lower.panel = NULL, #write all the environmental combinations
         pch = 1 , col = c(pal5(length(unique(def_df[,3]))), 'grey')[def_df2[,3]], cex = 0.5)
  
  #5. g-space, only target stack...background not here... 
  #create SpatialPointsDataframe to obtain extent
  pts_sp = SpatialPointsDataFrame (def_df[,1:2],def_df, proj4string = WGS84) #transform values into spatial point dataframe for extent 
  dev.new () #open second figure space
  plot (pts_sp, col = pal5(length(unique(def_df[,3])))[def_df[,3]], pch = 1+def_df[,3], cex = 0.5) #plot the points 
  plot (wrld_simpl, xlim = c(pts_sp@bbox[1,]), ylim = c(pts_sp@bbox[2,]), add = T) #add the corresponding shape 
  cat_names = paste("Suitability",unique(def_df[,3]))
  legend('bottomleft', legend=cat_names, pch = 1+unique(def_df[,3]), cex = 0.7, 
         col = pal5(length(unique(def_df[,3]))))
#add grey color meaning to legend
  return (def_df2) #complete dataframe
} 

#
# Daniel Romero-Alvarez & Laura Jimenez, 2020
