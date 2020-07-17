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
#' @param wrld_map reference map used to plot in geographic space; default is wrld_simpl
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
#
e_space <- function(stck, pflag = F, col.use = NULL, wrld_map = wrld_simpl){
  # transform raster into referenced points and into a data frame
  pts1 = data.frame(rasterToPoints(stck, fun = NULL))
  # Plotting if TRUE
  if(pflag){
    # transform values into spatial points to plot on map
    pts_sp = SpatialPointsDataFrame(pts1[,1:2], pts1, proj4string = crs(wrld_map))
    # create function between two colors
    if(is.null(col.use)){
      print("Please define 'col.use' using two colors")
    } else{
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
      plot(wrld_map, xlim = c(pts_sp@bbox[1,]), ylim = c(pts_sp@bbox[2,]), add = T)
    }
  }
  return(pts1) # return dataframe 
}
#
#' @describeIn e_space_cat first transforms a raster stack of environmental
#' variables into a dataframe that contains the geographic coordinates and
#' environmental values as extracted from the raster file, and then assigns
#' the suitability value of each spatial point as indicated in the ctgr file;
#' it also displays the points into E-space and G-space.
# CODE e_space_cat ---------
# Dependencies: maptools, wrld_simpl, raster, plyr
#
e_space_cat <- function(stck, ctgr, pflag = F, col.use = NULL, wrld_map = wrld_simpl){
  # Create full dataframe of coordinates and climatic values divided by categories
  rr = list()
    # Obtain the number of categories in the raster (e.g., binary, thresholded)
    for (i in 1:ctgr@data@max){
      # use categories in raster and convert to points
      pre_ras = rasterToPoints(ctgr, fun = function(x){x == i})
      # extract environmental values for those points
      pre_vals = data.frame(extract (stck, pre_ras[,1:2]))
      # combine and save coordinates, categories, and environmental values
      rr[[i]] = cbind(pre_ras, pre_vals)
    }
  # create a single dataframe with all the elemenst of the list
  def_df = ldply(rr, data.frame)
  # Plotting if TRUE
  if(pflag){
    if(is.null(col.use)){
      print("Please define 'col.use' using two colors")
    } else{
      # E-space
      dev.new()
      # create function between two colors
      pal5 = colorRampPalette(col.use)
      # determine the number of categories
      catnum = length(unique(def_df[,3]))
      # scatter plots of all the environmental combinations
      pairs(def_df[,4:ncol(def_df)], lower.panel = NULL, pch = 1+def_df[,3], cex = 0.5,
            col = pal5(catnum)[def_df[,3]], main = 'E-space')
      #HERE MAYBE ADD SAME SYMBOLS AS IN THE MAP! 
      # G-space
      # create SpatialPointsDataframe to obtain extent
      pts_sp = SpatialPointsDataFrame(def_df[,1:2],def_df, proj4string = crs(wrld_map))
      dev.new()
      # plot the points that cover the area of interest and identify them with its category
      plot(pts_sp, col = pal5(catnum)[def_df[,3]], pch = 1+def_df[,3], cex = 0.5, main = 'G-space')
      # add the boundary of the area
      plot(wrld_map, xlim = c(pts_sp@bbox[1,]), ylim = c(pts_sp@bbox[2,]), add = T)
      suit_class = paste("Suitability value",unique(def_df[,3]))
      legend('bottomleft', legend = suit_class, pch = 1+unique(def_df[,3]), cex = 0.7,
             col = pal5(catnum))
    }
  }
  return(def_df) #complete dataframe
}
#
#' @describeIn e_space  allows the comparison of all the environmental spaces vs
#' a background defined as the a raster stack provided by the user. It can be the
#' same environmental variables of the studied area to depict how much of the
#' environemnts of the area are actually used/not used by the model. If other
#' environments are used, it depicts how different environemnts of the models
#' are in comparison to the selected background.
# CODE e_space_cat_back ---------
# Dependencies: maptools, wrld_simpl, raster, plyr, e_space_cat
#
e_space_cat_back = function(stck, ctgr, bck, pflag = F, col.use = NULL, wrld_map = wrld_simpl){
  # Create full dataframe of coordinates and climatic values divided by categories
  def_df = e_space_cat(stck = stck, ctgr = ctgr, pflag = F) 
  # Determine the number of categories
  catnum = length(unique(def_df[,3]))
  # convert background into points
  bck_ras = data.frame(rasterToPoints(bck, fun = NULL))
  # add column with new category label (= number of categories + 1)
  bck_ras = cbind(bck_ras[,1:2], rep(catnum+1,nrow(bck_ras)), bck_ras[,3:ncol(bck_ras)])
  names(bck_ras)[3] = names(ctgr)
  # combine the dataframe of the area of study with the dataframe created with the background
  def_df2 = rbind(bck_ras, def_df)  
  # Plotting if TRUE
  if(pflag){
    if(is.null(col.use)){
      print("Please define 'col.use' using two colors")
    } else{
      # E-space
      dev.new()
      # create function between two colors
      pal5 = colorRampPalette(col.use)
      # scatter plots of all the environmental combinations
      pairs(def_df2[,4:ncol(def_df2)], lower.panel = NULL, main = 'E-space', pch = 1,
             col = c(pal5(catnum), 'grey')[def_df2[,3]], cex = 0.8)
      # Transform values into spatial point dataframe 
      pts_sp = SpatialPointsDataFrame(def_df2[,1:2],def_df2, proj4string = crs(wrld_map))
      # G-space
      dev.new()
      # plot the points
      plot(pts_sp, col = c(pal5(catnum), 'grey')[def_df2[,3]], pch = 1+def_df2[,3], cex = 0.5)
      # add the region of the world
      plot(wrld_map, xlim = c(pts_sp@bbox[1,]), ylim = c(pts_sp@bbox[2,]), add = T)
      # add legend
      cat_names = paste("Suitability",unique(def_df[,3]))
      legend('bottomleft', legend=cat_names, pch = 1+unique(def_df2[,3]), cex = 0.7,
             col = c(pal5(catnum), 'grey'))
    }
  }
  return (def_df2)
} 

# END
# Daniel Romero-Alvarez & Laura Jimenez, 2020
