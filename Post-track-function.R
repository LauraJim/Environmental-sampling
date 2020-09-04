# Functions 'post_track' ----------
### This function allows the cleaning of the dataframe constructed via the 'Hutchinson' function and overlap  
### the dataframe with a raster of the selection of the user. Here we overlap the points with the raster that 
### examplifies uncertainty of the final model outputs on this particular excercise. 
#
#' @param tracks dataframe with coordinates, categories, track class
#' @param uncert_ras raster measuring variability. It can be replaced by any other raster, e.g., altitude. 
#' @param plyg polygon that delimits the geographical space of interest
#' @param col.use vector of lenght two with the colors to be used in plots
#' 
#' @return
#' \code{post_track} returns a dataframe with coordinates, categories,
#' environmental values per pixel, track number, uncertainty values.
#' 
#' @describeIn post_track depicts the points selected in either the
#' environmental or geographical sampling over the uncertainty surface
#' calculated during model selection. It allows to see whether the
#' points occupy areas of high or low uncertainty.
#' 
# CODE post_track ---------
# Dependencies: none
post_track = function(tracks, uncert_ras, plyg, col.use = NULL){
  if(is.null(col.use)){
    print("Please define 'col.use' using two colors")
  } else{
    # color ramp for points
    pal = colorRampPalette(col.use)
    
    # eliminating duplicates
    tracks$dup = paste(tracks[,3], tracks[,1], tracks[,2], sep= '_')
    tracks = tracks[!duplicated(tracks$dup),]
    tracks$dup = NULL
    
    # plotting points on top of the uncertainty map 
    dev.new()
    # plot uncertainty raster and shapefile with region of interest
    plot (uncert_ras, main = 'Uncertainty levels', xlab = 'Latitude', ylab = 'Longitude')
    plot (plyg, add = T)
    # add points 1: 
    points(tracks[,1:2],
          col = pal(length(unique(tracks[,3])))[tracks[,3]], 
          pch = 1+tracks[,dim(tracks)[2]], cex = 0.5)
    tr.names = paste('Transect', unique(tracks[,dim(tracks)[2]]))
    legend('bottomleft', legend = tr.names, pch = 1+unique(tracks[,dim(tracks)[2]]))
    
    #adding uncertainty values 
    unc_data = extract(uncert_ras, tracks[,1:2])
    def_unc = cbind(tracks, uncertainty_val = unc_data)
    
    return (def_unc) 
  }
}


# END
# Daniel Romero-Alvarez & Laura Jimenez, 2020