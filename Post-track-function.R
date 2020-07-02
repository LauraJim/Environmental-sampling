# Functions 'post_track' ----------
### This function ...
### ...
#
#' @param data_track dataframe with coordinates, categories, track class
#' @param uncert_ras raster measuring variability
#' @param pgly polygon (shape file)
#' @return
#' \code{post_track} returns a dataframe with coordinates, categories,
#' environmental values per pixel, track number, uncertainty values.
#' 
#' @describeIn post_track depicts the points selected in either the
#' environmental or geographical sampling over the uncertainty surface
#' calculated during model selection. It allows to see whether the
#' points occupy high or low suitability areas.

# CODE post_track ---------
# Dependencies: 
post_track = function(data_track, uncert_ras, pgly){
  #selecting color ramp 
  pal4 = colorRampPalette(c('#AF8DC3', '#7FBF7B'))
  #mycols = brewer.pal(n = length(unique(data_track[,3])), name = 'PRGn') #for tracking points 
  pal5 = colorRampPalette(c('lightyellow', '#FFA571')) #for uncertainty raster
  
  #eliminating duplicates: POTENTIALLY THIS SHOULD BE DONE IN THE OTHER FUNCTIONS... 
  data_track$dup = paste(data_track[,3], data_track[,1], data_track[,2], sep= '_')
  data_track = data_track[!duplicated(data_track$dup),]
  data_track$dup = NULL
  
  #plotting points against uncertainty map 
  dev.new()
  #points 1: 
  plot (data_track[,1], data_track[,2], 
        main = 'Uncertainty',
        xlab = 'Latitude',
        ylab = 'Longitude',
        col = pal4(length(unique(data_track[,3])))[data_track[,3]], 
        pch = 1+data_track[,dim(data_track)[2]], cex = 0.5)
  #raster and shape:
  plot (uncert_ras, col = pal5(13), add = T)
  plot (pgly, add = T)
  #points 2: 
  points (data_track[,1], data_track[,2], 
          col = pal4(length(unique(data_track[,3])))[data_track[,3]], 
          pch = 1+data_track[,dim(data_track)[2]], cex = 0.5)
  tr.names = paste ('Transect', unique(data_track[,dim(data_track)[2]]))
  legend ('bottomleft', legend = tr.names, pch = 1+unique(data_track[,dim(data_track)[2]]))
  
  #adding uncertainty values 
  unc_data = extract (uncert_ras, data_track[,1:2])
  def_unc = cbind (data_track, uncertainty_val = unc_data)
  
  return (def_unc) #definitive dataframe no longer usful for any function because ot the newer column
}
