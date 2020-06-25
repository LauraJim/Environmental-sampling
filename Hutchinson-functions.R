# Functions 'hutchinson_e_g' and 'hutchinson_g_e' ----------
### These two functions ...
### ...
#
#' @param data dataframe with the following columns: longitude, latitude,
#'   category 1, ..., category n, environmental variables (one per column).
#' @param calls vector length of two that indicates the columns to be used
#'   in the plots.
#' @param pglypolygon that delimits the geographical space of interest.
#' @param ntr number of transects to be drawn.
#' @return
#' \code{hutchinson_e_g} returns a database with coordinates, values, categories,
#' and the selected tracks.
#' \code{hutchinson_g_e} returns a database with coordinates, values, categories,
#' and the selected tracks.
#' 
#' @describeIn hutchinson_e_g plots the selected environmental space and allows
#' the user to select transects that are then represented in the geographical
#' space.
# CODE hutchinson_e_g ---------
# Dependencies: 
hutchinson_e_g <- function(data, calls, pgly, ntr){
  #defining color ramp for plotting: 
  pal5 = colorRampPalette(c('#AF8DC3', '#7FBF7B'))
  
  # Plot 1: E-space
  dev.new()
  plot(data[,calls[1]], data[,calls[2]], col = pal5(length(unique(data[,3])))[data[,3]],
       main="E-space", cex = 0.5, pch = 1+data[,3],
       xlab = paste ('Env_var1:', colnames(data[calls[1]])), 
       ylab = paste ('Env_var2:', colnames(data[calls[2]])))
  suit_class = paste("Suitability value",unique(data[,3]))
  # legend('bottomleft', legend= suit_class,
  #        col= pal5(length(unique(data[,3]))), pch = 1+unique(data[,3]), cex=0.7)
  # defining transects in E-space
  transects <- vector("list",length=ntr)
  for(j in 1:ntr){
    # left click to create points, right click to finish
    ss = fhs(data.frame(data[,calls[1]], data[,calls[2]]),pch=1+j)
    transects[[j]] = cbind(as.matrix(data[ss,]),rep(j,length(ss)))
    #transects[[j]] = cbind(as.matrix(data[ss[1:length(ss)],]),rep(j,length(ss)))
  }
  all.tr = data.frame (do.call(rbind,transects)) #I left it as a dataframe, originally matrix  #POTENTIALLY ELIMINATE DUPLICATES 
  
  # Plot 2: geography
  dev.new()
  plot (pgly, main = paste ('G-space'))
  points (all.tr[,1:2], col = pal5(length(unique(all.tr[,3])))[all.tr[,3]], pch = 1+all.tr[,dim(all.tr)[2]], cex = 0.8)
  tr.names <- paste("Transect",1:ntr)
  legend('bottomleft', legend=tr.names, pch = 1+(1:ntr))
  # save new matrix with selected points
  return(all.tr)
}
#' @describeIn hutchinson_e_g plots the selected geographical space and allows
#' the user to select transects that are then represented in the environmental
#' space.
#' 
# CODE hutchinson_g_e ---------
# Dependencies: 
hutchinson_g_e <- function(data, calls, pgly, ntr){
  #selecting color ramp 
  #mycols = brewer.pal(n = length(unique(data[,3])), name = 'PRGn')
  pal5 = colorRampPalette(c('#AF8DC3', '#7FBF7B'))
  
  #dataframe in spatial object 
  data("wrld_simpl", package = "maptools")
  sp_ob = SpatialPointsDataFrame(data[,1:2], data, proj4string = crs(wrld_simpl))
  
  # Plot 1: G-space
  dev.new()
  plot (sp_ob, main = paste ('G_space'), 
        col = pal5(length(unique(sp_ob@data[,3])))[sp_ob@data[,3]], pch = 1+sp_ob@data[,3], cex = 0.8)
  plot (pgly, add = T)
  suit_class = paste("Suitability value",unique(sp_ob@data[,3]))
  legend('bottomleft', legend=suit_class,
         col= pal5(length(unique(sp_ob@data[,3]))), pch = 1+ unique(sp_ob@data[,3]), cex=0.7)
  
  # defining transects in G-space
  transects <- vector("list",length=ntr)
  for(j in 1:ntr){
    # left click to create points, right click to finish
    ss = fhs(sp_ob@data,pch=1+j)
    transects[[j]] = cbind(as.matrix(sp_ob@data[ss,]),rep(j,length(ss))) #here I am subsetting the entire dataset according to the index selected 
  }
  all.tr = data.frame (do.call(rbind,transects)) #I just left this as a dataframe! #POTENTIALLY ELIMINATE DUPLICATES 
  
  # Plot 2: E-space
  dev.new()
  plot(all.tr[,calls[1]], all.tr[,calls[2]], 
       cex = 0.8, col = pal5(length(unique(all.tr[,3])))[all.tr[,3]], pch = 1+all.tr[,dim(all.tr)[2]],
       main="E-space", 
       xlab = paste ('Env_var1:', colnames(all.tr[calls[1]])), 
       ylab = paste ('Env_var2:', colnames(all.tr[calls[2]])))
  tr.names = paste ('Transect', 1:ntr)
  legend ('bottomleft', legend = tr.names, pch = 1+(1:ntr))
  return(all.tr)
}

#
# Daniel Romero-Alvarez & Laura Jimenez, 2020