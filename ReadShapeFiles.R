library(tidyverse)
library(sf)
library(raster)
library(ggspatial)
library(viridis)
library(igraph)
library(riverconn)
library(elevatr)
library(gridExtra)
library(ggnetwork)
library(lwgeom)
library(corrmorant)
library(RANN)
library(ggpubr)
library(cowplot)
library(dplyr)
library("riverconn")
library("shp2graph")
library(ggrepel)
library(rlist)

setwd("E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/ShapeFiles/")

#read shapefile, make a list of file names grouped basin-wise
filenames <- list.files(pattern="*.shp", full.names=FALSE)
g <- sub("(.+?)(\\_.*)", "\\1", filenames)
g <- split(filenames, g)

#call function to loop through each basin
listofres = lapply(g,collate)
out.df <- (do.call("rbind", listofres))
out.df

# for each basin, extract the basin name, and pass river, dam and wshed shape file to index calculation function
collate <- function(basin_vars){
  
  basin_name <- sub("(.+?)(\\_.*)", "\\1", basin_vars[1])
  
  # parse through file names to detect river, wshed and dam files
  river_file = basin_vars[which(as.numeric(grepl('river', basin_vars)) == 1)]
  wshed_file = basin_vars[which(as.numeric(grepl('wshed', basin_vars)) == 1)]
  dams_file = basin_vars[which(as.numeric(grepl('SHPs', basin_vars)) == 1)]
  
  #send river, wshed and dame file names to be read and for calculating index
  res = index_calc_wrapper(basin_name,river_file,dams_file,wshed_file)
  return(res)
}

index_calc_wrapper <- function(name, river_file, dam_file, wshed_file){  

  shape_river <- st_read(river_file) #confluences removed
  shape_basin <- st_read(wshed_file)
  shape_dams <- st_read(dam_file)

    ggplot() +
    coord_fixed() +
    theme_minimal() +
    ggspatial::layer_spatial(shape_basin, fill = NA, color = "gray90") +
    ggspatial::layer_spatial(shape_river, aes(color = log10(UPLAND_SKM)) )+
    ggspatial::layer_spatial(shape_dams,color = "red") +
    scale_color_viridis(direction = -1, name= "upstream area \n(log10[Km^2])") +
    theme(legend.position = "bottom") +
    ggspatial::annotation_scale(location = "bl", style = "ticks") +
    ggspatial::annotation_north_arrow(location = "br")+
    labs(caption = "Black dots are the position of the dams")+
    ggtitle(paste(name,"wshed"))  
    
    return(data.frame(name = name, Sum_length = sum(shape_river$DIST_UP_KM),mean_area = mean(shape_basin$UP_AREA)))
}

