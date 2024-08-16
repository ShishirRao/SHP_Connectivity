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
lapply(g,collate)


# for each basin, extract the basin name, and pass river, dam and wshed shape file to index calculation function
collate <- function(basin_vars){
  basin_name <- sub("(.+?)(\\_.*)", "\\1", basin_vars[1])
  index_calc_wrapper(basin_name,basin_vars[1],basin_vars[2],basin_vars[3])
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
    ggtitle(paste(name,"basin"))  
}


x <- list(p1 = list(type='A',score=c(c1=9)),
          p2 = list(type=c('A','B'),score=c(c1=8,c2=9)),
          p3 = list(type=c('B','C'),score=c(c1=9,c2=7)),
          p4 = list(type=c('B','C'),score=c(c1=8,c2=NA)))

## Search exact values
list.search(x, identical(., 'A'))
?list.search

data <- list(
  p1 = list(name='Ken',age=24),
  p2 = list(name='Kent',age=26),
  p3 = list(name='Sam',age=24),
  p4 = list(name='Keynes',age=30),
  p5 = list(name='Kwen',age=31)
)

list.search(data, grepl('^K', .), 'character')

