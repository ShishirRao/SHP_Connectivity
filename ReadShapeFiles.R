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


# for each basin, extract the basin name, and pass river, dam and wshed shape file to index calculation function
collate <- function(basin_vars){
  
  #basin_vars = g[30][[1]]
  basin_name <- sub("(.+?)(\\_.*)", "\\1", basin_vars[1])
  
  # parse through file names to detect river, wshed and dam files
  river_file = basin_vars[which(as.numeric(grepl('river', basin_vars)) == 1)]
  wshed_file = basin_vars[which(as.numeric(grepl('wshed', basin_vars)) == 1)]
  SHP_file = basin_vars[which(as.numeric(grepl('SHPs', basin_vars)) == 1)]
  LargeDams_file = basin_vars[which(as.numeric(grepl('LargeDams', basin_vars)) == 1)]
  
  #data.frame(name = c("A1","A2"), index = c(as.numeric(1),as.numeric(2)),type = c("SHPs","LargeDams"))
  
  #if dam files are missing, then return here itself
  if (rlang::is_empty(SHP_file) & rlang::is_empty(LargeDams_file)){
    print(paste(basin_name,"no dams found"))
    return(data.frame(name = c(basin_name,basin_name),
                      index = c(as.numeric(1),as.numeric(1)),
                      type = c("SHPs","LargeDams")))  
  }
  
  # read shape files
  shape_river <- st_read(river_file) 
  shape_basin <- st_read(wshed_file)
  
  #clear the results variable
  res1 = res2 = NULL
  
  if (!rlang::is_empty(SHP_file)){ # if SHP shape file is not empty, then read it
    shape_SHPs <- st_read(SHP_file)
    # ignore irrigation canal SHPs
    shape_SHPs = shape_SHPs[shape_SHPs$Sitatued.o == "river" | shape_SHPs$Sitatued.o == "part of bigger project",]
    res1 = index_calc_wrapper(basin_name,shape_river,shape_SHPs,shape_basin)
    #res1 = data.frame(basin_name,index = 20)
    res1 = cbind(res1,type = "SHPs")
    print(res1)
  }
  
  if (!rlang::is_empty(LargeDams_file)){
    shape_Large_dams <- st_read(LargeDams_file)
    #send river, wshed and dame file names to be read and for calculating index
    shape_Large_dams$Sitatued.o = "river_non_SHP"
    res2 = index_calc_wrapper(basin_name,shape_river,shape_Large_dams,shape_basin)
    #res2 = data.frame(basin_name,index = 30)
    res2 = cbind(res2,type = "LargeDams")
    print(res2)
  }
  return(rbind(res1,res2))
}


# this function takes basin name, river_file, dam_file, wshed_file as inputs and returns network stats as a list
index_calc_wrapper <- function(name, shape_river, shape_dams, shape_basin){  
  
  #shape_dams = shape_Large_dams
  #name = basin_name
  #pruned river network
  # Set a threshold of 10 square kilometers
  threshold = 10
  
  # Prune HydroRIVERS network based on upstream area
  shape_river_small <- shape_river[as.numeric(shape_river$UPLAND_SKM) > threshold,]
  
  
  ###### Confluence processing #######
  
  # Simplify river shapefile
  shape_river_simple <- shape_river_small %>%
    st_as_sf %>%
    st_union()
  
  # Convert shapefile to point object
  river_to_points <- shape_river_simple %>%
    st_as_sf %>%
    st_cast("POINT") %>%
    mutate(id = 1:nrow(.))
  
  # Check points that overlap
  joins_selection <- river_to_points %>%
    st_equals() %>%
    Filter(function(x){length(x) > 2}, .) %>%
    lapply(., FUN = min) %>%
    unlist() %>%
    unique()
  
  # Filter original point shapefile to retain only confluences
  river_joins <- river_to_points %>% 
    filter(id %in% joins_selection)
  
  # Split polyline
  shape_river_simplified <- lwgeom::st_split(shape_river_simple, river_joins) %>%
    st_collection_extract(.,"LINESTRING") %>%
    data.frame(id = 1:length(.), geometry = .) %>%
    st_as_sf() %>%
    mutate(length = st_length(.))
  
  
  #### Dams processing ####

  # add id row
  shape_dams <- shape_dams %>%
    mutate(id = 1:nrow(.))
  
  # Snap dams
  dams_snapped <- snap_to_river(shape_dams,
                                shape_river_simple %>% st_sf(),
                                max_dist = 1000)
  
  # Retain dams that were snapped. 
  dams_snapped_reduced <-
    dams_snapped[st_contains(shape_river_simple %>% st_sf(), dams_snapped, prepared = FALSE)[[1]],]
  
  # Check if dams were snapped properly to the network (all the distances should be zero)
  st_distance(dams_snapped_reduced, shape_river_simple) %>% sum
  st_distance(dams_snapped, shape_river_simple) %>% sum
  ## 0 [m]
  
  # this piece of code removes dams that are at the same location, and attaches the river network attributes to the dam points. 
  dams_snapped_joined <- dams_snapped_reduced %>%
    mutate(cluster =
             st_equals(dams_snapped_reduced, dams_snapped_reduced) %>%
             sapply(., FUN = function(x) paste0(x, collapse = "_"))) %>%
    group_by(cluster) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(id_dam = as.character(1:nrow(.)), pass_u = 0,pass_d = 0) %>%
    as.data.frame %>%
    st_as_sf() %>%
    st_join(., shape_river, join = st_is_within_distance, dist = 10 ) %>% 
    group_by(id) %>%
    slice(which.max(UPLAND_SKM)) %>% 
    ungroup()
  
  ggplot() +
    coord_fixed() +
    theme_minimal() +
    ggspatial::layer_spatial(shape_river_small, color = "gray90" )+
    ggspatial::layer_spatial(shape_dams,color = "blue") +
    ggspatial::layer_spatial(dams_snapped_reduced,color = "black") +
    ggspatial::layer_spatial(dams_snapped_joined,color = "red") +
    scale_color_viridis(direction = -1, name= "upstream area \n(log10[Km^2])") +
    theme(legend.position = "bottom") +
    ggspatial::annotation_scale(location = "bl", style = "ticks") +
    ggspatial::annotation_north_arrow(location = "br")
  
  nrow(shape_SHPs)
  nrow(shape_Large_dams)
  nrow(shape_dams)
  nrow(shape_SHPs)+nrow(shape_Large_dams)
  nrow(dams_snapped_joined)
  
    headwaters_checking <- headwaters_dam(dams_snapped_joined, shape_river_simple)
    head(headwaters_checking$flag_headwater)
    
  # Create junction point shapefile
  network_links <- rbind(
    dams_snapped_joined %>% 
      mutate(type = "dam", id_barrier = id_dam) %>%
      dplyr::select(type, id_barrier, pass_u, pass_d),
    river_joins %>% mutate(type = "joint") %>%
      dplyr::select(type) %>%
      mutate(id_barrier = NA, pass_u = NA, pass_d = NA) %>%
      rename(geometry = x)) %>%
    mutate(id_links = 1:nrow(.))
  
  # Split river network
  river_net_simplified <- lwgeom::st_split(shape_river_simple, network_links) %>%
    st_collection_extract(.,"LINESTRING") %>%
    data.frame(NodeID = 1:length(.), geometry = .) %>%
    st_as_sf() %>%
    mutate(length = st_length(.)) %>%
    st_join(., shape_river_small, join = st_is_within_distance, dist = 0.01 ) %>% 
    group_by(NodeID) %>%
    slice(which.max(UPLAND_SKM)) %>% 
    ungroup()
  
  #check for 3+ line segments joining to form a confluence
  confluences <- multiple_confluences(river_net_simplified) 
  head(confluences)
  
  #Check for disconnected line segments
  shp_check <- check_components(network_links, river_net_simplified)
  head(shp_check)
  
  # get DEM and transform to data frame with coordinates
  elevation <- get_elev_raster(shape_basin, z = 8)
  catchment_DEM <- raster::as.data.frame(elevation, xy = TRUE)
  
  # Get coordinates of the river network segments
  river_net_simplified_centroids <- river_net_simplified %>%
    st_as_sf() %>%
    st_centroid()
  
  # Get coordinates of both elements for joining
  Coord_Edges <- st_coordinates(river_net_simplified_centroids) #Coordinates of the joins
  Coord_DEM <- catchment_DEM[,1:2] #Coordinates of the dams
  
  # Matching each centroid with its closer altittude point to later obtain the altitudes
  matching_altitudes <- RANN::nn2(data=Coord_DEM, query = Coord_Edges, k=1, radius = 1)[[1]] 
  
  # Get values and add to the river shapefile
  catchment_DEM <- catchment_DEM[matching_altitudes,3]
  river_net_simplified <- river_net_simplified %>% 
    mutate(alt = catchment_DEM)
  
  # To avoid issues in the network creation process, retain only the important columns
  river_net_simplified <- river_net_simplified %>% 
    dplyr::select(NodeID, length, alt, DIST_DN_KM, UPLAND_SKM)
  
  # use DIST_DN_KM to find the downstream most outlet 
  outlet <- river_net_simplified$NodeID[which(river_net_simplified$DIST_DN_KM == min(river_net_simplified$DIST_DN_KM))]
  
  #if there are multiple segments, find the one with lowest elevation. Note: Just using elevation won't work because some segments
  # can have elevation lower than the downstream most segment, due to inaccuracies in the elevation data
  if(length(outlet)>1){
    dwn_seg <- river_net_simplified[which(river_net_simplified$DIST_DN_KM == min(river_net_simplified$DIST_DN_KM)),]
    outlet <- dwn_seg$NodeID[which(dwn_seg$alt == min(dwn_seg$alt))]
    # if there still are multiple segments, then a decision has to be made based on other variables, to select the outlet.
    # select the segment with the shortest length. no other option because dist_dn_km and alt are same for each segment
    if(length(outlet)>1){
      outlet <- dwn_seg$NodeID[which(dwn_seg$length == min(dwn_seg$length))]  
    }
  }

  ######## create igraph object  ########
  river_graph <- create_network(network_links, river_net_simplified, outlet)
  
  # check river_graph edges
  igraph::edge_attr(river_graph) %>% names
  ## [1] "type"       "id_links"   "id_barrier" "pass_u"     "pass_d"
  
  # check river_graph vertices
  igraph::vertex_attr(river_graph) %>% names
  ## [1] "name"       "length"     "alt"        "DIST_DN_KM" "UPLAND_SKM"
  
  
  # update length attribute
  V(river_graph)$length <- V(river_graph)$length / 10000
  
  # update length attribute
  V(river_graph)$name <- as.character(V(river_graph)$name)
  
  
  # Initialize list where to store all the index calculation outputs
  index <- list()
  lab_index <- list()
  letter_index <- list()
  
  # 1: Symmetric Dendritic Connectivity Index (no biotic effects)
  lab_index[[1]] <- "Symmetric DCI"
  letter_index[[1]] <- "A"
  index[[1]] <- index_calculation(graph = river_graph,
                                  weight = "length",
                                  c_ij_flag = TRUE,
                                  B_ij_flag = FALSE,
                                  index_type = "full",
                                  index_mode = "from")
  
    
    return(data.frame(name = name, DCIp = index[[1]][3]))
}

#read shapefile, make a list of file names grouped basin-wise
setwd("E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/ShapeFiles/")

#Read all the shapefiles
filenames <- list.files(pattern="*.shp", full.names=FALSE)
#This expression seperates the basin name from _wshed, _river, or _SHP of the file name
g <- sub("(.+?)(\\_.*)", "\\1", filenames)
g <- split(filenames, g)
g


#call function to loop through each basin calculating DCI
listofres = lapply(g,collate)
out.df <- (do.call("rbind", listofres))
out.df <- out.df %>% `rownames<-`(seq_len(nrow(out.df)))
names(out.df) <- c("Basin_name","DCIp","Type")
out.df$DCIp = out.df$DCIp*100
out.df


# prepare the output for display
#read only the wsheds
wshednames <- list.files(pattern="*wshed.shp", full.names=FALSE)
wshed_shp_files <- lapply(wshednames, read_sf)
basin_names <- sub("(.+?)(\\_.*)", "\\1", wshednames)
wsheds <-bind_rows(wshed_shp_files)
wsheds$Basin_name = basin_names

wsheds = left_join(wsheds,out.df)
#st_write(wsheds, "E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/Basins/Results_DCI.shp", delete_layer = TRUE)

getwd()

?st_write()


