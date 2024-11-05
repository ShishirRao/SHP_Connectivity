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
  #basin_vars = g[13][[1]]
  basin_name <- sub("(.+?)(\\_.*)", "\\1", basin_vars[1])
  
  # parse through file names to detect river, wshed and dam files
  river_file = basin_vars[which(as.numeric(grepl('river', basin_vars)) == 1)]
  wshed_file = basin_vars[which(as.numeric(grepl('wshed', basin_vars)) == 1)]
  dams_file = basin_vars[which(as.numeric(grepl('SHPs', basin_vars)) == 1)]
  
  if (rlang::is_empty(dams_file)){
    print(paste(basin_name,"has empty dam file "))
    return(data.frame(name = basin_name, index = as.numeric(1)))  
  }else if (rlang::is_empty(SHP_file)){
    print(paste(basin_name,"no SHPs found"))
    return(data.frame(name = basin_name,),
           index = as.numeric(1),
           type = "SHPs")  
  }else if (rlang::is_empty(LargeDams_file)){
    print(paste(basin_name,"no Large dams found"))
    return(data.frame(name = basin_name,
                      index = as.numeric(1),
                      type = "LargeDams"))
  }else{
    #send river, wshed and dame file names to be read and for calculating index
    res = index_calc_wrapper(basin_name,river_file,dams_file,wshed_file)
    #return(data.frame(name = basin_name, index = as.numeric(.2)))  
    return(res)
  }
}


# this function takes basin name, river_file, dam_file, wshed_file as inputs and returns network stats as a list
index_calc_wrapper <- function(name, river_file, dam_file, wshed_file){  
  
  # read shape files
  shape_river <- st_read(river_file) 
  shape_basin <- st_read(wshed_file)
  shape_dams <- st_read(dam_file)
  
  #if there are no dams 
  
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
  
  # remove irrigation canal based and offshore SHPs, and keep only stand-alone (river) and multipurpose SHPs
  shape_dams = shape_dams[shape_dams$Sitatued.o == "river" | shape_dams$Sitatued.o == "part of bigger project",]
  names(shape_dams)
  
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
names(out.df) <- c("Basin_name","DCIp")
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





dewatered = as.numeric(edges$from[!is.na(edge_attr(river_graph)$Company)])
dewatered = c(dewatered,as.numeric(edges$to[!is.na(edge_attr(river_graph)$Company)]))
dewatered = dewatered[duplicated(dewatered)]

dewatered_len = vertices$length[dewatered[duplicated(dewatered)]]
dewatered_DCP = (dewatered_len/sum(river_net_simplified$length))^2



#### index calculation #####
?index_calculation
graph = river_graph
weight = "length"
nodes_id = "name"
index_type = "full"
index_mode = "from"
c_ij_flag = TRUE
B_ij_flag = FALSE
dir_fragmentation_type = "asymmetric" 
pass_confluence = 1
pass_u = "pass_u"
pass_d = "pass_d"
field_B = "length"
dir_distance_type = "symmetric"
disp_type = "exponential"
param_u
param_d
param
param_l

graph = river_graph
weight = "length"
c_ij_flag = TRUE
B_ij_flag = FALSE
index_type = "full"
index_mode = "from"


index_calculation_dewater = 
  
if (!igraph::is_igraph(graph)) 
  stop("'graph' must be an 'igraph' object")
if (!(index_type %in% c("full", "reach", "sum"))) 
  stop("'index_type' must me either 'full', 'reach', or 'sum'")
if (index_type == "reach" & !(index_mode %in% c("from", "to"))) 
  stop("'index_mode' must me either 'from' or 'to'")
if (index_type == "reach" & missing(index_mode)) 
  stop("'index_mode' must me defined when index_type = 'reach'")
if (!(weight %in% igraph::vertex_attr_names(graph))) 
  stop("'weight' argument must be a valid vertex attribute in 'graph'")
if (!(nodes_id %in% igraph::vertex_attr_names(graph))) 
  stop("'nodes_id' argument must be a valid vertex attribute in 'graph'")
if (!(c_ij_flag | B_ij_flag)) 
  stop("at least one among c_if and B_ij should be selected for calculations")
if (length(igraph::vertex_attr(graph, nodes_id)) < igraph::gorder(graph)) 
  stop("'nodes_id' must be unique for each vertex")
if (!igraph::is_connected(graph)) 
  stop("'graph' must be connected (check if some nodes are disconnected with igraph::components() )")
if ((dir_fragmentation_type == "asymmetric" | dir_distance_type == 
     "asymmetric") & igraph::is_directed(graph) == FALSE) 
  stop("'graph' must be directed when 'dir_fragmentation_type' or 'dir_distance_type' are set to 'asymmetric'")
if (weight %in% igraph::edge_attr_names(graph)) 
  stop("'weight' argument must be a edge attribute in 'graph'")
if (field_B %in% igraph::edge_attr_names(graph)) 
  stop("'field_B' argument must be a edge attribute in 'graph'")
if (!is.character(igraph::get.vertex.attribute(graph, nodes_id))) 
  stop("'nodes_id' attribute of 'graph' must be of type 'charachter'")
if (c_ij_flag == TRUE) {
  if (!(pass_u %in% igraph::edge_attr_names(graph))) 
    stop("'pass_u' argument must be a edge attribute in 'graph'")
  if (!(pass_d %in% igraph::edge_attr_names(graph))) 
    stop("'pass_d' argument must be a edge attribute in 'graph'")
}
if (B_ij_flag == FALSE) {
  param_u = param_d = param = param_l <- NA
}
if (dir_distance_type == "symmetric") {
  param_u = param_d <- NA
}
if (dir_distance_type == "asymmetric") {
  param <- NA
}
if (disp_type == "leptokurtic") {
  param_u = param_d = param <- NA
}



igraph::V(graph)$name <- igraph::vertex_attr(graph, nodes_id)

if (c_ij_flag == TRUE) {
  c_ij_mat <- c_ij_fun(graph, dir_fragmentation_type = dir_fragmentation_type, 
                       pass_confluence = pass_confluence, pass_u = pass_u, 
                       pass_d = pass_d)
}
if (B_ij_flag == TRUE) {
  B_ij_mat <- B_ij_fun(graph, field_B = field_B, dir_distance_type = dir_distance_type, 
                       disp_type = disp_type, param_u = param_u, param_d = param_d, 
                       param = param, param_l = param_l)
}
if (c_ij_flag == TRUE & B_ij_flag == TRUE) {
  agg_mat <- c_ij_mat * B_ij_mat
}
if (c_ij_flag == TRUE & B_ij_flag == FALSE) {
  agg_mat <- c_ij_mat
}
if (c_ij_flag == FALSE & B_ij_flag == TRUE) {
  agg_mat <- B_ij_mat
}
g_v_df <- dplyr::rename_with(igraph::as_data_frame(graph, 
                                                   what = "vertices"), ~"weight_node", contains(weight))
v_weights <- g_v_df$weight_node
if (index_type == "full") {
  agg_mat_dew = agg_mat
  index_num = t(v_weights) %*% agg_mat 
  index_num = index_num %*% v_weights
  
  
  #This is always true. Make the dewatered stretch(es) Cij = 0
  if(length(dewatered) >= 1){
    #dewatered stretch can't be connected to any other stretch. 
    #So make the Cij of everything connected to that stretch = 0
    agg_mat_dew[dewatered,1:nrow(agg_mat_dew)] = agg_mat_dew[1:ncol(agg_mat_dew),dewatered] = 0
  }
  #Now connect the partly dewatered and free trib to the rest of the network
  if(length(party_dewatered) >= 1){
    
    # First connect dewatered to stretch to all the tribs to which
    # the stretch downstream of partly dewatered is connected to. 
    # do it row wise and then col wise
    agg_mat_dew[party_dewatered,1:nrow(agg_mat_dew)] = 
      as.numeric(agg_mat_dew[party_dewatered,1:nrow(agg_mat_dew)] | agg_mat_dew[dwnstream_party_dew,1:nrow(agg_mat_dew)])
    agg_mat_dew[1:ncol(agg_mat_dew),party_dewatered] = t(agg_mat_dew[party_dewatered,1:nrow(agg_mat_dew)])
    
    # do the same with free trib and party dewatered and free trib
    agg_mat_dew[free_trib,1:nrow(agg_mat_dew)] = 
      as.numeric(agg_mat_dew[free_trib,1:nrow(agg_mat_dew)] | agg_mat_dew[party_dewatered,1:nrow(agg_mat_dew)])
    agg_mat_dew[1:ncol(agg_mat_dew),free_trib] = t(agg_mat_dew[free_trib,1:nrow(agg_mat_dew)])
    
    # lastly, connect free trib with stretch downstream of partly dewatered
    agg_mat_dew[dwnstream_party_dew,free_trib] = agg_mat_dew[free_trib,dwnstream_party_dew] = 1 
    
  }
  
  
  
  #agg_mat_dew[dewatered[1],dewatered[2]] = agg_mat_dew[dewatered[2],dewatered[1]] = 0
  #agg_mat_dew[dewatered[1],dewatered[3]] = agg_mat_dew[dewatered[3],dewatered[1]] = 0
  #agg_mat_dew[dewatered[1],dewatered[3]] = agg_mat_dew[dewatered[3],dewatered[1]] = 0
  #agg_mat_dew[3,4] = agg_mat_dew[4,3]= 1
  
  
  index_num = t(v_weights) %*% agg_mat_dew 
  index_num = index_num %*% v_weights
  
  
  index_den = sum(v_weights)^2
  index = index_num/index_den
  index <- data.frame(num = index_num, den = index_den, 
                      index = index)
}
if (index_type == "reach") {
  if (index_mode == "to") {
    index_num = agg_mat %*% v_weights
  }
  if (index_mode == "from") {
    index_num = t(t(v_weights) %*% agg_mat)
  }
  index_den = sum(v_weights)
  index = index_num/index_den
  index = data.frame(name = igraph::V(graph)$name, num = index_num, 
                     den = index_den, index = index) %>% dplyr::rename_with(~nodes_id, 
                                                                            contains("name"))
}
if (index_type == "sum") {
  if (!("type" %in% igraph::edge_attr_names(graph))) 
    stop("the graph's edges must contain the 'type' attribute with labels 'dam' or\n      'link' depending on the role of the edge (barrier or confluence).\n      Essential to calculate CAFI properly.")
  igraph::E(graph)$pass_u <- igraph::get.edge.attribute(graph, 
                                                        pass_u)
  igraph::E(graph)$pass_d <- igraph::get.edge.attribute(graph, 
                                                        pass_d)
  igraph::E(graph)$pass_u <- ifelse(is.na(igraph::E(graph)$pass_u), 
                                    pass_confluence, igraph::E(graph)$pass_u)
  igraph::E(graph)$pass_d <- ifelse(is.na(igraph::E(graph)$pass_d), 
                                    pass_confluence, igraph::E(graph)$pass_d)
  if (dir_fragmentation_type == "symmetric") {
    igraph::E(graph)$pass <- igraph::E(graph)$pass_d * 
      igraph::E(graph)$pass_u
  }
  if (dir_fragmentation_type == "asymmetric" & index_mode == 
      "to") {
    igraph::E(graph)$pass <- igraph::E(graph)$pass_d
  }
  if (dir_fragmentation_type == "asymmetric" & index_mode == 
      "from") {
    igraph::E(graph)$pass <- igraph::E(graph)$pass_u
  }
  g_v_df <- dplyr::rename_with(igraph::as_data_frame(graph, 
                                                     what = "vertices"), ~"weight_node", contains(weight))
  g_e_df <- igraph::as_data_frame(graph, what = "edges") %>% 
    dplyr::filter(.data$type == "dam") %>% dplyr::mutate(pass = 1 - 
                                                           .data$pass) %>% dplyr::select(.data$from, .data$pass) %>% 
    dplyr::rename(name = .data$from) %>% dplyr::left_join(g_v_df)
  index = sum(g_e_df$pass * g_e_df$weight_node/max(g_v_df$weight_node))
}
return(index)
}


index[[1]][3]
?index_calculation

distances(river_graph,5,2)
distance_table(river_graph, directed = TRUE)
plot(river_graph)

?nel2igraph()


edge_attr(river_graph)

vertex_attr(river_graph)

?get.vertex.attribute

igraph::edge_attr(river_graph)[[1]][2]
igraph::edge_attr(river_graph)[[2]][which(edge_attr(river_graph)$Company == "Sri")]


get.edge.ids(river_graph,c(3,1))
igraph::edge_attr(river_graph)[[6]][get.edge.ids(river_graph,c(3,1))]
igraph::edge_attr(river_graph)[[6]][get.edge.ids(river_graph,c(2,3))]

which(edge_attr(river_graph)$Company == "Sri_dam")
which(edge_attr(river_graph)$Company == "Sri_ph")

get.data.frame(river_graph, what = "edges")
get.data.frame(river_graph, what = "vertices")




install.packages("installr")
library(installr)
updateR()






edges_split = split(edges %>% select(-Company),edges$Company)
dewatered = lapply(edges_split,DewateredNodes)
dewatered = as.numeric(sapply(dewatered, function(x){as.numeric(x[1])}))
dewatered = dewatered[!is.na(dewatered)]

river_net_simplified$length_sq = river_net_simplified$length * river_net_simplified$length
river_net_simplified$DCI = (river_net_simplified$length_sq)/(sum(river_net_simplified$length))^2

index[[1]][3] =   index[[1]][3] - as.numeric(sum(river_net_simplified$DCI[c(dewatered)]))