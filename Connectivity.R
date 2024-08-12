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


#shape_river <- st_read("Nethravathi/Nethravathi_river.shp")
shape_river <- st_read("Nethravathi/Nethravathi_river_v2.shp")
shape_basin <- st_read("Nethravathi/Nethravathi_wshed.shp")
#shape_dams <- st_read("Nethravathi/Nethravathi_SHPs.shp")
shape_dams <- st_read("Nethravathi/Nethravathi_SHPs_v2.shp")
#### shape files processing ####

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
  labs(caption = "Black dots are the position of the dams")

#pruned river network
# Set a threshold of 80 square kilometers
threshold = 10

# Prune HydroRIVERS network based on upstream area
shape_river_small <- shape_river[as.numeric(shape_river$UPLAND_SKM) > threshold,]

ggplot() +
  coord_fixed() +
  theme_minimal() +
  ggspatial::layer_spatial(shape_basin, fill = NA, color = "gray90") +
  #ggspatial::layer_spatial(shape_river_small, aes(color = log10(UPLAND_SKM)) )+
  ggspatial::layer_spatial(shape_dams,color = "red") +
  scale_color_viridis(direction = -1, name= "upstream area \n(log10[Km^2])") +
  theme(legend.position = "bottom") +
  ggspatial::annotation_scale(location = "bl", style = "ticks") +
  ggspatial::annotation_north_arrow(location = "br")+
  labs(caption = "Black dots are the position of the dams")


#### Confluence processing ####

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

ggplot() +
  coord_fixed() +
  ggspatial::layer_spatial(shape_river_simplified, aes(color = id))+
  scale_color_viridis(direction = -1, name= "Reach ID") +
  ggspatial::layer_spatial(river_joins, shape = 1)+
  theme_minimal() +
  theme(legend.position = "bottom")+
  ggspatial::annotation_scale(location = "bl", style = "ticks") +
  ggspatial::annotation_north_arrow(location = "br")+
  labs(caption = "Hollow points are the position of the junctions")

#### Dams processing ####
shape_dams <- shape_dams %>%
  mutate(id = 1:nrow(.))
  

# Snap dams
dams_snapped <- snap_to_river(shape_dams,
                              shape_river_simple %>% st_sf(),
                              max_dist = 1000)

# Retain dams that were snapped. 
dams_snapped_reduced <-
  dams_snapped[st_contains(shape_river_simple %>% st_sf(), dams_snapped, prepared = FALSE)[[1]],]
# something wrong with the above line of code. Not sure why some dams are being dropped even though they all are on the river network.


# Check if dams were snapped properly to the network (all the distances should be zero)
st_distance(dams_snapped_reduced, shape_river_simple) %>% sum
st_distance(dams_snapped, shape_river_simple) %>% sum
## 0 [m]
# using dams_snapped because the sum is 0, i.e., all dams are snapped. 

# this piece of code removes dams that are at the same location, and attaches the river network attributes to the dam points. 
dams_snapped_joined <- dams_snapped_reduced %>%
  mutate(cluster =
           st_equals(dams_snapped_reduced, dams_snapped_reduced) %>%
           sapply(., FUN = function(x) paste0(x, collapse = "_"))) %>%
  group_by(cluster) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(id_dam = as.character(1:nrow(.)), pass_u = 0.1, pass_d = 0.2) %>%
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
  ggspatial::layer_spatial(dams_snapped,color = "black") +
  ggspatial::layer_spatial(dams_snapped_reduced,color = "red") +
  scale_color_viridis(direction = -1, name= "upstream area \n(log10[Km^2])") +
  theme(legend.position = "bottom") +
  ggspatial::annotation_scale(location = "bl", style = "ticks") +
  ggspatial::annotation_north_arrow(location = "br")

headwaters_checking <- headwaters_dam(dams_snapped_joined, shape_river_simple)
head(headwaters_checking$flag_headwater)


ggplot() +
  coord_fixed() +
  ggspatial::layer_spatial(shape_river_simple, color = "gray70")+
  ggspatial::layer_spatial(dams_snapped_joined, shape = 1) +
  theme_minimal() +
  theme(legend.position = "bottom")+
  ggspatial::annotation_north_arrow(location = "br")+
  ggspatial::annotation_scale(location = "bl", style = "ticks") +
  labs(caption = "Hollow points are the position of the dams")


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
  st_join(., shape_river, join = st_is_within_distance, dist = 0.01 ) %>% 
  group_by(NodeID) %>%
  slice(which.max(UPLAND_SKM)) %>% 
  ungroup()


ggplot() +
  coord_fixed() +
  ggspatial::layer_spatial(river_net_simplified, color = "gray70")+
  ggspatial::layer_spatial(network_links, aes(shape = type))+
  scale_shape_manual(name = "Splitting points", values=c("dam" =17,"joint" = 23))+
  theme_minimal() +
  theme(legend.position = "bottom")+
  ggspatial::annotation_north_arrow(location = "br")+
  ggspatial::annotation_scale(location = "bl", style = "ticks")


confluences <- multiple_confluences(river_net_simplified) 
head(confluences)

#st_write(confluences[confluences$flag_confluences == TRUE,], "Nethravathi/confluences.shp")

ggplot() +
  coord_fixed() +
  ggspatial::layer_spatial(river_net_simplified, color = "gray70")+
  ggspatial::layer_spatial(confluences, aes(shape = flag_confluences,color = flag_confluences))+
    theme_minimal() +
  theme(legend.position = "bottom")

shp_check <- check_components(network_links, river_net_simplified)
head(shp_check)

#st_write(shp_check, "Nethravathi/shp_check.shp")

  
ggplot() +
  coord_fixed() +
  ggspatial::layer_spatial(river_net_simplified, color = "gray70")+
  ggspatial::layer_spatial(shp_check, aes(shape = as.factor(component),color = as.factor(component)))+
  theme_minimal() +
  theme(legend.position = "bottom")

?get_elev_raster


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

ggplot() +
  coord_fixed() +
  ggspatial::layer_spatial(river_net_simplified, aes(color = alt))+
  scale_color_viridis(name = "Elevation")+
  theme_minimal() +
  theme(legend.position = "bottom")+
  ggspatial::annotation_north_arrow(location = "br")+
  ggspatial::annotation_scale(location = "bl", style = "ticks")

?geom_spatial_label
?layer_spatial

#st_write(network_links, "Nethravathi/network_links.shp")
#st_write(river_net_simplified, "Nethravathi/river_net_simplified.shp")


#outlet <- get_outlet(river_net_simplified, shape_basin, distance = 1)
outlet <- river_net_simplified$NodeID[river_net_simplified$DIST_DN_KM == 0 ]

river_graph <- create_network(network_links, river_net_simplified, outlet)

plot(river_graph)
?create_network

# Check igraph object
river_graph

# check river_graph edges
igraph::edge_attr(river_graph) %>% names
## [1] "type"       "id_links"   "id_barrier" "pass_u"     "pass_d"

igraph::edge_attr(river_graph)$id_links[24]
igraph::edge_attr(river_graph)$id_barrier[24]

which(igraph::edge_attr(river_graph)$id_barrier == "1")
which(igraph::edge_attr(river_graph)$id_links == 92)

network_links_df <- st_is_within_distance(network_links, river_net_simplified,
                                          dist = 0.01) %>%
  lapply(.,
         FUN = function(x){data.frame("from" = x[1], "to" = x[2], "to2" = x[3]) }) %>%
  do.call(rbind,.) %>%
  cbind(network_links %>% st_drop_geometry())

?graph_from_data_frame

actors <- data.frame(
  name = c(
    "Alice", "Bob", "Cecil", "David",
    "Esmeralda"
  ),
  age = c(48, 33, 45, 34, 21),
  gender = c("F", "M", "F", "M", "F")
)
relations <- data.frame(
  from = c(
    "Bob", "Cecil", "Cecil", "David",
    "David", "Esmeralda"
  ),
  to = c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
  same.dept = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
  friendship = c(4, 5, 5, 2, 1, 1), advice = c(4, 5, 5, 4, 2, 3)
)
g <- graph_from_data_frame(relations, directed = TRUE, vertices = actors)
print(g, e = TRUE, v = TRUE)
plot(g)

# check river_graph vertices
igraph::vertex_attr(river_graph) %>% names
## [1] "name"       "length"     "alt"        "DIST_DN_KM" "UPLAND_SKM"


# update length attribute
V(river_graph)$length <- V(river_graph)$length / 10000
hist(V(river_graph)$length)

# update length attribute
V(river_graph)$name <- as.character(V(river_graph)$name)


# Function for organism that prefers high elevation
suit_fun_high <- function(x){dnorm(x, mean = 1500, sd = 500)*410*sqrt(3*pi)}

# Function for organism that prefers low elevation
suit_fun_low <- function(x){exp(- 0.001*x)}

# Calculate HSI for the igraph nodes
V(river_graph)$HSI_low <- suit_fun_low(V(river_graph)$alt)
V(river_graph)$HSI_high <- suit_fun_high(V(river_graph)$alt)

# Calculate weighted usable length for igraph nodes
V(river_graph)$WUL_low <- V(river_graph)$HSI_low * V(river_graph)$length
V(river_graph)$WUL_high <- V(river_graph)$HSI_high * V(river_graph)$length

# Plot the two response functions
1:10:2500 %>%
  data.frame("Elevation" = .,
             "Low" = suit_fun_low(.), 
             "High" = suit_fun_high(.)) %>%
  pivot_longer(c("Low", "High"), 
               names_to = "Type", values_to = "HSI") %>%
  ggplot() + 
  geom_line(aes(x = Elevation, y = HSI, color = Type))+
  theme_bw() + xlab("Elevation (m)") + ylab("Habitat Suitability Index (HSI)")


# Extract reaches centroids
river_net_simplified_centroids <- river_net_simplified %>%
  st_as_sf() %>%
  st_centroid() 

# get the centroids coordinates
coordinates <- river_net_simplified_centroids %>%
  dplyr::mutate(lat = sf::st_coordinates(.)[,1], lon = sf::st_coordinates(.)[,2]) %>%
  dplyr::select(lat, lon) %>%
  st_set_geometry( NULL)

# fortify the igraph object
gg0 <- ggnetwork(river_graph, layout = coordinates %>% as.matrix(), scale = FALSE)

grid.arrange(
  ggplot(gg0, aes(x = x, y = y, xend = xend, yend = yend)) +
    coord_fixed() +
    geom_nodes(aes(color = HSI_high)) +
    geom_edges(alpha = 0.5) +
    scale_color_viridis()+
    theme_blank()+
    ggtitle("High-altitude organism") +
    labs(caption = "Network directionality not shown"), 
  
  ggplot(gg0, aes(x = x, y = y, xend = xend, yend = yend)) +
    coord_fixed() +
    geom_nodes(aes(color = WUL_high)) +
    geom_edges(alpha = 0.5) +
    scale_color_viridis()+
    theme_blank()+
    ggtitle("High-altitude organism") +
    labs(caption = "Network directionality not shown"), 
  
  ggplot(gg0, aes(x = x, y = y, xend = xend, yend = yend)) +
    coord_fixed() +
    geom_nodes(aes(color = HSI_low)) +
    geom_edges(alpha = 0.5) +
    scale_color_viridis()+
    theme_blank()+
    ggtitle("Low-altitude organism") +
    labs(caption = "Network directionality not shown"),
  
  ggplot(gg0, aes(x = x, y = y, xend = xend, yend = yend)) +
    coord_fixed() +
    geom_nodes(aes(color = WUL_low)) +
    geom_edges(alpha = 0.5) +
    scale_color_viridis()+
    theme_blank()+
    ggtitle("Low-altitude organism") +
    labs(caption = "Network directionality not shown"),
  ncol=2, nrow=2)

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
                                index_type = "sum",
                                index_mode = "from")

?index_calculation

sum(shape_river_simplified$length)/ 10000





#### dissecting index calculation ####
function (graph, weight = "length", nodes_id = "name", index_type = "full", 
          index_mode = "to", c_ij_flag = TRUE, B_ij_flag = TRUE, dir_fragmentation_type = "symmetric", 
          pass_confluence = 1, pass_u = "pass_u", pass_d = "pass_d", 
          field_B = "length", dir_distance_type = "symmetric", disp_type = "exponential", 
          param_u, param_d, param, param_l) 

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
    index_num = t(v_weights) %*% agg_mat %*% v_weights
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


?c_ij_fun
