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


len = seq(from=0, to=1000,by = 50)

DCIp = ((100*((len)/1000)^2)+(100*((1000-len)/1000)^2))
DCIpInv = 100 - DCIp 

df = data.frame(len=len,DCIp = DCIp)

plot(DCIpInv ~ len)

DCIdInv = 100 - (100*((len)/1000))

plot(DCIdInv ~ len)

Area = seq(from=0, to=10000,by = 500)

CAFI =  100 *(10000-Area/10000)

plot(CAFI ~ Area)

c11 = c22 = 1
c12 = c21 = 0.5
l1 = 5
l2 = 25
L = l1 + l2

C= c11 *  (l1/L)^2 + c12*(l1*l2/L^2) + c21*(l1*l2)/L^2 + c22*(l2/L)^2


c11 = c22 = 1
c12 = c21 = 0.5
l1 = 10
l2 = 20
L = l1 + l2

DCIp= c11 *  (l1/L)^2 + c12*(l1*l2/L^2) + c21*(l1*l2)/L^2 + c22*(l2/L)^2

c1 = 0.5
c2 = 1
DCId = c1 * l1/L + c2 * l2/L


c11  = c33 = 1
c22 = 0

c12 = c21 = 0
c23 = c32 = 0
c13 = c31 = 0

l1 = 5
l2 = 5
l3 = 20

L = l1 + l2 + l3

C = c11*(l1/L)^2 + c12*(l1*l2/L^2) + c13*(l1*l3/L^2)+
  c21*(l2*l1)/L^2 + c22*(l2/L)^2 + c23*(l2*l3/L^2)+
  c31*(l3*l1)/L^2 + c32*(l3*l2/L^2)+ c33*(l3/L)^2

l2 = 0



#shape_river <- st_read("test/TestRiver.shp")
shape_river <- st_read("test/TestRiver_split.shp")
#shape_river <- st_read("test/TestRiver_split_v3.shp")
shape_basin <- st_read("test/TestBasin.shp")
shape_dams <- st_read("test/TestPoints.shp")
#shape_dams <- st_read("test/TestPoints_v3.shp")

#Let us filter to just one dam
shape_dams <- shape_dams[shape_dams$DamId =='A' | shape_dams$DamId =='B' | shape_dams$DamId =='D',]
#shape_dams <- shape_dams[shape_dams$DamId =='A' | shape_dams$DamId =='D',]

#shape_river <- shape_river[-23,]



ggplot() +
  coord_fixed() +
  theme_minimal() +
  ggspatial::layer_spatial(shape_basin, fill = NA, color = "gray90") +
  ggspatial::layer_spatial(shape_river, color = "blue")+
  ggspatial::layer_spatial(shape_dams,color = "red") +
    theme(legend.position = "bottom") +
  ggspatial::annotation_scale(location = "bl", style = "ticks") +
  ggspatial::annotation_north_arrow(location = "br")+
  labs(caption = "Black dots are the position of the dams")


# Prune HydroRIVERS network based on upstream area
shape_river_small <- shape_river


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
                              max_dist = 10000)

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
  mutate(id_dam = as.character(1:nrow(.)), pass_u = 0, pass_d = 0) %>%
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
    dplyr::select(type, id_barrier, pass_u, pass_d,Company,Comments),
  river_joins %>% mutate(type = "joint") %>%
    dplyr::select(type) %>%
    mutate(id_barrier = NA, pass_u = NA, pass_d = NA,Company = NA,Comments = NA) %>%
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

#river_net_simplified$length[river_net_simplified$NodeID == 2] = 50
#river_net_simplified$length[river_net_simplified$NodeID == 1] = 75
#river_net_simplified$length[river_net_simplified$NodeID == 3] = 125
#river_net_simplified$length[river_net_simplified$NodeID == 4] = 750



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


ggplot() +
  coord_fixed() +
  ggspatial::layer_spatial(river_net_simplified, color = "gray70")+
  ggspatial::layer_spatial(confluences, aes(shape = flag_confluences,color = flag_confluences))+
  theme_minimal() +
  theme(legend.position = "bottom")

shp_check <- check_components(network_links, river_net_simplified)
head(shp_check)



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
#river_net_simplified <- river_net_simplified %>% 
#  dplyr::select(NodeID, length, alt, DIST_DN_KM, UPLAND_SKM)

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



#river_net_simplified$DIST_DN_KM[river_net_simplified$NodeID == 5] = 0

#st_write(river_net_simplified, "test/river_net_simplified5.shp")


#outlet <- get_outlet(river_net_simplified, shape_basin, distance = 1)
outlet <- river_net_simplified$NodeID[river_net_simplified$DIST_DN_KM == 0 ]

river_graph <- create_network(network_links, river_net_simplified, outlet)

plot(river_graph)
summary(river_graph)
?create_network

# Check igraph object
river_graph

# check river_graph edges
igraph::edge_attr(river_graph) %>% names
## [1] "type"       "id_links"   "id_barrier" "pass_u"     "pass_d"

# check river_graph vertices
igraph::vertex_attr(river_graph) %>% names
## [1] "name"       "length"     "alt"        "DIST_DN_KM" "UPLAND_SKM"

# update length attribute
V(river_graph)$length <- V(river_graph)$length / 10
#hist(V(river_graph)$length)

# update length attribute
V(river_graph)$name <- as.character(V(river_graph)$name)



# Initialize list where to store all the index calculation outputs
index <- list()
lab_index <- list()
letter_index <- list()
?index_calculation

# 1: Symmetric Dendritic Connectivity Index (no biotic effects)
lab_index[[1]] <- "Symmetric DCI"
letter_index[[1]] <- "A"
index[[1]] <- index_calculation(graph = river_graph,
                                weight = "length",
                                c_ij_flag = TRUE,
                                B_ij_flag = FALSE,
                                index_type = "full",
                                index_mode = "from")


edges = get.data.frame(river_graph, what = "edges")
vertices = get.data.frame(river_graph, what = "vertices")


#create a blank category
edges_split = split(edges %>% select(-Company),edges$Company,drop=FALSE)
result = lapply(edges_split,DewateredNodes_TributaryFinder)

dewatered = unlist(lapply(result, `[[`, 1), use.names = F)
dewatered = dewatered[!is.na(dewatered)]

party_dewatered = unlist(lapply(result, `[[`, 2), use.names = F)
party_dewatered = party_dewatered[!is.na(party_dewatered)]

dwnstream_party_dew = unlist(lapply(result, `[[`, 3), use.names = F)
dwnstream_party_dew = dwnstream_party_dew[!is.na(dwnstream_party_dew)]

free_trib = unlist(lapply(result, `[[`, 4), use.names = F)
free_trib = free_trib[!is.na(free_trib)]

river_net_simplified$length_sq = river_net_simplified$length * river_net_simplified$length
river_net_simplified$DCI = (river_net_simplified$length_sq)/(sum(river_net_simplified$length))^2

DCI_withoutDewater = sum(river_net_simplified$length[c(1,4,5,7,8)])^2 / (sum(river_net_simplified$length))^2 + 
  sum(river_net_simplified$DCI[c(2,3,6)])

DCI_Dewater1 = sum(river_net_simplified$length[c(1,4,5,7,8)])^2 / (sum(river_net_simplified$length))^2 + 
  sum(river_net_simplified$DCI[c(2,6)])

DCI_Dewater2 = as.numeric(DCI_withoutDewater) - as.numeric(sum(river_net_simplified$DCI[c(dewatered)]))


index[[1]] <- index_calculation_dewater(graph = river_graph,
                                weight = "length",
                                c_ij_flag = TRUE,
                                B_ij_flag = FALSE,
                                index_type = "full",
                                index_mode = "from")

## for the tributary in between dam and power house
# this is how the RiverConn calculates
sum(river_net_simplified$length[c(3,5,7)])^2 / (sum(river_net_simplified$length))^2 + 
#isolated dam segments
sum(river_net_simplified$DCI[c(2,6)])+
#segments in between dams / ph but has a tributary
sum(river_net_simplified$length[c(1,4,8)])^2 / (sum(river_net_simplified$length))^2

#This is what we want
# 1. find out what flows in to the dewatered stretch. That is the id link of the tributary

#connected free segments
sum(river_net_simplified$length[c(3,5,7,4,8)])^2 / (sum(river_net_simplified$length))^2 + 
# isolated dam segments
sum(river_net_simplified$DCI[c(2,6)])
# fully dewatered stretch is 1 which we ignore
# the only missing component is introducing reduced flow in stretch 4. 



  

  
# A function that returns the dewatered nodes for each SHP company
DewateredNodes_TributaryFinder = function(vars){
    
    #vars = edges_split[[2]]
    # each SHP company should have a weir and Ph location, i.e it has to have two rows. If not, there 
    # isn't a dewatered stretch
    dewatered = as.numeric(NA) # initialization
    tributary = as.numeric(NA)
    partly_dewatered = as.numeric(NA)
    dwnstream_party_dew = as.numeric(NA)
    
    if(nrow(vars) == 2){
      dewatered = as.numeric(c(vars$from,vars$to))
      dewatered = dewatered[duplicated(dewatered)]
      
      # if there isn't a common node between dam and the ph, it means there is a tributary joining.
      # in case send both the from and to nodes back to the calling function
      if(length(dewatered) == 0){
        # this is the stretch immediately downstream of the dam
        dewatered = as.numeric(vars$to[which(vars$Comments != "Powerhouse" | is.na(vars$Comments))])
        
        #this is the part of dewatered stretch downstream of the tributary joining
        partly_dewatered = as.numeric(vars$from[which(vars$Comments == "Powerhouse")])
        
        dwnstream_party_dew = as.numeric(vars$to[which(vars$Comments == "Powerhouse")])
        
        # the tributary joining the dewatered stretch
        tributary = as.numeric(edges$from[which(edges$to == (edges$from[which(edges$Comments == "Powerhouse")]))])
        tributary = tributary[!(tributary %in% dewatered)]
        }
    }
    return(data.frame(dewatered = dewatered,partly_dewatered = partly_dewatered,
                      dwnstream_party_dew = dwnstream_party_dew,free_trib = tributary))
  }



