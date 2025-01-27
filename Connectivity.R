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

setwd("E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/")


#shape_river <- st_read("Nethravathi/Nethravathi_river.shp")
#shape_river <- st_read("Nethravathi/Nethravathi_river_V2.shp")
#shape_basin <- st_read("Nethravathi/Nethravathi_wshed.shp")
#shape_SHPs <- st_read("Nethravathi/Nethravathi_SHPs.shp")
#shape_SHPs_PH <- st_read("Nethravathi/Nethravathi_PH.shp")
#shape_SHPs_new <- st_read("Nethravathi/Nethravathi_SHPs_new.shp")


#shape_river <- st_read("Kaveri/Kaveri_river.shp")
#shape_river <- st_read("Kaveri/Kaveri_river_v2.shp") #confluences removed
#shape_basin <- st_read("Kaveri/Kaveri_sub_basin_Karnataka_wshed.shp")
#shape_SHPs <- st_read("Kaveri/Kaveri_SHPs.shp")
#shape_SHPs_PH <- st_read("Kaveri/Kaveri_PH.shp")
#shape_Large_dams <- st_read("Kaveri/Kaveri_LargeDams.shp")

#shape_river <- st_read("Sharavathi/Sharavathi_river.shp") #confluences removed
#shape_river <- st_read("Sharavathi/Sharavathi_river_v2.shp") #confluences removed
#shape_basin <- st_read("Sharavathi/Sharavathi_wshed.shp")
#shape_SHPs <- st_read("Sharavathi/Sharavathi_SHPs.shp")
#shape_Large_dams <- st_read("Sharavathi/Sharavathi_LargeDams.shp")
#shape_SHPs_PH <- st_read("Sharavathi/Sharavathi_PH.shp")
#shape_SHPs_new <- st_read("Sharavathi/Sharavathi_SHPs_new.shp")

#shape_river <- st_read("Haladi/Haladi_river.shp")
#shape_river <- st_read("Haladi/Haladi_river_v2.shp")
#shape_basin <- st_read("Haladi/Haladi_wshed.shp") 
#shape_SHPs <- st_read("Haladi/Haladi_SHPs.shp")
#shape_Large_dams <- st_read("Haladi/Haladi_LargeDams.shp")
#shape_SHPs_PH <- st_read("Haladi/Haladi_PH.shp")
#shape_SHPs_new <- st_read("Haladi/Haladi_SHPs_new.shp")

#shape_river <- st_read("Suvarna/Suvarna_river.shp")
#shape_river <- st_read("Suvarna/Suvarna_river_v2.shp")
#shape_basin <- st_read("Suvarna/Suvarna_wshed.shp")
#shape_SHPs <- st_read("Suvarna/Suvarna_SHPs.shp")
#shape_SHPs_PH <- st_read("Suvarna/Suvarna_PH.shp")
#shape_SHPs_new <- st_read("Suvarna/Suvarna_SHPs_new.shp")

#shape_river <- st_read("Sita/Sita_river.shp")
#shape_river <- st_read("Sita/Sita_river_v2.shp")
#shape_basin <- st_read("Sita/Sita_wshed.shp")
#shape_SHPs_new <- st_read("Sita/Sita_SHPs_new.shp")

#shape_river <- st_read("Gurupura/Gurupura_river.shp")
#shape_river <- st_read("Gurupura/Gurupura_river_v2.shp")
#shape_basin <- st_read("Gurupura/Gurupura_wshed.shp")
#shape_SHPs <- st_read("Gurupura/Gurupura_SHPs.shp")
#shape_SHPs_PH <- st_read("Gurupura/Gurupura_PH.shp")
#shape_SHPs_new <- st_read("Gurupura/Gurupura_SHPs_new.shp")

#shape_river <- st_read("Shambhavi/Shambhavi_river.shp")
#shape_basin <- st_read("Shambhavi/Shambhavi_wshed.shp")
#shape_SHPs_new <- st_read("Shambhavi/Shambhavi_SHPs_new.shp")


#shape_river <- st_read("Tunga/Tunga_river.shp")
shape_river <- st_read("Tunga/Tunga_river_v2.shp")
shape_basin <- st_read("Tunga/Tunga_wshed.shp")
#shape_SHPs <- st_read("Tunga/Tunga_SHPs.shp")
#shape_Large_dams <- st_read("Tunga/Tunga_LargeDams.shp")
#shape_SHPs_PH <- st_read("Tunga/Tunga_PH.shp")
shape_SHPs_new <- st_read("Tunga/Tunga_SHPs_new.shp")

#shape_river <- st_read("Krishna/Krishna_river.shp")
#shape_river <- st_read("Krishna/Krishna_river_v2.shp")
#shape_river <- st_read("Krishna/Krishna_river_v3.shp")
#shape_basin <- st_read("Krishna/Krishna_wshed.shp")
#shape_basin <- st_read("Krishna/Krishna_wshed_v2.shp")
#shape_SHPs <- st_read("Krishna/Krishna_SHPs.shp")
#shape_Large_dams <- st_read("Krishna/Krishna_LargeDams.shp")
#shape_SHPs_PH <- st_read("Krishna/Krishna_PH.shp")
#shape_SHPs_new <- st_read("Krishna/Krishna_SHPs_new.shp")

#shape_river <- st_read("Bhima/Bhima_river.shp")
#shape_river <- st_read("Bhima/Bhima_river_v2.shp")
#shape_basin <- st_read("Bhima/Bhima_wshed.shp")
#shape_SHPs <- st_read("Bhima/Bhima_SHPs.shp")
#shape_Large_dams <- st_read("Bhima/Bhima_LargeDams.shp")
#shape_SHPs_PH <- st_read("Bhima/Bhima_PH.shp")

#shape_river <- st_read("Kali/Kali_river.shp")
#shape_river <- st_read("Kali/Kali_river_v2.shp")
#shape_basin <- st_read("Kali/Kali_wshed.shp")
#shape_SHPs <- st_read("Kali/Kali_SHPs.shp")
#shape_Large_dams <- st_read("Kali/Kali_LargeDams.shp")
#shape_SHPs_new <- st_read("Kali/Kali_SHPs_new.shp")

#shape_river <- st_read("Gangavali/Gangavali_river.shp")
#shape_basin <- st_read("Gangavali/Gangavali_wshed.shp")
#shape_Large_dams <- st_read("Gangavali/Gangavali_LargeDams.shp")
#shape_SHPs_new <- st_read("Gangavali/Gangavali_SHPs_new.shp")


#shape_river <- st_read("Chakra/Chakra_river.shp")
#shape_river <- st_read("Chakra/Chakra_river_v2.shp")
#shape_basin <- st_read("Chakra/Chakra_wshed.shp")
#shape_Large_dams <- st_read("Chakra/Chakra_LargeDams.shp")
#shape_SHPs_new <- st_read("Chakra/Chakra_SHPs_new.shp")

#shape_river <- st_read("Kolluru/Kolluru_river.shp")
#shape_basin <- st_read("Kolluru/Kolluru_wshed.shp")
#shape_SHPs_new <- st_read("Kolluru/Kolluru_SHPs_new.shp")


#shape_river <- st_read("Sankadagundi/Sankadagundi_river.shp")
#shape_basin <- st_read("Sankadagundi/Sankadagundi_wshed.shp")
#shape_SHPs_new <- st_read("Sankadagundi/Sankadagundi_SHPs_new.shp")

#shape_river <- st_read("Venkatapura/Venkatapura_river.shp")
#shape_basin <- st_read("Venkatapura/Venkatapura_wshed.shp")
#shape_SHPs_new <- st_read("Venkatapura/Venkatapura_SHPs_new.shp")


#shape_river <- st_read("Gundbala/Gundabala_river.shp")
#shape_basin <- st_read("Gundbala/Gundabala_wshed.shp")
#shape_SHPs_new <- st_read("Gundbala/Gundabala_SHPs_new.shp")

#shape_river <- st_read("Aghanashini/Aghanashini_river.shp")
#shape_river <- st_read("Aghanashini/Aghanashini_river_v2.shp")
#shape_basin <- st_read("Aghanashini/Aghanashini_wshed.shp")
#shape_SHPs_new <- st_read("Aghanashini/Aghanashini_SHPs_new.shp")


#shape_river <- st_read("Hattikeri/Hattikeri_river.shp")
#shape_basin <- st_read("Hattikeri/Hattikeri_wshed.shp")
#shape_SHPs_new <- st_read("Hattikeri/Hattikeri_SHPs_new.shp")



# Load the proposed SHP locations to the SHP variable
shape_SHPs = shape_SHPs_new[shape_SHPs_new$Checked == TRUE,]

# remove SHPs on irrigation canals, tank outlets and offshore SHPs and keep only stand-alone (river) and multipurpose SHPs
shape_SHPs = shape_SHPs[shape_SHPs$Sitatued.o == "river" | shape_SHPs$Sitatued.o == "part of bigger project",]

# Add a comment to the powerhouse locations so that we can distinguish them from weir locations later on.
shape_SHPs_PH$Comments = "Powerhouse"
shape_SHPs_PH = shape_SHPs_PH[shape_SHPs_PH$Sitatued.o == "river" | shape_SHPs_PH$Sitatued.o == "part of bigger project" |
                              shape_SHPs_PH$Sitatued.o == "river_non_SHP",]


#combine with other large dams. Create a identified for large dams
shape_Large_dams$Sitatued.o = "river_non_SHP"


#shape_dams = bind_rows(list(shape_SHPs, shape_Large_dams))
#shape_dams = bind_rows(list(shape_SHPs, shape_Large_dams,shape_SHPs_PH))
#shape_dams = bind_rows(list(shape_SHPs, shape_SHPs_PH))
shape_dams = shape_SHPs
#shape_dams = shape_Large_dams

nrow(shape_SHPs)
nrow(shape_Large_dams)
nrow(shape_SHPs_PH)
nrow(shape_SHPs_new)
#### shape files processing ####

ggplot() +
  coord_fixed() +
  theme_minimal() +
  ggspatial::layer_spatial(shape_basin, fill = NA, color = "gray90") +
  ggspatial::layer_spatial(shape_river, aes(color = log10(UPLAND_SKM)) )+
  #ggspatial::layer_spatial(shape_dams,color = "red") +
  #ggspatial::layer_spatial(shape_Large_dams,color = "blue") +
  ggspatial::layer_spatial(shape_dams,color = "orange") +
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
  ggspatial::layer_spatial(shape_river_small, aes(color = log10(UPLAND_SKM)) )+
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

names(shape_dams)
shape_dams$Act_Lat_W_

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
# using dams_snapped because the sum is 0, i.e., all dams are snapped. 

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

#temp = dams_snapped_joined

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

 dams_snapped$id[(which(dams_snapped$id %in% dams_snapped_joined$id == FALSE))]
 dams_snapped$Company[(which(dams_snapped$id %in% dams_snapped_joined$id == FALSE))]
 dams_snapped$SL._No.[(which(dams_snapped$id %in% dams_snapped_joined$id == FALSE))]
 

nrow(shape_SHPs)
nrow(shape_SHPs_PH)
nrow(shape_Large_dams)
nrow(shape_dams)
nrow(dams_snapped_joined)


#st_write(dams_snapped, "Krishna/dams_snapped_reduced.shp",delete_layer = TRUE)
#st_write(shape_river_small, "Sita/shape_river_small.shp",delete_layer = TRUE)
#st_write(dams_snapped_joined, "Krishna/dams_snapped_joined.shp",delete_layer = TRUE)

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

nrow(dams_snapped_joined[dams_snapped_joined$Sitatued.o == "river_non_SHP",]) # large dams
nrow(dams_snapped_joined[dams_snapped_joined$Sitatued.o != "river_non_SHP",]) # SHPs

#all the large dams
DCI_Large = NetworkGenerate(dams_snapped_joined[dams_snapped_joined$Sitatued.o == "river_non_SHP",],shape_river_simple,"Large")

#just the SHP weir
DCI_SHP = NetworkGenerate(dams_snapped_joined[dams_snapped_joined$Sitatued.o != "river_non_SHP" &
                                                (dams_snapped_joined$Comments != "Powerhouse" | 
                                                   is.na(dams_snapped_joined$Comments)),],shape_river_simple,"SHP")

#SHP weir and ph = dewatering
DCI_Dewater = NetworkGenerate(dams_snapped_joined[dams_snapped_joined$Sitatued.o != "river_non_SHP",],shape_river_simple,"Dewater")


#temp = dams_snapped_joined[dams_snapped_joined$Sitatued.o != "river_non_SHP" &
#                             (dams_snapped_joined$Comments != "Powerhouse" | 
#                               is.na(dams_snapped_joined$Comments)),]

# proposed dams
DCI_SHP_new = NetworkGenerate(dams_snapped_joined[dams_snapped_joined$Sitatued.o != "river_non_SHP" &
                                                       (dams_snapped_joined$Comments != "Powerhouse" | 
                                                          is.na(dams_snapped_joined$Comments)),],shape_river_simple,"SHP")


# If there are power houses for large dams, then, for dewatering, we must include large dam locations and their powerhouses too. 
if (nrow(dams_snapped_joined[dams_snapped_joined$Sitatued.o == "river_non_SHP" & # large dams
                           dams_snapped_joined$Comments == "Powerhouse" &  # power house
                           !is.na(dams_snapped_joined$Comments),]) > 0) {
  
  # select rows for which hydropower company has two rows -- 1) dam and 2) powerhouse
  DCI_Dewater = NetworkGenerate((dams_snapped_joined %>% group_by(Company) %>% filter(n() > 1))%>% ungroup(),shape_river_simple,"Dewater")
  
}

temp = dams_snapped_joined[dams_snapped_joined$Sitatued.o == "river_non_SHP",]
temp = dams_snapped_joined[dams_snapped_joined$Sitatued.o != "river_non_SHP",]
temp = dams_snapped_joined %>% group_by(Company) %>% filter(n() > 1) %>% ungroup()



#dams_snapped_joined = dams_snapped_joined %>% group_by(Company) %>% filter(n() > 1) %>% ungroup()
dams_snapped_joined = dams_snapped_joined[dams_snapped_joined$Sitatued.o != "river_non_SHP",]


# This function generates a network link for the set of dams. The dam set could be of different scenarios 1) SHP 2)large 3) dewatered )
NetworkGenerate <- function(dams_snapped_joined,shape_river_simple,type){
  
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
    st_join(., shape_river_small, join = st_is_within_distance, dist = 0.01 ) %>% 
    #st_join(., shape_river_small, join = st_contains) %>% 
    #filter(NodeID == 50) 
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
  
  #st_write(confluences[confluences$flag_confluences == TRUE,], "Krishna/confluences.shp",delete_layer = TRUE)
  
  ggplot() +
    coord_fixed() +
    ggspatial::layer_spatial(river_net_simplified, color = "gray70")+
    ggspatial::layer_spatial(confluences, aes(shape = flag_confluences,color = flag_confluences))+
    theme_minimal() +
    theme(legend.position = "bottom")
  
  shp_check <- check_components(network_links, river_net_simplified)
  head(shp_check)
  
  #st_write(shp_check, "Sita/shp_check.shp")
  #st_write(river_net_simplified,"kaveri/river_net_simplified.shp")
  
  
  ggplot() +
    coord_fixed() +
    ggspatial::layer_spatial(river_net_simplified, color = "gray70")+
    ggspatial::layer_spatial(shp_check, aes(shape = as.factor(component),color = as.factor(component)))+
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # get DEM and transform to data frame with coordinates
  elevation <- get_elev_raster(shape_basin, z = 8)
  ?get_elev_raster
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
  
  
  ggplot() +
    coord_fixed() +
    ggspatial::layer_spatial(river_net_simplified, aes(color = alt))+
    scale_color_viridis(name = "Elevation")+
    theme_minimal() +
    theme(legend.position = "bottom")+
    ggspatial::annotation_north_arrow(location = "br")+
    ggspatial::annotation_scale(location = "bl", style = "ticks")
  
  #st_write(network_links, "Nethravathi/network_links.shp")
  #st_write(river_net_simplified, "Sita/river_net_simplified.shp",delete_layer = TRUE)

  # this won't work because not all rivers drain to the sea. Some are sub-basins
  #outlet <- river_net_simplified$NodeID[river_net_simplified$DIST_DN_KM == 0 ] 
  
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
  
  ggplot() +
    coord_fixed() +
    theme_minimal() +
    ggspatial::layer_spatial(shape_river_small, color = "gray90" )+
    ggspatial::layer_spatial(river_net_simplified[river_net_simplified$NodeID == outlet,],color = "black" )+
    ggspatial::layer_spatial(shape_dams,color = "blue") +
    ggspatial::layer_spatial(dams_snapped,color = "black") +
    ggspatial::layer_spatial(dams_snapped_reduced,color = "red") +
    scale_color_viridis(direction = -1, name= "upstream area \n(log10[Km^2])") +
    theme(legend.position = "bottom") +
    ggspatial::annotation_scale(location = "bl", style = "ticks") +
    ggspatial::annotation_north_arrow(location = "br")
  
  
  ######## create igraph object  ########
  # To avoid issues in the network creation process, retain only the important columns
  river_net_simplified <- river_net_simplified %>% 
    dplyr::select(NodeID, length, alt, DIST_DN_KM, UPLAND_SKM)
  
  river_graph <- create_network(network_links, river_net_simplified, outlet)
  
  plot(river_graph)
  
  river_graph
  
  # check river_graph edges
  igraph::edge_attr(river_graph) %>% names
  ## [1] "type"       "id_links"   "id_barrier" "pass_u"     "pass_d"
  
  # check river_graph vertices
  igraph::vertex_attr(river_graph) %>% names
  ## [1] "name"       "length"     "alt"        "DIST_DN_KM" "UPLAND_SKM"
  
  
  # update length attribute
  V(river_graph)$length <- V(river_graph)$length / 10000
  hist(V(river_graph)$length)
  
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
  
  if(type == "Dewater"){
      edges = get.data.frame(river_graph, what = "edges")
      vertices = get.data.frame(river_graph, what = "vertices")
      
      result = NULL
      #edges_split = split(edges %>% select(-Company),edges$Company,drop=FALSE)
      edges_split = split(edges,edges$Company,drop=FALSE)
      result = lapply(edges_split,DewateredNodes_TributaryFinder,edges = edges)
      
      dewatered = unlist(lapply(result, `[[`, 1), use.names = F)
      dewatered = dewatered[!is.na(dewatered)]
      
      party_dewatered = unlist(lapply(result, `[[`, 2), use.names = F)
      party_dewatered = party_dewatered[!is.na(party_dewatered)]
      
      dwnstream_party_dew = unlist(lapply(result, `[[`, 3), use.names = F)
      dwnstream_party_dew = dwnstream_party_dew[!is.na(dwnstream_party_dew)]
      
      free_trib = unlist(lapply(result, `[[`, 4), use.names = F)
      free_trib = free_trib[!is.na(free_trib)]

      index[[1]] <- index_calculation_dewater(graph = river_graph,
                                              weight = "length",
                                              c_ij_flag = TRUE,
                                              B_ij_flag = FALSE,
                                              index_type = "full",
                                              index_mode = "from",
                                              dewatered_nodes = dewatered,
                                              party_dewatered_nodes = party_dewatered,
                                              dwnstream_party_dew_nodes = dwnstream_party_dew,
                                              free_trib_nodes = free_trib)
      }

  return(index[[1]])
}



