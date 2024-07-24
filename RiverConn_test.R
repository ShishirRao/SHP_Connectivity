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
library("riverconn")
library("shp2graph")

# install remotes package if necessary
#install.packages("remotes")
# install corrmorant from the github repository
#remotes::install_github("r-link/corrmorant")

shape_river <- st_read("riverconn_tutorial-main/Ebro_shape_corrected/Ebro_rivers.shp")
shape_basin <- st_read("riverconn_tutorial-main//Ebro_catchment/Ebro_mask.shp")
shape_dams <- st_read("riverconn_tutorial-main/Ebro_dams/Embalses/Embalses.shp")


#### shape files processing ####
# Set a threshold of 80 square kilometers
threshold = 80

# Prune HydroRIVERS network based on upstream area
shape_river_small <- shape_river[as.numeric(shape_river$UPLAND_SKM) > threshold,]

?st_read


dams_to_points <- shape_dams %>%
  st_as_sf %>%
  st_centroid %>%
  mutate(id = 1:nrow(.))%>% 
  dplyr::select(id)%>% 
  st_transform(crs = "+proj=longlat +datum=WGS84")

ggplot() +
  coord_fixed() +
  theme_minimal() +
  ggspatial::layer_spatial(shape_basin, fill = NA, color = "gray90") +
  ggspatial::layer_spatial(shape_river_small, aes(color = log10(UPLAND_SKM)) )+
  ggspatial::layer_spatial(dams_to_points,color = "red") +
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
# Snap dams
dams_snapped <- snap_to_river(dams_to_points,
                              shape_river_simple %>% st_sf(),
                              max_dist = 1000)
# Retain dams that were snapped
dams_snapped_reduced <-
  dams_snapped[st_contains(shape_river_simple %>% st_sf(), dams_snapped, prepared = FALSE)[[1]],]

# Check if dams were snapped properly to the network (all the distances should be zero)
st_distance(dams_snapped_reduced, shape_river_simple) %>% sum
## 0 [m]


dams_snapped_reduced_joined <- dams_snapped_reduced %>%
  mutate(cluster =
           st_equals(dams_snapped_reduced, dams_snapped_reduced) %>%
           sapply(., FUN = function(x) paste0(x, collapse = "_"))) %>%
  group_by(cluster) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(id_dam = as.character(1:nrow(.)), pass_u = 0.1, pass_d = 0.8) %>%
  as.data.frame %>%
  st_as_sf() %>%
  st_join(., shape_river_small, join = st_is_within_distance, dist = 10 ) %>% 
  group_by(id) %>%
  slice(which.max(UPLAND_SKM)) %>% 
  ungroup()

?st_equals
