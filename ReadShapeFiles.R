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


# for each basin, extract the basin name, and pass river, dam and wshed shape file to index calculation function
collate <- function(basin_vars){
  
  #basin_vars = g[1][[1]]
  basin_name <- sub("(.+?)(\\_.*)", "\\1", basin_vars[1])
  print(basin_name)
  
  #initialize file name variables
  river_file = wshed_file = SHP_file = SHP_PH_file = LargeDams_file = SHP_new_file = character(0)
  
  # parse through file names to detect river, wshed and dam files
  river_file = basin_vars[which(as.numeric(grepl('river', basin_vars)) == 1)]
  wshed_file = basin_vars[which(as.numeric(grepl('wshed', basin_vars)) == 1)]
  
  SHP_file = basin_vars[which(as.numeric(grepl('SHPs', basin_vars)) == 1)]
  if(!rlang::is_empty(SHP_file)){
    if (length(SHP_file) == 2){ # implies both existing and new SHPs are present
      SHP_file = SHP_file[1]  # load only existing SHPs
    } else if (length(SHP_file) == 1 & grepl('new', SHP_file)){ # only new SHPs are present
      SHP_file = character(0)
    } 
  }
  
  if(basin_name != "Sharavathi"){
    SHP_PH_file = basin_vars[which(as.numeric(grepl('PH', basin_vars)) == 1)]
  }
  
  LargeDams_file = basin_vars[which(as.numeric(grepl('LargeDams', basin_vars)) == 1)]
  SHP_new_file = basin_vars[which(as.numeric(grepl('new', basin_vars)) == 1)]
  
  #data.frame(name = c("A1","A2"), index = c(as.numeric(1),as.numeric(2)),type = c("SHPs","LargeDams"))
  
  #if dam files are missing, then return here itself
  if (rlang::is_empty(SHP_file) & rlang::is_empty(LargeDams_file) & rlang::is_empty(SHP_new_file)){
    print(paste(basin_name,"no dams found"))
    return(data.frame(name = c(basin_name,basin_name,basin_name),
                      index = c(as.numeric(1),as.numeric(1),as.numeric(1),as.numeric(1),as.numeric(1),as.numeric(1)),
                      type = c("SHPs","LargeDams","Dewater","Existing_total","SHPs_new","Existing_Proposed_total")))  
  }
  
  # read shape files
  shape_river <- st_read(river_file) 
  shape_basin <- st_read(wshed_file)
  
  shape_SHPs = shape_SHPs_PH = shape_Large_dams = shape_existing = shape_SHPs_new = NULL
  
  #clear the results variable
  res1 = res2 = res3 = res4 = res5 = res6 = NULL
  
  if (!rlang::is_empty(SHP_file)){ # if SHP shape file is not empty, then read it
    shape_SHPs <- st_read(SHP_file)
    # ignore irrigation canal SHPs
    shape_SHPs = shape_SHPs[shape_SHPs$Sitatued.o == "river" | shape_SHPs$Sitatued.o == "part of bigger project",]
    res1 = index_calc_wrapper(basin_name,shape_river,shape_SHPs,shape_basin,"SHP")
    #res1 = data.frame(basin_name,index = 20)
    res1 = cbind(res1,type = "SHPs")
    print(res1)
  }
  
  if (!rlang::is_empty(SHP_PH_file)){ # if PH shape file is not empty, then read it
    shape_SHPs_PH <- st_read(SHP_PH_file)
    shape_SHPs_PH$Comments = "Powerhouse"
    shape_SHPs_PH = shape_SHPs_PH[shape_SHPs_PH$Sitatued.o == "river" | shape_SHPs_PH$Sitatued.o == "part of bigger project",]
    shape_SHPs_PH = bind_rows(list(shape_SHPs,shape_SHPs_PH))
    res2 = index_calc_wrapper(basin_name,shape_river,shape_SHPs_PH,shape_basin,"Dewater")
    res2 = cbind(res2,type = "Dewater")
    print(res2)
  }
  
  if (!rlang::is_empty(LargeDams_file)){
    shape_Large_dams <- st_read(LargeDams_file)
    #send river, wshed and dame file names to be read and for calculating index
    shape_Large_dams$Sitatued.o = "river_non_SHP"
    for(i in 1:nrow(shape_Large_dams)){
      shape_Large_dams$Company[i] = shape_Large_dams$Comments[i] = paste0("Large Hydro_",i)
    }
    res3 = index_calc_wrapper(basin_name,shape_river,shape_Large_dams,shape_basin,"Large")
    res3 = cbind(res3,type = "LargeDams")
    print(res3)
  }
  
  if(!is.null(res1) & !is.null(res2) & !is.null(res3)){
    shape_existing = bind_rows(list(shape_Large_dams,shape_SHPs,shape_SHPs_PH))
    shape_existing$Allotted.C = as.numeric(shape_existing$Allotted.C) # required for mering with new SHPs later
    res4 = index_calc_wrapper(basin_name,shape_river,shape_existing,shape_basin,"Existing_total")
    res4 = cbind(res4,type = "Existing_total")
    print(res4)
  } else if(!is.null(res2) & is.null(res3)){
    shape_existing = shape_SHPs_PH
    shape_existing$Allotted.C = as.numeric(shape_existing$Allotted.C) # required for mering with new SHPs later
    res4 = res2[,-3] # if large dams are missing but SHPs are present then existing total = dewatered
    res4 = cbind(res4,type = "Existing_total")
  } else if(is.null(res2) & !is.null(res3)){
    shape_existing = shape_Large_dams
    res4 = res3[,-3] # if SHPs are missing but large dams exist then existing total = large dams
    res4 = cbind(res4,type = "Existing_total")
  }
  
  if (!rlang::is_empty(SHP_new_file)){ # if SHP shape file is not empty, then read it
    shape_SHPs_new <- st_read(SHP_new_file)
    # ignore irrigation canal SHPs
    shape_SHPs_new = shape_SHPs_new[shape_SHPs_new$Sitatued.o == "river" | shape_SHPs_new$Sitatued.o == "part of bigger project",]
    res5 = index_calc_wrapper(basin_name,shape_river,shape_SHPs_new,shape_basin,"new")
    #res1 = data.frame(basin_name,index = 20)
    res5 = cbind(res5,type = "SHPs_new")
    print(res5)
  }
  
  if(!is.null(res4) & !is.null(res5)){
    #shape_existing$Allotted.C = as.numeric(shape_existing$Allotted.C)
    shape_Ins_Prop = bind_rows(list(shape_existing,shape_SHPs_new))
    res6 = index_calc_wrapper(basin_name,shape_river,shape_Ins_Prop,shape_basin,"Existing_Proposed_total")
    res6 = cbind(res6,type = "Existing_Proposed_total")
    print(res6)
  } else if (is.null(res4) & !is.null(res5)){ #existing total missing but new SHPs
    res6 = res5[,-3]
    res6 = cbind(res6,type = "Existing_Proposed_total")
  } else if (!is.null(res4) & is.null(res5)){ # existing total but no new SHPs
    res6 = res4[,-3]
    res6 = cbind(res6,type = "Existing_Proposed_total")
  }
  
  return(rbind(res1,res2,res3,res4,res5,res6))
}






# this function takes basin name, river_file, dam_file, wshed_file and type == SHP or large or dewatering
# as inputs and returns network stats as a list
index_calc_wrapper <- function(name, shape_river, shape_dams, shape_basin,type){  
  
  #shape_dams = shape_Large_dams
  #shape_dams = shape_SHPs
  #shape_dams = shape_SHPs_PH
  #name = basin_name
  #type = "Dewater"
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
  
  if(type == "Dewater" | type == "Existing_total"){
    edges = get.data.frame(river_graph, what = "edges")
    vertices = get.data.frame(river_graph, what = "vertices")
    
    result = NULL
    #edges_split = split(edges %>% select(-Company),edges$Company,drop=FALSE)
    edges_split = split(edges,edges$Company,drop=FALSE)
    result = lapply(edges_split,DewateredNodes_TributaryFinder,edges = edges)
    
    print("result")
    print(result)

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
                                            free_trib_nodes = free_trib,
                                            shape_river_ref = shape_river,
                                            river_net_simplified_ref = river_net_simplified)
  }
    
  return(data.frame(name = name, DCIp = index[[1]][3]))
}

#read shapefile, make a list of file names grouped basin-wise
setwd("E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/ShapeFiles/")

#Read all the shapefiles
filenames <- list.files(pattern="*.shp", full.names=FALSE)
#This expression seperates the basin name from _wshed, _river, or _SHP of the file name
g <- sub("(.+?)(\\_.*)", "\\1", filenames)
g <- split(filenames, g)
g_df = as.data.frame(names(g))
g_df

g[30]

listofres = NULL
#call function to loop through each basin calculating DCI
listofres = lapply(g,collate)
#listofres = lapply(g[1],collate)
#Tunga = NULL
#Tunga = lapply(g[30],collate)
out.df <- (do.call("rbind", listofres))
out.df <- out.df %>% `rownames<-`(seq_len(nrow(out.df)))
names(out.df) <- c("Basin_name","DCIp","Type")

#Tunga = as.data.frame(Tunga)
#names(Tunga) <- c("Basin_name","DCIp","Type","Direction")

#out.df = rbind(out.df, Tunga)

out.df$Direction = "West"
out.df$Direction[out.df$Basin_name == "Bhima" |
                 out.df$Basin_name == "Kaveri" |
                 out.df$Basin_name == "Krishna" |
                 out.df$Basin_name == "Tunga" |
                 out.df$Basin_name == "Palar" |
                out.df$Basin_name == "NorthPennar" |
                  out.df$Basin_name == "SouthPennar"] = "East"

out.df.wide = spread(out.df, key = Type, value = DCIp)

out.df.wide[is.na(out.df.wide)] <- 100
#Only for Sharavathi, dewater is NA
out.df.wide$Dewater[out.df.wide$Basin_name == "Sharavathi"] <- NA

range(out.df.wide$LargeDams)
range(out.df.wide$SHPs)
range(out.df.wide$Dewater,na.rm = TRUE)
range(out.df.wide$Existing_total)
range(out.df.wide$SHPs_new)
range(out.df.wide$Existing_Proposed_total)

#write.csv(out.df.wide, "E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/Basins/DCI_v6.csv")


# prepare the output for display
#read only the wsheds
wshednames <- list.files(pattern="*wshed.shp", full.names=FALSE)
wshed_shp_files <- lapply(wshednames, read_sf)
basin_names <- sub("(.+?)(\\_.*)", "\\1", wshednames)
wsheds <-bind_rows(wshed_shp_files)
wsheds$Basin_name = basin_names

class(wsheds)

st_write(wsheds, "E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/Basins/Basins.shp", delete_layer = TRUE)

wsheds = left_join(wsheds,out.df.wide)
#st_write(wsheds, "E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/Basins/Results_DCI_v6.shp", delete_layer = TRUE)
#write.csv(out.df, "E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/Basins/DCI_v5.csv")


LargeDamNames = list.files(pattern="*LargeDams.shp", full.names=FALSE)
LargeDams_shp_files <- lapply(LargeDamNames, read_sf)
LargeDams <- bind_rows(LargeDams_shp_files)
st_write(LargeDams, "E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/Basins/LargeDams_shp_files.shp", delete_layer = TRUE)

SHPNames = list.files(pattern="*SHPs.shp", full.names=FALSE)
SHPNames_shp_files <- lapply(SHPNames, read_sf)
SHPs <- bind_rows(SHPNames_shp_files)
st_write(SHPs, "E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/Basins/SHPs_shp_files.shp", delete_layer = TRUE)

PHNames = list.files(pattern="*PH.shp", full.names=FALSE)
PHNames_shp_files <- lapply(PHNames, read_sf)
PHs <- bind_rows(PHNames_shp_files)
st_write(PHs, "E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/Basins/PHs_shp_files.shp", delete_layer = TRUE)

Existing = bind_rows(cbind(LargeDams,type="Large"),cbind(SHPs,type="SHPs"),cbind(PHs,type="PHs"))
st_write(Existing, "E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/Basins/Existing_shp_files.shp", delete_layer = TRUE)

SHP_new_Names = list.files(pattern="*new.shp", full.names=FALSE)
SHP_new_Names_shp_files <- lapply(SHP_new_Names, read_sf)
SHPs_new <- bind_rows(SHP_new_Names_shp_files)
st_write(SHPs_new, "E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/Basins/SHPs_new_shp_files.shp", delete_layer = TRUE)


# Plotting using ggplots
ggplot(out.df,aes(x=Type,y=DCIp))+geom_point(aes(color = Basin_name))+
  geom_line(aes(group = Basin_name,color = Basin_name))

ggplot(out.df,aes(x=Type,y=DCIp))+geom_point(aes(color = Basin_name))+
  geom_line(aes(group = Direction,color = Direction))


+facet_wrap(.~Basin_name)



# creating fish species richness shape file            
wshednames <- list.files(pattern="*wshed.shp", full.names=FALSE)
wshed_shp_files <- lapply(wshednames, read_sf)
basin_names <- sub("(.+?)(\\_.*)", "\\1", wshednames)
wsheds <-bind_rows(wshed_shp_files)
wsheds$Basin_name = basin_names


#' @param dsn Path to one shapefile with multiple polygons or a list of files
#' @param id \code{character} Character specifying the id column
#' @param touches \code{logical} If TRUE, all cells touched by lines or polygons are affected, 
#' not just those on the line render path, or whose center point is within the polygon. 
#' @param resolution \code{integer} Resolution in degrees
#' @param save \code{logical} Should individual output be stored. Path to output folder can be specified under path 
#' @param extent \code{extent} Extent of area that we are interested in.
#' @param split \code{character} default is NA
#' @param name_split \code{integer} Specifies which splits to use, default is c(1,2).
#' @param seasonal \code{integer} 1 = Resident, 2 = Breeding Season, 3 = Non-breeding Season, 4 = Passage, 5 =	Seasonal occurence uncertain.
#' @param origin \code{integer} 1 = Native, 2 =	Reintroduced, 3 =	Introduced, 4 =	Vagrant, 5 = Origin Uncertain.
#' @param presence \code{integer} 1 =	Extant, 2 = Probably Extant, 3 = Possibly Extant, 4 = Possibly Extinct, 5 = Extinct (post 1500), 6 = Presence Uncertain.
#' @param getCover \code{logical} Calculate the percentage covered by a polygon rather than the presence of a species
#' @param df \code{logical} Store the output as data.frame or not. If df=FALSE output will be stored as .tif files.
#' @param crs \code{character} Define the output projection of your data.
#' @param path \code{character} Path where individual output files are going to be stored.
#' @return list of raster layers for each \code{id} with the given area \code{shapefile}
#' @examples


# download the IUCN fish ranges that overlap with the study basins
Fish_rich = st_read("E:/Shishir/Thesis_GIS/FW_FISH/FW_FISH_DCI_intersect.shp")
names(Fish_rich)
Fish_rich = as.data.frame(Fish_rich %>% dplyr::select("Basn_nm","id_no","sci_name",
                                                      "presence","origin","seasonal",
                                                      "compiler","subspecies",
                                                      "marine","freshwater","category"))

unique(Fish_rich$category)

Fish_rich$estuarine = ifelse(Fish_rich$marine == "true" & Fish_rich$freshwater == "true","true","false")

Fish_rich_count = Fish_rich %>% dplyr::group_by(Basn_nm) %>% summarise(total_rich=n())
Fish_rich_freshwater = Fish_rich %>% dplyr::group_by(Basn_nm) %>% filter(freshwater == "true" & marine == "false") %>% summarise(fw_rich=n())
Fish_rich_estuarine = Fish_rich %>% dplyr::group_by(Basn_nm) %>% filter(estuarine == "true") %>% summarise(est_rich=n())    
Fish_IUCN_CR = Fish_rich %>% dplyr::group_by(Basn_nm)  %>% filter(category == "CR") %>% summarise(CR=n())
Fish_IUCN_VU = Fish_rich %>% dplyr::group_by(Basn_nm) %>% filter(category == "VU") %>% summarise(VR=n())
Fish_IUCN_EN = Fish_rich %>% dplyr::group_by(Basn_nm) %>% filter(category == "EN") %>% summarise(EN=n())    



Fish_rich_summary = left_join(Fish_rich_count,Fish_rich_freshwater)
Fish_rich_summary = left_join(Fish_rich_summary,Fish_rich_estuarine)
Fish_rich_summary = left_join(Fish_rich_summary,Fish_IUCN_CR)
Fish_rich_summary = left_join(Fish_rich_summary,Fish_IUCN_VU)
Fish_rich_summary = left_join(Fish_rich_summary,Fish_IUCN_EN)

names(Fish_rich_summary) 
#write.csv(Fish_rich_summary,"Fish_rich_summary.csv")

getwd()




#Fish_rich = read.csv("E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/Fish_rich_summary.csv",header=T)
names(Fish_rich_summary)[1] = "Basin_name"
Fish_rich_wsheds = left_join(wsheds,Fish_rich_summary)

#st_write(Fish_rich_wsheds, "E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/Basins/Fish_rich_wsheds_v2.shp", delete_layer = TRUE)
