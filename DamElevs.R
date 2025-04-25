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
library(directlabels)
library(sf)
library(terra)
library(data.table)

setwd("E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/ShapeFiles/")

# for each basin, extract the basin name, and pass river, dam and wshed shape file to index calculation function
collate <- function(basin_vars){
  
  #basin_vars = g[1][[1]]
  #basin_vars = g[5][[1]]
  
  
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
  
  #if dam files are missing, then return here itself
  if (rlang::is_empty(SHP_file) & rlang::is_empty(LargeDams_file) & rlang::is_empty(SHP_new_file)){
    print(paste(basin_name,"no dams found"))
    return(NULL)  
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
    shape_SHPs$type = "small"
    shape_SHPs$Allotted.C = as.numeric(shape_SHPs$Allotted.C) # required for mering with new SHPs later
  }

  if (!rlang::is_empty(LargeDams_file)){
    shape_Large_dams <- st_read(LargeDams_file)
    #send river, wshed and dame file names to be read and for calculating index
    shape_Large_dams$Sitatued.o = "river_non_SHP"
    for(i in 1:nrow(shape_Large_dams)){
      shape_Large_dams$Company[i] = shape_Large_dams$Comments[i] = paste0("Large Hydro_",i)
    }
    shape_Large_dams$type = "large"
    shape_Large_dams$Allotted = NA
  }

  shape_existing = bind_rows(list(shape_Large_dams,shape_SHPs))
  if (nrow(shape_existing) != 0){
    shape_existing$status = "existing"
  }else{
    shape_existing = NULL  
  }
  
  
  if (!rlang::is_empty(SHP_new_file)){ # if SHP shape file is not empty, then read it
    shape_SHPs_new <- st_read(SHP_new_file)
    # ignore irrigation canal SHPs
    shape_SHPs_new = shape_SHPs_new[shape_SHPs_new$Sitatued.o == "river" | shape_SHPs_new$Sitatued.o == "part of bigger project",]
    shape_SHPs_new$type = "small"
    shape_SHPs_new$status = "proposed"
    shape_SHPs_new$Allotted.C = as.numeric(shape_SHPs_new$Allotted.C) # required for mering with new SHPs later
    shape_Ins_Prop = bind_rows(list(shape_existing,shape_SHPs_new))
    res = elev_calc_wrapper(basin_name,shape_river,shape_Ins_Prop,shape_basin)
    return(res) 
    }else if (nrow(shape_existing) != 0){
      res1 = elev_calc_wrapper(basin_name,shape_river,shape_existing,shape_basin)
      return (res)
    }else{
      return (NULL)
    }
}


# this function takes basin name, river_file, dam_file, wshed_file and type == SHP or large or dewatering
# as inputs and returns network stats as a list
elev_calc_wrapper <- function(name, shape_river, shape_dams, shape_basin){  
  
  #shape_dams = shape_Large_dams
  #shape_dams = shape_SHPs
  #shape_dams = shape_SHPs_PH
  #name = basin_name
  #type = "Dewater"
  #DCI_type = DCId
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
  
  # Identify the dam at the lowest elevation. This is for DCId
  Dam_loc = tidyr::extract(dams_snapped_joined, geometry, into = c('Lon', 'Lat'), '\\((.*),(.*)\\)', conv = T)
  Dam_loc = data.frame(x = Dam_loc$Lon, y = Dam_loc$Lat)
  Dam_elevs = get_elev_point(locations =Dam_loc, units='meters', prj="EPSG:4326", src='aws',z=10)
  dams_snapped_joined$Basn_nm = name
  dams_snapped_joined = cbind(dams_snapped_joined,Dam_elevs)
  names(dams_snapped_joined)
  dams_snapped_joined = dams_snapped_joined %>% dplyr::select(Basn_nm,Company,type,status,Sitatued.o,elevation,elev_units)
  return(dams_snapped_joined)
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

# Diadromous: river to sea connectivity
listofres = NULL
#call function to loop through each basin calculating DCId
listofres = lapply(g,collate)
out.df <- (do.call("rbind", listofres))
out.df <- out.df %>% `rownames<-`(seq_len(nrow(out.df)))
#saveRDS(out.df, file = "dam_elev.rds")
#out.df = readRDS("dam_elev.rds")
out.df$type[out.df$Sitatued.o == "part of bigger project"] = "large"

out.df = as.data.frame(out.df)

dam_elev = out.df %>% dplyr::group_by(Basn_nm,type,status) %>% 
          dplyr::summarize(dam_elev_max = max(elevation, na.rm=TRUE),
                           dam_elev_min = min(elevation, na.rm=TRUE))

dam_no = out.df %>% dplyr::group_by(Basn_nm,type,status) %>% 
  dplyr::summarize(n = n())

dam_elev$range = stri_paste(dam_elev$dam_elev_max,' ','-',' ',dam_elev$dam_elev_min)

dam_elev = dam_elev %>% select(Basn_nm,type,status,range)
names(dam_elev)

dam_elev_wide = NULL
dam_elev_wide = dam_elev %>% spread(key = type, value = range)
dam_elev_wide$large[is.na(dam_elev_wide$large)] = "-"
dam_elev_wide$small[is.na(dam_elev_wide$small)] = "-"

dam_elev_wide$Status = dam_elev_wide$status
dam_elev_wide = dam_elev_wide %>% spread(key = status, value = large)

names(dam_elev_wide) = c("Basn_nm","small","Status","large_existing_elev_range","large_proposed_elev_range")

dam_elev_wide = dam_elev_wide %>% spread(key = Status, value = small)
names(dam_elev_wide) = c("Basn_nm","large_existing_elev_range","large_proposed_elev_range",
                         "small_existing_elev_range","small_proposed_elev_range")

dam_elev_wide = dam_elev_wide %>% gather(var, val, large_existing_elev_range:small_proposed_elev_range) %>%  filter(!is.na(val)) %>% 
                spread(key = var, value = val)

#### now deal with number of dams
dam_no_wide = dam_no %>% spread(key = type, value = n)
names(dam_no_wide)
dam_no_wide$Status = dam_no_wide$status

dam_no_wide = dam_no_wide %>% spread(key = status, value = large)
names(dam_no_wide) = c("Basn_nm","small","Status","large_existing_no","large_proposed_no")

dam_no_wide = dam_no_wide %>% spread(key = Status, value = small)
names(dam_no_wide) = c("Basn_nm","large_existing_no","large_proposed_no","small_existing_no","small_proposed_no")

dam_no_wide = dam_no_wide %>% gather(var, val, large_existing_no:small_proposed_no) %>%  filter(!is.na(val)) %>% 
  spread(key = var, value = val)

class(dam_elev_wide$small_proposed_elev_range)

##############
#Combine dam elevation and dam number data




                        



