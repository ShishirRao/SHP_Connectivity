#libraries
packages <- c("tidyverse","DiagrammeR","ggplot2","gridExtra","ggpubr",
              "patchwork")
lapply(packages, require, character.only = T)

source("Jumani_et_al_FragmentationIndex_Functions.R")

# Load simulated data
data <- read.csv("Jumani_et_al_FragmentationIndex_simulation_data.csv")

#############Simulation 1######################

#Calculating index values for sequential addition of dams
#only simulated data needed

#---------------DCIp - river length------------------
test.dcip.sim1 <- data %>%
    mutate(frac = ((Uslth_cum^2)+(TOTlth-Uslth_cum)^2)/(TOTlth^2)) %>%
    distinct(DamID, .keep_all = TRUE) %>%
    group_by(DamID) %>%
    summarise(DCIp = (sum(frac)*100)) #As DCI increases, connectivity increases
data$DCIp.sim1 <- test.dcip.sim1$DCIp[match(data$DamID, test.dcip.sim1$DamID)]

#---------------DCId - river length------------------
test.dcid.sim1 <- data %>%
  mutate(frac = (DistMouth.km./TOTlth)) %>%
  distinct(DamID, .keep_all = TRUE) %>%
  group_by(DamID) %>%
  summarise(DCId = (sum(frac))*100) #As DCI increases, connectivity increases
data$DCId.sim1 <- test.dcid.sim1$DCId[match(data$DamID, test.dcid.sim1$DamID)]

#---------------CAFI - cumulative watershed area------------------
#here CAFI is calculated with range of barrier passibility 1,0.75,0.5,0.25
test.cafi.sim1 <- data %>%
  distinct(DamID, .keep_all = TRUE) %>%
  group_by(DamID) %>%
  summarise(CAFI1_sim1 = sum(USarea_cum*1/TOTarea)*100, CAFI0.75_sim1 = sum(USarea_cum*0.75/TOTarea)*100,
            CAFI0.5_sim1 =sum(USarea_cum*0.5/TOTarea)*100, CAFI0.25_sim1 = sum(USarea_cum*0.25/TOTarea)*100) %>%   #As CAFI increases, connectivity decreases
  data.frame()
data <- left_join(data,test.cafi.sim1, by = "DamID")

#---------------CARFI - cumulative watershed area & rainfall---------------
test.carfi.sim1 <- data %>%
  distinct(DamID, .keep_all = TRUE) %>%
  group_by(DamID) %>%
  summarise(CARFI1_sim1 = sum((USarea_cum*USrain_cum*1)/(TOTarea*TOTRain))*100, 
            CARFI0.75_sim1= sum((USarea_cum*USrain_cum*0.75)/(TOTarea*TOTRain))*100,
            CARFI0.5_sim1 =sum((USarea_cum*USrain_cum*0.5)/(TOTarea*TOTRain))*100, 
            CARFI0.25_sim1 = sum((USarea_cum*USrain_cum*0.25)/(TOTarea*TOTRain))*100) %>%   #As ARFI increases, connectivity decreases
  data.frame()
data <- left_join(data,test.carfi.sim1, by = "DamID")


############Simulation 2###############################

#### Setting up a connected graph network ####
#Define nodes (in this case Dams) - define id as sequence of no. of unique dams
dam <- data.frame("id" = as.factor(seq(1, length(unique(data$DamID)),1)),
                  data[!duplicated(data$DamID),])
rownames(dam) <- NULL

#Define edges (the connection between dams)
link <- na.omit(data.frame("fromDam"= data$DamID,
                           "toDam" = data$USdam))
link$from <- dam$id[match(link$fromDam, dam$DamID)]
link$to <- dam$id[match(link$toDam, dam$DamID)]
link <- link[,c("from", "to")]

#Create graph - connected network of dams with direction using dam and link files
graph1 <- 
  create_graph() %>%
  add_nodes_from_table(
    table = dam, 
    label_col = DamID)%>%
  add_edges_from_table(
    table = link,
    from_col = from,
    to_col = to,
    from_to_map = id_external)

#visualize network
render_graph(graph1, layout = "tree") 
#some warnings, but can proceed

#------------DCIp - river length-----------------
###2.1.1 Upstream to downstream---------------
dcip.sim2.usds <- data.frame(matrix(ncol = 2, nrow = 0)) #creating an empty dataframe of dam nos and dci
colnames(dcip.sim2.usds) <- c("dam_nos", "DCIp")
dcip.sim2.usds <- rbind(dcip.sim2.usds, data.frame("dam_nos" = 0, "DCIp"= 100)) #Inserting No Dam scenario in dataframe

sim_order <- c(2,4,3,5,6:21)                  #Adding dams in this order. 
simnodes <- NULL                              #Vector to hold cumulative nodes or dams. Starts with Null, then adds in the loop
TOTlth <- 1000                                 ##check

for(i in 1:length(sim_order)){                #for each dam in sim_order
  #print(paste("Node",sim_order[i]))          #selects nodes based on order in sim_order
  simnodes <- c(simnodes, sim_order[i])       #cumulatively adds new nodes based on sim_order (us to ds)
  print(simnodes)
  
  #loop2
  simstep <- NULL                             #this holds the USlth per node for selected nodes 
  for(s in 1:length(simnodes)){               #Each simulation of selected nodes above
    simstep[s] <- graph1 %>%
      select_nodes_by_id(simnodes[s]) %>%
      get_node_attrs_ws(node_attr = USlth) %>%
      sum()
  }
  DCIp <- ((sum(simstep^2) + (TOTlth - sum(simstep))^2)/TOTlth^2) * 100      #Calc DCI
  dcip.sim2.usds <- rbind(dcip.sim2.usds, data.frame("dam_nos"=length(simnodes), "DCIp" = DCIp)) #Add values to empty dataframe
}

data$DCIp.sim2.usds <- dcip.sim2.usds$DCIp[match(data$NoDams_UStoDS, dcip.sim2.usds$dam_nos)]

###2.1.2 Downstream to upstream-------------------
dcip.sim2.dsus <- data.frame(matrix(ncol = 2, nrow = 0)) #creating an empty dataframe of dam nos and dci
colnames(dcip.sim2.dsus) <- c("dam_nos", "DCIp")
dcip.sim2.dsus <- rbind(dcip.sim2.dsus, data.frame("dam_nos" = 0, "DCIp"= 100)) #Inserting No Dam scenario in dataframe

sim_order2 <- rev(sim_order)                  #Adding dams in this order. 
simnodes <- NULL                             #Vector to hold cumulative nodes or dams. Starts with Null, then adds in the loop
                     
##Start of sim
for(i in 1:length(sim_order2)){                #for each dam in sim_order
  #print(paste("Node",sim_order2[i]))          #selects nodes based on order in sim_order
  simnodes <- c(simnodes, sim_order2[i])     #cumulatively adds new nodes based on sim_order (us to ds)
  print(simnodes)
  
  #loop2
  simstep <- NULL                             #this holds the USlth per node for selected nodes 
  for(s in 1:length(simnodes)){               #Each simulation of selected nodes above
    if(!is.na(simnodes[s+1]) | simnodes[s] %in% c(2,4)){   #If node+1 does not exist (i.e of there is a node above) OR
      simstep[s] <- graph1 %>%                               #If simnode is a terminal node
        select_nodes_by_id(simnodes[s]) %>%                 #Then extract attributes (USlth) of that node
        get_node_attrs_ws(node_attr = USlth) %>%
        sum()
    } else {
      simstep[s] <- trav.func(graph1, simnodes[s], terminals = c(2,4)) #Else, calc total US length upto terminals
    }
    
  }
  DCIp <- ((sum(simstep^2) + (TOTlth - sum(simstep))^2)/TOTlth^2) * 100      #Calc DCI
  dcip.sim2.dsus <- rbind(dcip.sim2.dsus, data.frame("dam_nos"=length(simnodes), "DCIp" = DCIp)) #Add values to empty dataframe
  
}
 
data$DCIp.sim2.dsus <- dcip.sim2.dsus$DCIp[match(data$NoDams_DStoUS, dcip.sim2.dsus$dam_nos)]

#------------DCId - river length------------------
#### 2.2.1 Upstream to downstream ####

dcid.sim2.usds <- data.frame(matrix(ncol = 2, nrow = 0)) #creating an empty dataframe of dam nos and dci
colnames(dcid.sim2.usds) <- c("dam_nos", "DCId")
dcid.sim2.usds <- rbind(dcid.sim2.usds, data.frame("dam_nos" = 0, "DCId"= 100)) #Inserting No Dam scenario in dataframe

simnodes <- NULL                              #Vector to hold cumulative nodes or dams. Starts with Null, then adds in the loop

for(i in 1:length(sim_order)){                #for each dam in sim_order
  #print(paste("Node",sim_order[i]))          #selects nodes based on order in sim_order
  simnodes <- c(simnodes, sim_order[i])       #cumulatively adds new nodes based on sim_order (us to ds)
  print(simnodes)
  
  #loop2
  simstep <- NULL                             #this holds the USlth per node for selected nodes 
  for(s in 1:length(simnodes)){               #Each simulation of selected nodes above
    simstep[s] <- graph1 %>%
      select_nodes_by_id(simnodes[s]) %>%
      get_node_attrs_ws(node_attr = USlth) %>%
      sum()
  }
  DCId <- ((1000 - sum(simstep))/TOTlth) * 100      #Calc DCI
  dcid.sim2.usds <- rbind(dcid.sim2.usds, data.frame("dam_nos"=length(simnodes), "DCId" = DCId)) #Add values to empty dataframe
  
}

data$DCId.sim2.usds <- dcid.sim2.usds$DCId[match(data$NoDams_UStoDS, dcid.sim2.usds$dam_nos)]


###2.2.2 Downstream to Upstream ####
#DS length of Dam T = 0 
#Addition of us dams does not change DCId
data$DCId.sim2.dsus <- 0


#------------CAFI - cumulative watershed area-----------------
###2.3.1 Upstream to downstream--------------------------

cafi2.sim2.usds <- data.frame(matrix(ncol = 5, nrow = 0)) #creating an empty dataframe of dam nos and dci
colnames(cafi2.sim2.usds) <- c("dam_nos","CAFI1_sim2_usds",
                               "CAFI0.75_sim2_usds","CAFI0.5_sim2_usds",
                               "CAFI0.25_sim2_usds")
#Inserting No Dam scenario in dataframe
cafi2.sim2.usds <- rbind(cafi2.sim2.usds,
                        data.frame("dam_nos" = 0, "CAFI1_sim2_usds"= 0,
                                   "CAFI0.75_sim2_usds"= 0,
                                   "CAFI0.5_sim2_usds"= 0,
                                   "CAFI0.25_sim2_usds"= 0)) 

TOTarea <- data$TOTarea[1]                    #check
simnodes <- NULL                              #Vector to hold cumulative nodes or dams. Starts with Null, then adds in the loop

for(i in 1:length(sim_order)){                #for each dam in sim_order
  #print(paste("Node",sim_order[i]))          #selects nodes based on order in sim_order
  simnodes <- c(simnodes, sim_order[i])       #cumulatively adds new nodes based on sim_order (us to ds)
  print(simnodes)
  simstep <- dam$USarea_cum[dam$id %in% simnodes]
  CAFI <- calc_cafi(simstep,TOTarea)     #Calc CAFI
  colnames(CAFI) <- c("CAFI1_sim2_usds","CAFI0.75_sim2_usds","CAFI0.5_sim2_usds","CAFI0.25_sim2_usds")
  cafi2.sim2.usds <- rbind(cafi2.sim2.usds, data.frame("dam_nos"=length(simnodes), CAFI)) #Add values to empty dataframe
  
}
data <- left_join(data, cafi2.sim2.usds, by=c("NoDams_UStoDS"="dam_nos"))

##2.3.2 Downstream to upstream---------------------------
cafi2.sim2.dsus <- data.frame(matrix(ncol = 5, nrow = 0)) #creating an empty dataframe of dam nos and dci
colnames(cafi2.sim2.dsus) <- c("dam_nos","CAFI1_sim2_dsus",
                               "CAFI0.75_sim2_dsus","CAFI0.5_sim2_dsus",
                               "CAFI0.25_sim2_dsus")
#Inserting No Dam scenario in dataframe
cafi2.sim2.dsus <- rbind(cafi2.sim2.dsus,
                         data.frame("dam_nos" = 0, "CAFI1_sim2_dsus"= 0,
                                    "CAFI0.75_sim2_dsus"= 0,
                                    "CAFI0.5_sim2_dsus"= 0,
                                    "CAFI0.25_sim2_dsus"= 0)) 

simnodes <- NULL                           #Vector to hold cumulative nodes or dams. Starts with Null, then adds in the loop

for(i in 1:length(sim_order2)){                #for each dam in sim_order
  #print(paste("Node",sim_order[i]))          #selects nodes based on order in sim_order
  simnodes <- c(simnodes, sim_order2[i])       #cumulatively adds new nodes based on sim_order (us to ds)
  print(simnodes)
  
  simstep2 <- dam$USarea_cum[dam$id %in% simnodes]
  CAFI <- calc_cafi(simstep2, TOTarea)    #Calc AFI
  colnames(CAFI) <- c("CAFI1_sim2_dsus","CAFI0.75_sim2_dsus","CAFI0.5_sim2_dsus","CAFI0.25_sim2_dsus")
  cafi2.sim2.dsus <- rbind(cafi2.sim2.dsus, data.frame("dam_nos"=length(simnodes), CAFI)) #Add values to empty dataframe
  
}
cafi2.sim2.dsus
data <- left_join(data, cafi2.sim2.dsus, by = c("NoDams_DStoUS"="dam_nos"))

#------------CARFI - cumulative watershed area & Rainfall-----------------
###2.4.1 Upstream to downstream--------------------------
carfi2.sim2.usds <- data.frame(matrix(ncol = 5, nrow = 0)) #creating an empty dataframe of dam nos and dci
colnames(carfi2.sim2.usds) <- c("dam_nos","CARFI1_sim2_usds",
                                "CARFI0.75_sim2_usds",
                                "CARFI0.5_sim2_usds","CARFI0.25_sim2_usds")
#Inserting No Dam scenario in dataframe
carfi2.sim2.usds <- rbind(carfi2.sim2.usds, 
                          data.frame("dam_nos" = 0, "CARFI1_sim2_usds"= 0,
                                     "CARFI0.75_sim2_usds"= 0,
                                     "CARFI0.5_sim2_usds"= 0,
                                     "CARFI0.25_sim2_usds"= 0))
 

TOTarea <- data$TOTarea[1]                    #check
TOTRain <- data$TOTRain[1]

#Vector to hold cumulative nodes or dams. Starts with Null, then adds in the loop
simnodes <- NULL                              
for(i in 1:length(sim_order)){                #for each dam in sim_order
  #print(paste("Node",sim_order[i]))          #selects nodes based on order in sim_order
  simnodes <- c(simnodes, sim_order[i])       #cumulatively adds new nodes based on sim_order (us to ds)
  print(simnodes)
  simstep <- dam$USarea_cum[dam$id %in% simnodes]*dam$USrain_cum[dam$id %in% simnodes]
  CARFI <- calc_carfi(simstep,TOTarea,TOTRain)     #Calc ARFI
  colnames(CARFI) <- c("CARFI1_sim2_usds","CARFI0.75_sim2_usds","CARFI0.5_sim2_usds","CARFI0.25_sim2_usds")
  carfi2.sim2.usds <- rbind(carfi2.sim2.usds, data.frame("dam_nos"=length(simnodes), CARFI)) #Add values to empty dataframe
  
}

data <- left_join(data,carfi2.sim2.usds, by=c("NoDams_UStoDS"="dam_nos"))

##2.4.2 Downstream to upstream---------------------------
carfi2.sim2.dsus <- data.frame(matrix(ncol = 5, nrow = 0)) #creating an empty dataframe of dam nos and dci
colnames(carfi2.sim2.dsus) <- c("dam_nos","CARFI1_sim2_dsus",
                                "CARFI0.75_sim2_dsus",
                                "CARFI0.5_sim2_dsus",
                                "CARFI0.25_sim2_dsus")
#Adding dams in this order. 
carfi2.sim2.dsus <- rbind(carfi2.sim2.dsus, 
                          data.frame("dam_nos" = 0,
                                     "CARFI1_sim2_dsus"= 0,"CARFI0.75_sim2_dsus"= 0,
                                      "CARFI0.5_sim2_dsus"= 0,"CARFI0.25_sim2_dsus"= 0)) #Inserting No Dam scenario in dataframe
              
simnodes <- NULL                           #Vector to hold cumulative nodes or dams. Starts with Null, then adds in the loop

for(i in 1:length(sim_order2)){                #for each dam in sim_order
  #print(paste("Node",sim_order[i]))          #selects nodes based on order in sim_order
  simnodes <- c(simnodes, sim_order2[i])       #cumulatively adds new nodes based on sim_order (us to ds)
  print(simnodes)
  simstep2 <- dam$USarea_cum[dam$id %in% simnodes]*dam$USrain_cum[dam$id %in% simnodes]
  CARFI <- calc_carfi(simstep2, TOTarea,TOTRain)    #Calc ARFI
  colnames(CARFI) <- c("CARFI1_sim2_dsus","CARFI0.75_sim2_dsus",
                       "CARFI0.5_sim2_dsus","CARFI0.25_sim2_dsus")
  carfi2.sim2.dsus <- rbind(carfi2.sim2.dsus, data.frame("dam_nos"=length(simnodes),
                                                         CARFI)) #Add values to empty dataframe
  
}

data <- left_join(data, carfi2.sim2.dsus, by = c("NoDams_DStoUS"="dam_nos"))


#############Simulation 3##################
#3.1-------------DCIp- river length---------------------------

dcip.sim3 <- data.frame(matrix(ncol = 3, nrow = 0)) #creating an empty dataframe of dam nos and dci
colnames(dcip.sim3) <- c("dam_nos", "DCIp_mean", "sd")
dcip.sim3 <- rbind(dcip.sim3, 
                   data.frame("dam_nos" = 0, "DCIp_mean" = 100, "sd" = 0))
dam$type <- as.factor(rep("no", times = nrow(dam)))

simnodes <- NULL                              #Vector to hold cumulative nodes or dams. Starts with Null, then adds in the loop
nsim = 100                                     #total number of simulations

for(i in 1:length(sim_order)){                #loop to add dams 
   print(paste("Number of Dams =",i))
   all_dci <- NULL                             #vetor to store all DCI values from simulations
  #loop2     
   for(z in 1:nsim){                           #loop to simulate different combinations of dam positions
    print(paste("sim", z))
    simnodes <- sort(sample(sim_order, i, replace = FALSE)) #samples number of dams(i)
    dam$type <- ifelse(dam$id %in% simnodes, "yes", "no")
    sim_graph <- simgraph(dam, link)
    #loop3
    simstep <- NULL                                            #this holds the USlth per node for selected nodes 
    for(s in 1:length(simnodes)){                             #Each simulation of selected nodes above
      simstep[s] <- bettertrav(sim_graph, simnodes[s])
        
    }        #end of loop3
    DCIp <- ((sum(simstep^2) + (TOTlth - sum(simstep))^2)/TOTlth^2) * 100      #Calc DCI
    all_dci[z] <- DCIp
   }         #end of loop2
   dcip.sim3 <- rbind(dcip.sim3, data.frame("dam_nos" = i, "DCIp_mean" = mean(all_dci), "sd" = sd(all_dci)))
}            #end of loop1

data$DCIpmean.sim3 <- dcip.sim3$DCIp_mean[match(data$NoDams_UStoDS, dcip.sim3$dam_nos)]
data$DCIpsd.sim3 <- dcip.sim3$sd[match(data$NoDams_UStoDS, dcip.sim3$dam_nos)]


#3.2------------- DCId - river length####

dcid.sim3 <- data.frame(matrix(ncol = 3, nrow = 0)) #creating an empty dataframe of dam nos and dci
colnames(dcid.sim3) <- c("dam_nos", "DCId_mean", "sd")
dcid.sim3 <- rbind(dcid.sim3, 
                   data.frame("dam_nos" = 0, "DCId_mean" = 100, "sd" = 0))
dam$type <- as.factor(rep("no", times = nrow(dam)))

simnodes <- NULL                              #Vector to hold cumulative nodes or dams. Starts with Null, then adds in the loop

for(i in 1:length(sim_order)){                #loop to add dams 
  print(paste("Number of Dams =",i))
  all_dcid <- NULL                             #vetor to store all DCI values from simulations
  #loop2     
  for(z in 1:nsim){                           #loop to simulate different combinations of dam positions
    print(paste("sim", z))
    simnodes <- sort(sample(sim_order, i, replace = FALSE)) #samples number of dams(i)
  
    if(21 %in% simnodes){  #21 is sink
     simstep <- 0
    } else {
      simstep_all <- trav.func(graph1, 21, unique(c(simnodes,2,4))) #2,4 are terminals
      
     # if(c(2,4) %in% simnodes3)  
      lastdams <- graph1 %>%                               #If simnode is a terminal node
        select_nodes_by_id(sim_order[length(sim_order)]) %>%
        trav_out_until(conditions = id %in% simnodes, exclude_unmatched = TRUE) %>%
        get_selection()
        #get_node_attrs_ws(node_attr = USlth) %>%
        #sum()
      lastdamid <- simnodes[simnodes %in% lastdams]
      simstep <- simstep_all - sum(data$USlth[lastdamid])
    }
      
    DCId <- (simstep/TOTlth) * 100      #Calc DCId
    all_dcid[z] <- DCId
  }         #end of loop2
  dcid.sim3 <- rbind(dcid.sim3, data.frame("dam_nos" = i, "DCId_mean" = mean(all_dcid), "sd" = sd(all_dcid)))
}            #end of loop1

data$DCIdmean.sim3 <- dcid.sim3$DCId_mean[match(data$NoDams_UStoDS, dcid.sim3$dam_nos)]
data$DCIdsd.sim3 <- dcid.sim3$sd[match(data$NoDams_UStoDS, dcid.sim3$dam_nos)]

#3.3---------------AFI - cumulative watershed area-----------------------------------
cafi.sim3 <- data.frame(matrix(ncol = 9, nrow = 0)) #creating an empty dataframe of dam nos and dci
colnames(cafi.sim3) <- c("dam_nos", "CAFI1_sim3_mean", "CAFI1_sim3_sd",
                        "CAFI0.75_sim3_mean", "CAFI0.75_sim3_sd",
                        "CAFI0.5_sim3_mean", "CAFI0.5_sim3_sd",
                        "CAFI0.25_sim3_mean", "CAFI0.25_sim3_sd")
cafi.sim3 <- rbind(cafi.sim3, 
                   data.frame("dam_nos" = 0, "CAFI1_sim3_mean" = 0, "CAFI1_sim3_sd" = 0,
                              "CAFI0.75_sim3_mean" = 0, "CAFI0.75_sim3_sd" = 0,
                              "CAFI0.5_sim3_mean" = 0, "CAFI0.5_sim3_sd" = 0,
                              "CAFI0.25_sim3_mean" = 0, "CAFI0.25_sim3_sd" = 0))
dam$type <- as.factor(rep("no", times = nrow(dam)))
  

for(i in 1:length(sim_order)){                #loop to add dams 
  print(paste("Number of Dams =",i))
  all_cafi <- data.frame(matrix(ncol=4))       #vector to store all AFI values from simulations
  
  #loop2     
  for(z in 1:nsim){                           #loop to simulate different combinations of dam positions
    print(paste("sim", z))
    simnodes <- sort(sample(sim_order, i, replace = FALSE)) #samples number of dams(i)
    simstep <- dam$USarea_cum[dam$id %in% simnodes]
    CAFI <- calc_cafi(simstep, TOTarea)     #Calc AFI
    all_cafi[z,] <- CAFI
  }         #end of loop2
   cafi.sim3 <- rbind(cafi.sim3,
                      data.frame("dam_nos" = i, "CAFI1_sim3_mean"=round(mean(all_cafi[,1]),3),
                                 "CAFI1_sim3_sd"=round(sd(all_cafi[,1]),3),
                                 "CAFI0.75_sim3_mean"=round(mean(all_cafi[,2]),3), 
                                 "CAFI0.75_sim3_sd"=round(sd(all_cafi[,2]),3),
                                 "CAFI0.5_sim3_mean"=round(mean(all_cafi[,3]),3), 
                                 "CAFI0.5_sim3_sd"=round(sd(all_cafi[,3]),3),
                                 "CAFI0.25_sim3_mean"=round(mean(all_cafi[,4]),3), 
                                 "CAFI0.25_sim3_sd"=round(sd(all_cafi[,4]),3)))
}            #end of loop1

data <- left_join(data, cafi.sim3, by=c("NoDams_UStoDS"="dam_nos"))


#3.3---------------ARFI - cumulative watershed area & rainfall----------------------------
carfi.sim3 <- data.frame(matrix(ncol = 9, nrow = 0)) #creating an empty dataframe of dam nos and dci
colnames(carfi.sim3) <- c("dam_nos", "CARFI1_sim3_mean", "CARFI1_sim3_sd",
                        "CARFI0.75_sim3_mean", "CARFI0.75_sim3_sd",
                        "CARFI0.5_sim3_mean", "CARFI0.5_sim3_sd",
                        "CARFI0.25_sim3_mean", "CARFI0.25_sim3_sd")
carfi.sim3 <- rbind(carfi.sim3, 
                    data.frame("dam_nos"=0, "CARFI1_sim3_mean"=0, "CARFI1_sim3_sd"=0,
                               "CARFI0.75_sim3_mean"=0, "CARFI0.75_sim3_sd"=0,
                               "CARFI0.5_sim3_mean"=0, "CARFI0.5_sim3_sd"=0,
                               "CARFI0.25_sim3_mean"=0, "CARFI0.25_sim3_sd"=0))
dam$type <- as.factor(rep("no", times = nrow(dam)))

for(i in 1:length(sim_order)){                #loop to add dams 
  print(paste("Number of Dams =",i))
  all_carfi <- data.frame(matrix(ncol=4))       #vector to store all AFI values from simulations
  
  #loop2     
  for(z in 1:nsim){                           #loop to simulate different combinations of dam positions
    print(paste("sim", z))
    simnodes <- sort(sample(sim_order, i, replace = FALSE)) #samples number of dams(i)
    simstep <- dam$USarea_cum[dam$id %in% simnodes]*dam$USrain_cum[dam$id %in% simnodes]
    CARFI <- calc_carfi(simstep, TOTarea, TOTRain)     #Calc ARFI
    all_carfi[z,] <- CARFI
  }         #end of loop2
  carfi.sim3 <- rbind(carfi.sim3,
                      data.frame("dam_nos" = i, "CARFI1_sim3_mean"=round(mean(all_carfi[,1]),3),
                                 "CARFI1_sim3_sd"=round(sd(all_carfi[,1]),3),
                                 "CARFI0.75_sim3_mean"=round(mean(all_carfi[,2]),3), 
                                 "CARFI0.75_sim3_sd"=round(sd(all_carfi[,2]),3),
                                 "CARFI0.5_sim3_mean"=round(mean(all_carfi[,3]),3), 
                                 "CARFI0.5_sim3_sd"=round(sd(all_carfi[,3]),3),
                                 "CARFI0.25_sim3_mean"=round(mean(all_carfi[,4]),3), 
                                 "CARFI0.25_sim3_sd"=round(sd(all_carfi[,4]),3)))
}            #end of loop1

data <- left_join(data, carfi.sim3, by=c("NoDams_UStoDS"="dam_nos"))

write.csv(data, "Jumani_et_al_Results_simulations_DCI_CAFI_CARFI.csv", row.names = F)




#--------------------------------------------------------------------------
##########Plotting simulation outputs###############
sim<- read.csv("Jumani_et_al_Results_simulations_DCI_CAFI_CARFI.csv")
#head(sim)

#-------Simulation 1---------------------------------
# Most basic line chart
p1 <- ggplot(sim, aes(x=DistMouth.km., y=(100-DCIp.sim1), label=DamID)) +
  geom_line(color="black", size=1) +
  geom_point(size=2)+
  ylim(0, 100)+
  #geom_text(aes(label=ifelse(DistMouth.km.>99,as.character(DamID),'')),hjust=0.7,vjust=-0.8)+
  xlab("Distance to mouth (km)") + ylab("100-DCIp") +
  #ggtitle("DCI") +
  theme_bw()+ theme(text = element_text(size=rel(4)))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  

p2 <- ggplot(sim, aes(x=DistMouth.km., y=(100-DCId.sim1), label=DamID)) +
  geom_line(color="black", size=1) +
  geom_point(size=2)+
  ylim(0, 100)+
  #geom_text(aes(label=ifelse(DistMouth.km.>99,as.character(DamID),'')),hjust=0.7,vjust=-0.8)+
  xlab("Distance to mouth (km)") + ylab("100-DCId") +
  theme_bw()+ theme(text = element_text(size=rel(4)))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p3 <- ggplot(sim, aes(x=DistMouth.km.))+
  geom_line(aes(y = CAFI1_sim1),color="black",size=1) +
  geom_point(aes(y = CAFI1_sim1),size=2)+
  #geom_line(aes(y = CAFI0.75_sim1),color="gray25",size=1) +
  #geom_point(aes(y = CAFI0.75_sim1),size=2)+
  #geom_line(aes(y = CAFI0.5_sim1),color="gray43",size=1) +
  #geom_point(aes(y = CAFI0.5_sim1),size=2)+
  #geom_line(aes(y = CAFI0.25_sim1),color="gray70",size=1) +
  #geom_point(aes(y = CAFI0.25_sim1),size=2)+
  xlab("Distance to mouth (km)") + ylab("CAFI") +
  #annotate("text", x = 25, y = 90, label = "1.0",  size =4)+
  #annotate("text", x = 25, y = 69, label = "0.75",  size =4)+
  #annotate("text", x = 25, y = 46, label = "0.5",  size = 4)+
  #annotate("text", x = 25, y = 25, label = "0.25",  size =4)+
  theme_bw()+ theme(text = element_text(size=rel(4)))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p4<- ggplot(sim, aes(x=DistMouth.km.))+
  geom_line(aes(y = CARFI1_sim1),color="black",size=1) +
  geom_point(aes(y = CARFI1_sim1),size=2)+
  #geom_line(aes(y = ARFI0.75_sim1),color="gray25",size=1) +
  #geom_point(aes(y = ARFI0.75_sim1),size=2)+
  #geom_line(aes(y = ARFI0.5_sim1),color="gray43",size=1) +
  #geom_point(aes(y = ARFI0.5_sim1),size=2)+
  #geom_line(aes(y = ARFI0.25_sim1),color="gray70",size=1) +
  #geom_point(aes(y = ARFI0.25_sim1),size=2)+
  xlab("Distance to mouth (km)") + ylab("CARFI") +
  #annotate("text", x = 25, y = 64, label = "1.0",  size =4)+
  #annotate("text", x = 25, y = 48, label = "0.75",  size =4)+
  #annotate("text", x = 25, y = 33, label = "0.5",  size = 4)+
  #annotate("text", x = 25, y = 17, label = "0.25",  size =4)+
  theme_bw()+ theme(text = element_text(size=rel(4)))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p1 + p2 + p3 + p4

#-------Simulation 2---------------------------------
#Here, we calculate the indices for an increasing number of dams, starting with 1, 2, 3, 4... upto 20 dams
#2 variants of this simulation - with dams being placed from US to DS; the other with dams being placed from DS to US
# BOTH DIRECTIONS ON SAME PLOT
dcip_ustods<-sim$DCIp.sim2.usds
dcip_dstous<-sort(sim$DCIp.sim2.dsus, decreasing = TRUE)
dcip_nodams<-sim$NoDams_UStoDS
dp<-as.data.frame(cbind(dcip_ustods, dcip_dstous, dcip_nodams))

dcid_ustods<-sim$DCId.sim2.usds
dcid_dstous<-sort(sim$DCId.sim2.dsus, decreasing = TRUE)
dcid_nodams<-sim$NoDams_UStoDS
dd<-as.data.frame(cbind(dcid_ustods, dcid_dstous, dcid_nodams))

cafi_ustods_1<-sim$CAFI1_sim2_usds
cafi_ustods_0.75<-sim$CAFI0.75_sim2_usd
cafi_ustods_0.5<-sim$CAFI0.5_sim2_usds
cafi_ustods_0.25<-sim$CAFI0.25_sim2_usds

cafi_dstous_1<-sort(sim$CAFI1_sim2_dsus, decreasing = FALSE)
cafi_dstous_0.75<-sort(sim$CAFI0.75_sim2_dsus, decreasing = FALSE)
cafi_dstous_0.5<-sort(sim$CAFI0.5_sim2_dsus, decreasing = FALSE)
cafi_dstous_0.25<-sort(sim$CAFI0.25_sim2_dsus, decreasing = FALSE)

cafi_nodams<-sim$NoDams_UStoDS
a<-data.frame(cafi_ustods_1, cafi_ustods_0.75, cafi_ustods_0.5, cafi_ustods_0.25,
                       cafi_dstous_1, cafi_dstous_0.75, cafi_dstous_0.5, cafi_dstous_0.25,
                       cafi_nodams)

carfi_ustods_1<-sim$CARFI1_sim2_usds
carfi_ustods_0.75<-sim$CARFI0.75_sim2_usd
carfi_ustods_0.5<-sim$CARFI0.5_sim2_usds
carfi_ustods_0.25<-sim$CARFI0.25_sim2_usds

carfi_dstous_1<-sort(sim$CARFI1_sim2_dsus, decreasing = FALSE)
carfi_dstous_0.75<-sort(sim$CARFI0.75_sim2_dsus, decreasing = FALSE)
carfi_dstous_0.5<-sort(sim$CARFI0.5_sim2_dsus, decreasing = FALSE)
carfi_dstous_0.25<-sort(sim$CARFI0.25_sim2_dsus, decreasing = FALSE)

carfi_nodams<-sim$NoDams_UStoDS
ar<-as.data.frame(cbind(carfi_ustods_1, carfi_ustods_0.75, carfi_ustods_0.5, carfi_ustods_0.25,
                       carfi_dstous_1, carfi_dstous_0.75, carfi_dstous_0.5, carfi_dstous_0.25,
                       carfi_nodams))


st1<- ggplot(dp, aes(x=dcip_nodams)) + 
  geom_line(aes(y = 100- dcip_ustods), color = "steelblue2", size=1) + 
  geom_point(aes(y = 100- dcip_ustods),size=2)+
  geom_line(aes(y = 100- dcip_dstous), color="orange", size=1)+
  geom_point(aes(y = 100- dcip_dstous),size=2)+
  xlab("Number of dams") + ylab("100-DCIp") +
  #ggtitle("DCI") +
  theme_bw()+ theme(text = element_text(size=rel(4)))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

st2<- ggplot(dd, aes(x=dcid_nodams)) + 
  geom_line(aes(y = 100- dcid_ustods), color = "steelblue2", size=1) + 
  geom_point(aes(y = 100- dcid_ustods),size=2)+
  geom_line(aes(y = 100- dcid_dstous), color="orange", size=1)+
  geom_point(aes(y = 100- dcid_dstous),size=2)+
  xlab("Number of dams") + ylab("100-DCId") +
  #ggtitle("DCI") +
  theme_bw()+ theme(text = element_text(size=rel(4)))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

st3<- ggplot(a, aes(x=cafi_nodams)) + 
  geom_line(aes(y = cafi_ustods_1), color = "steelblue4",size=1) + 
  geom_point(aes(y = cafi_ustods_1),size=2)+
  #geom_line(aes(y = cafi_ustods_0.75), color = "steelblue3",size=1) + 
  #geom_point(aes(y = cafi_ustods_0.75),size=2)+
  #geom_line(aes(y = cafi_ustods_0.5), color = "steelblue2",size=1) + 
  #geom_point(aes(y = cafi_ustods_0.5),size=2)+
  #geom_line(aes(y = cafi_ustods_0.25), color = "steelblue1",size=1) + 
  #geom_point(aes(y = cafi_ustods_0.25),size=2)+
  
  geom_line(aes(y = cafi_dstous_1), color="orange", size=1)+
  geom_point(aes(y = cafi_dstous_1),size=2)+
  #geom_line(aes(y = cafi_dstous_0.75), color="orangered3", size=1)+
  #geom_point(aes(y = cafi_dstous_0.75),size=2)+
  #geom_line(aes(y = cafi_dstous_0.5), color="orangered2", size=1)+
  #geom_point(aes(y = cafi_dstous_0.5),size=2)+
  #geom_line(aes(y = cafi_dstous_0.25), color="orangered1", size=1)+
  #geom_point(aes(y = cafi_dstous_0.25),size=2)+
  xlab("Number of dams") + ylab("CAFI") +
  #ggtitle("CAFI") +
  
  #annotate("text", x = 19, y = 1060, label = "1.0",  size =4)+
  #annotate("text", x = 19, y = 800, label = "0.75",  size =4)+
  #annotate("text", x = 19, y = 550, label = "0.5",  size = 4)+
  #annotate("text", x = 19, y = 285, label = "0.25",  size =4)+
  theme_bw()+ theme(text = element_text(size=rel(4)))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

st4<- ggplot(ar, aes(x=carfi_nodams)) + 
  geom_line(aes(y = carfi_ustods_1), color = "steelblue4",size=1) + 
  geom_point(aes(y = carfi_ustods_1),size=2)+
  #geom_line(aes(y = carfi_ustods_0.75), color = "steelblue3",size=1) + 
  #geom_point(aes(y = carfi_ustods_0.75),size=2)+
  #geom_line(aes(y = carfi_ustods_0.5), color = "steelblue2",size=1) + 
  #geom_point(aes(y = carfi_ustods_0.5),size=2)+
  #geom_line(aes(y = carfi_ustods_0.25), color = "steelblue1",size=1) + 
  #geom_point(aes(y = carfi_ustods_0.25),size=2)+
  
  geom_line(aes(y = carfi_dstous_1), color="orange", size=1)+
  geom_point(aes(y = carfi_dstous_1),size=2)+
  #geom_line(aes(y = arfi_dstous_0.75), color="orangered3", size=1)+
  #geom_point(aes(y = arfi_dstous_0.75),size=2)+
  #geom_line(aes(y = arfi_dstous_0.5), color="orangered2", size=1)+
  #geom_point(aes(y = arfi_dstous_0.5),size=2)+
  #geom_line(aes(y = arfi_dstous_0.25), color="orangered1", size=1)+
  #geom_point(aes(y = arfi_dstous_0.25),size=2)+
  xlab("Number of dams") + ylab("CARFI") +
  #ggtitle("CARFI") +
  
  #annotate("text", x = 19, y = 790, label = "1.0",  size =4)+
  #annotate("text", x = 19, y = 600, label = "0.75",  size =4)+
  #annotate("text", x = 19, y = 405, label = "0.5",  size = 4)+
  #annotate("text", x = 19, y = 212, label = "0.25",  size =4)+
  theme_bw()+ theme(text = element_text(size=rel(4)))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

st1 + st2 + st3 + st4# Display charts side by side 


#-------Simulation 3---------------------------------
a<- ggplot(sim, aes(x=NoDams_UStoDS, y=DCIpmean.sim3)) + 
  geom_errorbar(aes(ymin=DCIpmean.sim3-DCIpsd.sim3, ymax=DCIpmean.sim3+DCIpsd.sim3),colour="black", width=.5) +
  geom_line(color="black", size=1) +
  geom_point(size=2)+
  xlab("Number of dams") + ylab("DCIp") +
  #ggtitle("DCI") +
  theme_bw()+ theme(text = element_text(size=rel(4)))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

b<- ggplot(sim, aes(x=NoDams_UStoDS, y=DCIdmean.sim3)) + 
  geom_errorbar(aes(ymin=DCIdmean.sim3-DCIdsd.sim3, ymax=DCIdmean.sim3+DCIdsd.sim3),colour="black", width=.5) +
  geom_line(color="black", size=1) +
  geom_point(size=2)+
  xlab("Number of dams") + ylab("DCId") +
  #ggtitle("DCI") +
  theme_bw()+ theme(text = element_text(size=rel(4)))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

c<- ggplot(sim, aes(x=NoDams_UStoDS))+
  geom_line(aes(y = CAFI1_sim3_mean),color="black",size=1) +
  geom_errorbar(aes(ymin=CAFI1_sim3_mean-CAFI1_sim3_sd, ymax=CAFI1_sim3_mean+CAFI1_sim3_sd), colour="black", width=.5) +
  geom_point(aes(y = CAFI1_sim3_mean),size=2)+
  #geom_line(aes(y = CAFI0.75_sim3_mean),color="gray25",size=1) +
  #geom_errorbar(aes(ymin=CAFI0.75_sim3_mean-CAFI0.75_sim3_sd, ymax=CAFI0.75_sim3_mean+CAFI0.75_sim3_sd), colour="black", width=.5) +
  #geom_point(aes(y =CAFI0.75_sim3_mean),size=2)+
  #geom_line(aes(y = CAFI0.5_sim3_mean),color="gray43",size=1) +
  #geom_errorbar(aes(ymin=CAFI0.5_sim3_mean-CAFI0.5_sim3_sd, ymax=CAFI0.5_sim3_mean+CAFI0.5_sim3_sd), colour="black", width=.5) +
  #geom_point(aes(y = CAFI0.5_sim3_mean),size=2)+
  #geom_line(aes(y = CAFI0.25_sim3_mean),color="gray70",size=1) +
  #geom_errorbar(aes(ymin=CAFI0.25_sim3_mean-CAFI0.25_sim3_sd, ymax=CAFI0.25_sim3_mean+CAFI0.25_sim3_sd), colour="black", width=.5) +
  #geom_point(aes(y = CAFI0.25_sim3_mean),size=2)+
  xlab("Number of dams") + ylab("CAFI") +
  #annotate("text", x = 19, y = 1045, label = "1.0",  size =4)+
  #annotate("text", x = 19, y = 795, label = "0.75",  size =4)+
  #annotate("text", x = 19, y = 550, label = "0.5",  size = 4)+
  #annotate("text", x = 19, y = 280, label = "0.25",  size =4)+
  #ggtitle("CAFI") + 
  theme_bw()+ theme(text = element_text(size=rel(4)))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

d<- ggplot(sim, aes(x=NoDams_UStoDS))+
  geom_line(aes(y = CARFI1_sim3_mean),color="black",size=1) +
  geom_errorbar(aes(ymin=CARFI1_sim3_mean-CARFI1_sim3_sd, ymax=CARFI1_sim3_mean+CARFI1_sim3_sd), colour="black", width=.5) +
  geom_point(aes(y = CARFI1_sim3_mean),size=2)+
  #geom_line(aes(y = CARFI0.75_sim3_mean),color="gray25",size=1) +
  #geom_errorbar(aes(ymin=CARFI0.75_sim3_mean-CARFI0.75_sim3_sd, ymax=CARFI0.75_sim3_mean+CARFI0.75_sim3_sd), colour="black", width=.5) +
  #geom_point(aes(y =CARFI0.75_sim3_mean),size=2)+
  #geom_line(aes(y = CARFI0.5_sim3_mean),color="gray43",size=1) +
  #geom_errorbar(aes(ymin=CARFI0.5_sim3_mean-CARFI0.5_sim3_sd, ymax=CARFI0.5_sim3_mean+CARFI0.5_sim3_sd), colour="black", width=.5) +
  #geom_point(aes(y = CARFI0.5_sim3_mean),size=2)+
  #geom_line(aes(y = CARFI0.25_sim3_mean),color="gray70",size=1) +
  #geom_errorbar(aes(ymin=CARFI0.25_sim3_mean-CARFI0.25_sim3_sd, ymax=CARFI0.25_sim3_mean+CARFI0.25_sim3_sd), colour="black", width=.5) +
  #geom_point(aes(y = CARFI0.25_sim3_mean),size=2)+
  xlab("Number of dams") + ylab("CARFI") +
  #annotate("text", x = 19, y = 780, label = "1.0",  size =4)+
  #annotate("text", x = 19, y = 600, label = "0.75",  size =4)+
  #annotate("text", x = 19, y = 400, label = "0.5",  size = 4)+
  #annotate("text", x = 19, y = 215, label = "0.25",  size =4)+
  #ggtitle("CARFI") + 
  theme_bw()+ theme(text = element_text(size=rel(4)))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

a +b + c +d



