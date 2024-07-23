###FUNCTION TO CREATE GRAPH NETWORK
simgraph <- function(dam, link) {
  create_graph() %>%
    add_nodes_from_table(
      table = dam, label_col = DamID, type_col = type)%>%
    add_edges_from_table(
      table = link, from_col = from, to_col = to,
      from_to_map = id_external)
}

###Trav out function 
#travel outward until the terminal nodes & select US lth
trav.func <- function(graph, node, terminals){
  graph %>%
    select_nodes_by_id(node) %>%
    trav_out_until(conditions = id %in% terminals, add_to_selection = TRUE) %>%
    get_node_attrs_ws(node_attr = USlth) %>%
    sum()
}

bettertrav <- function(graph, node){
  graph %>%
    select_nodes_by_id(node) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    trav_out(conditions = type != "yes", add_to_selection = T) %>%
    get_node_attrs_ws(
      node_attr = USlth) %>%
    sum() 
}

###FUNCTION TO CALCULATE CAFI
calc_cafi <- function(catchmentarea, totalarea){                                       
  cafi <- data.frame("CAFI1"= round((sum(catchmentarea*1)/totalarea)*100,3),
                    "CAFI0.75"= round((sum(catchmentarea*0.75)/totalarea)*100,3),
                    "CAFI0.5"= round((sum(catchmentarea*0.5)/totalarea)*100,3),
                    "CAFI0.25"= round((sum(catchmentarea*0.25)/totalarea)*100,3))
  cafi
}

###FUNCTION TO CALCULATE CARFI
calc_carfi <- function(catchmentarea, totalarea, TOTRain){                             
  carfi <- data.frame("CARFI1"= round((sum(catchmentarea*1)/(totalarea*TOTRain))*100,3),
                     "CARFI0.75"= round((sum(catchmentarea*0.75)/(totalarea*TOTRain))*100,3),
                     "CARFI0.5"= round((sum(catchmentarea*0.5)/(totalarea*TOTRain))*100,3),
                     "CARFI0.25"= round((sum(catchmentarea*0.25)/(totalarea*TOTRain))*100,3))
  carfi
}

