###############################################################################
# Functions for enrichment of graph statistics
#
# Author: Vivek Katial
# Created 2020-05-21 00:54:38
###############################################################################

#' Function to extract node degree vector from graph
get_graph_stats_vec = function(graph, node_type){
  if (is.na(graph)) {
    return(NA)
  } else {
    degree(graph) %>% 
      as_tibble(rownames = "node") %>% 
      filter(str_detect(node, node_type)) %>% 
      pull(value)
  }
}

#' Function to find mean node degree
get_graph_node_deg_mean = function(graph, node_type){
  graph_vec = get_graph_stats_vec(graph, node_type)
  mean(graph_vec)
}

#' Function to find median node degree
get_graph_node_deg_median = function(graph, node_type){
  graph_vec = get_graph_stats_vec(graph, node_type)
  median(graph_vec)
}

# Function to find sd of node degree
get_graph_node_deg_sd = function(graph, node_type){
  graph_vec = get_graph_stats_vec(graph, node_type)
  sd(graph_vec)
}

# Function to find min of node degree
get_graph_node_deg_min = function(graph, node_type){
  graph_vec = get_graph_stats_vec(graph, node_type)
  min(graph_vec)
}

# Function to find max of node degree
get_graph_node_deg_max = function(graph, node_type){
  graph_vec = get_graph_stats_vec(graph, node_type)
  max(graph_vec)
}
