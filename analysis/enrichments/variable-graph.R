###############################################################################
# Variable Graph Enirchment Function
#
# Author: Vivek Katial
# Created 2020-05-20 09:46:27
###############################################################################

#' Variable Graph:
#'   - Node for each variable
#'   - Edge between variables that occur together in at least one clause

#' This function creates a variable graph from the clause data
#' @param d_clauses A `list()` containing the variable clause graph
#' @param n_qubits An `integer` representing the number of qubits
#' @param ret.all An optional parameter, if `TRUE` the function returns a `list()` containing the edge list, 
#' node list and graph object
make_variable_graph = function(d_clauses, n_qubits, ret.all = FALSE){
  
  if (is.na(d_clauses)) {
    return (NA)
  } else if (d_clauses %>% unlist() %>% as.vector() %>% max() > n_qubits){
    return (NA)
  }
  
  # Construct node list
  node_list <- paste0("var_",seq.int(1, n_qubits))
  
  # Build edge list
  edge_list <- tibble(
    from = character(),
    to = character()
  )
  
  # Build edges
  for (clause in d_clauses) {
    
    clause_nodes <- paste0("var_", clause)
    
    # Get all pair combinations
    c_edges <- combn(clause_nodes, 2) %>% 
      t() %>% 
      # Construct a tibble containing all edges for the specific clause
      as_tibble() %>% 
      rename(
        from = 1,
        to = 2
      )
    
    edge_list <- edge_list %>% 
      bind_rows(c_edges)
  }
  
  # Ensure repeated edges remoeed
  edge_list <- edge_list %>% 
    distinct()
  
  # Build graph
  var_graph <- graph_from_data_frame(d = edge_list, vertices = node_list, directed = FALSE)
  
  if (ret.all == T) {
    list(
      var_graph = var_graph,
      edge_list = edge_list,
      node_list = node_list
    )
  } else {
    var_graph
  }
}

# Example -----------------------------------------------------------------


# example_d_clauses = list(
#   k_1 = c(1L, 3L, 4L), 
#   k_2 = c(1L, 5L, 6L), 
#   k_3 = c(1L, 4L, 6L), 
#   k_4 = c(2L, 5L, 6L), 
#   k_5 = c(1L, 2L, 5L)
# )
# 
# colrs = c("tomato", "gold")
# example_qubits = 6
# 
# d_clauses = example_d_clauses
# n_qubits = example_qubits
# 
# var_graph = make_variable_graph(example_d_clauses, example_qubits)
# 
# plot(var_graph, edge.arrow.size = 0.2, vertex.label = NA)

