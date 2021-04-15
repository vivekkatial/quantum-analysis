###############################################################################
# Variable-Clause Graph Enrichment Functions
#
# Author: Vivek Katial
# Created 2020-05-20 00:55:16
###############################################################################

#'   Variable-Clause Graph (VCG):
#'   - Node for each variable $i \in N$
#'   - Node for each clause $j \in K$
#'   - Edge connecting variable and clause nodes whenever a variable occurs in a clause.

#' This function creates a variable clause graph from the VCG
#' @param d_clauses A `list()` containing the variable clause graph
#' @param n_qubits An `integer` representing the number of qubits
#' @param ret.all An optional parameter, if `TRUE` the function returns a `list()` containing the edge list,
#' node list and graph object
make_variable_clause_graph = function(d_clauses, n_qubits, ret.all = FALSE) {
  if (is.na(d_clauses)) {
    return (NA)
  } else if (d_clauses %>% unlist() %>% as.vector() %>% max() > n_qubits) {
    return (NA)
  }
  
  var_nodes <- paste0("var_", seq.int(1, n_qubits))
  clause_nodes <- paste0("cls_", seq.int(1, length(d_clauses)))
  
  node_list <- tibble(id = c(var_nodes, clause_nodes)) %>%
    mutate(
      node_type = case_when(str_detect(id, "var_") ~ "var",
                            str_detect(id, "cls_") ~ "cls"),
      color = case_when(node_type == "var" ~ "tomato",
                        node_type == "cls" ~ "gold")
    )
  
  edge_list <- tibble(var_nodes = character(),
                      clause_nodes = character())
  
  clause_node <- 1
  
  for (clause in d_clauses) {
    clause_edges <- tibble(var_nodes = paste0("var_", clause),
                           clause_nodes = paste0("cls_", rep(clause_node, length(clause))))
    
    edge_list <- edge_list %>%
      bind_rows(clause_edges)
    
    clause_node = clause_node + 1
  }
  
  tryCatch({
    var_clause_graph <-
      graph_from_data_frame(d = edge_list,
                            vertices = node_list$id,
                            directed = FALSE)
    V(var_clause_graph)$color <- node_list$color
  },
  error = function(e) {
    message(e)
    return (NA)
  })
  
  if (ret.all == T) {
    list(node_list = node_list,
         edge_list = edge_list,
         vcg = var_clause_graph)
  } else {
    var_clause_graph
  }
}



# Example -----------------------------------------------------------------

# example_d_clauses = list(
#   k_1 = c(1L, 3L, 4L),
#   k_2 = c(1L, 5L, 6L),
#   k_3 = c(1L, 4L, 6L),
#   k_4 = c(2L, 5L, 6L),
#   k_5 = c(1L, 2L, 5L)
#   )
#
# colrs = c("tomato", "gold")
# example_qubits = 6
# example_var_clause_graph = make_variable_clause_graph(d_clauses = example_d_clauses, example_qubits, ret.all = T)
#
# # Plotting Variable Clause Graph
# plot(example_var_clause_graph$vcg, edge.arrow.size = 0.2, vertex.label = NA)
# legend(
#   x=-1.5,
#   y=-1.1,
#   c("Variable","Clause"),
#   pch=21,
#   col="#777777",
#   pt.bg=colrs,
#   pt.cex=2,
#   cex=.8,
#   bty="n",
#   ncol=1
#   )
