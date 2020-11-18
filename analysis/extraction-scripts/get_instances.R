###############################################################################
# Implementation of an R script which is used to download instance data
#
# Author: Vivek Katial
# Created 2020-05-13 23:48:16
###############################################################################


# Setting Up --------------------------------------------------------------

library(igraph)
library(tidyverse)
library(mlflow)
library(yaml)
source("utils/mlflow-utils.R")
source("analysis/enrichments/variable-graph.R")
source("analysis/enrichments/variable-clause-graph.R")
source("analysis/enrichments/graph-statistics.R")

# Global Vars
DATA_PATH = "data/d_runs.csv"

# Read experiment config file
exp_config <- read_yaml("config/mlflow-tracking-server.yml")
# Set MLFlow tracking URI
mlflow_set_tracking_uri(exp_config$mlflow$tracking_server_uri)

# Read in MLFlow data (AFTER having run `get_mlflow_data.py`)
d_runs <- get_mlflow_data(DATA_PATH)

# Enrich Instances --------------------------------------------------------

d_instances <- d_runs %>% 
  mutate(d_clauses = map(run_id, get_clause_data))

d_instances %>% 
  write_rds("data/d_instances.rds")

d_enriched <- d_instances %>% 
  # Add Problem size features
  mutate(
    f_p_size_n_clauses = map_dbl(d_clauses, length),
    f_p_size_n_variables = params_n_qubits,
    f_p_size_ratio = metrics_clause_var_ratio,
    f_p_size_ratio_sq = metrics_clause_var_ratio^2,
    f_p_size_ratio_cub = metrics_clause_var_ratio^3,
    f_p_size_ratio_recp = 1/metrics_clause_var_ratio,
    f_p_size_ratio_recp_sq = (1/metrics_clause_var_ratio)^2,
    f_p_size_ratio_recp_cub = (1/metrics_clause_var_ratio)^3,
    f_p_size_lin_ratio = abs(4.26 - metrics_clause_var_ratio),
    f_p_size_lin_ratio_sq = abs(4.26 - metrics_clause_var_ratio)^2,
    f_p_size_lin_ratio_cb = abs(4.26 - metrics_clause_var_ratio)^3
  ) %>% 
  # Apply VCG Node stats
  mutate(
    f_vcg_graph = map2(d_clauses, params_n_qubits, make_variable_clause_graph),
    f_vcg_mean_var = map2_dbl(f_vcg_graph, "var", get_graph_node_deg_mean),
    f_vcg_mean_cls = map2_dbl(f_vcg_graph, "cls", get_graph_node_deg_mean),
    f_vcg_median_var = map2_dbl(f_vcg_graph, "var", get_graph_node_deg_median),
    f_vcg_median_cls = map2_dbl(f_vcg_graph, "cls", get_graph_node_deg_median),
    f_vcg_min_var = map2_dbl(f_vcg_graph, "var", get_graph_node_deg_min),
    f_vcg_min_cls = map2_dbl(f_vcg_graph, "cls", get_graph_node_deg_min),
    f_vcg_max_var = map2_dbl(f_vcg_graph, "var", get_graph_node_deg_max),
    f_vcg_max_cls = map2_dbl(f_vcg_graph, "cls", get_graph_node_deg_max)
  ) %>% 
  # Apply VG Node Stats
  mutate(
    f_vg_graph = map2(d_clauses, params_n_qubits, make_variable_graph),
    f_vg_mean = map2_dbl(f_vg_graph, "var", get_graph_node_deg_mean),
    f_vg_median = map2_dbl(f_vg_graph, "var", get_graph_node_deg_median),
    f_vg_min = map2_dbl(f_vg_graph, "var", get_graph_node_deg_min),
    f_vg_max = map2_dbl(f_vg_graph, "var", get_graph_node_deg_max)
  )

d_enriched %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  write_rds("data/d_enriched.rds")
