###############################################################################
# Prep stuff for matilda
#
# Author: Vivek Katial
# Created 2021-04-13 17:16:59
###############################################################################

library(tidyverse)

feature_vector <- c("params.radius", "params.q_p", "params.vertex_connectivity", 
  "params.number_of_vertices", "params.edge_connectivity", "params.algebraic_connectivity", 
  "params.eulerian", "params.planar", "params.average_distance", 
  "params.density", "params.acyclic", "params.num_vehicles", "params.connected", 
  "params.q_n_max", "params.minimum_degree", "params.second_largest_eigenvalue", 
  "params.q_threshold", "params.minimum_dominating_set", "params.clique_number", 
  "params.maximum_degree", "params.number_of_edges", "params.laplacian_largest_eigenvalue", 
  "params.smallest_eigenvalue", "params.source", "params.number_of_components", 
  "params.bipartite", "params.diameter", "params.regular","params.source"
)

metrics <- c("metrics.m_optimal_value","metrics.m_p_success", "metrics.m_num_layers")

Sys.time()

RUN_DATETIME <- as.POSIXct("2021-04-17 05:00:00", tz = "UTC")

# Adding stuff for QAOA
d_runs <- read_csv("data/d_vrp-qaoa.csv") %>% 
  filter(
    status == "FINISHED", 
    start_time > RUN_DATETIME,
    !is.na(metrics.layer_4_quantum_burden)
    ) %>% 
  select(run_id, starts_with("metrics.layer_"), feature_vector)


d_matilda <- d_runs %>% 
  select(run_id, feature_vector, starts_with("metrics.layer_")) %>% 
  rename(
    Source = params.source,
    Instances = run_id
    ) %>% 
  rename_at(vars(starts_with("params.")), list( ~ str_replace(., "params.", "feature_"))) %>% 
  rename_at(vars(starts_with("metrics.layer_")), list( ~ str_replace(., "metrics.layer_", "algo_layer_"))) %>% 
  select(
    Instances,
    Source,
    contains("feature_"),
    contains("algo")
  ) %>% 
  mutate_if(is.logical, as.numeric) %>% 
  select_if(~n_distinct(.) > 1)

# Test no missing values
testthat::expect_equal(d_matilda %>%
                         summarise_all(list( ~ sum(is.na(
                           .
                         )))) %>%
                         gather(var, n) %>%
                         pull(n) %>%
                         sum(),
                       0)

# Tests unique names
testthat::expect_equal(d_matilda %>%
                         count(Instances) %>%
                         pull(n) %>%
                         sum(),
                       d_matilda %>%
                         nrow(),
                       info = "Checking Unique Instances")

# Test that atleast 2 sources
testthat::expect_gt(d_matilda %>%
                      count(Source) %>%
                      nrow(),
                    1)

d_matilda %>% 
  write_csv("data/d_matilda.csv")

d_matilda %>% 
  view()
