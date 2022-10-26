###############################################################################
# Prep stuff for matilda
#
# Author: Vivek Katial
# Created 2021-04-13 17:16:59
###############################################################################

library(tidyverse)
library(glue)

feature_vector <- c("params.radius", "params.q_p", 
  "params.number_of_vertices", "params.algebraic_connectivity", 
  "params.eulerian", "params.planar", "params.average_distance", 
  "params.density", "params.acyclic", "params.num_vehicles", "params.connected", 
  "params.q_n_max", "params.minimum_degree", 
  "params.q_threshold", "params.minimum_dominating_set", "params.clique_number", 
  "params.maximum_degree", "params.laplacian_largest_eigenvalue", 
  "params.smallest_eigenvalue", "params.source", "params.number_of_components", 
  "params.bipartite", "params.diameter", "params.regular","params.source"
)

metrics <- c("metrics.m_optimal_value","metrics.m_p_success", "metrics.m_num_layers")

Sys.time()

RUN_DATETIME <- as.POSIXct("2022-10-18 22:45:00 AEDT", tz = "UTC")

# Adding stuff for QAOA
d_runs <- read_csv("data/d_vqe-maxcut.csv") %>% 
  filter(
    status == "FINISHED", 
    start_time > RUN_DATETIME
    )


instance_classes <- c("uniform_random", "watts_strogatz_small_world", "power_law_tree", "geometric", "nearly_complete_bi_partite")

d_matilda <- map_df(instance_classes, function(instance_class){
  d_runs %>% 
    select(run_id, contains(instance_class)) %>% 
    mutate(Instances = glue("{run_id}_{instance_class}")) %>% 
    select(Instances, everything()) %>% 
    rename_at(
      vars(starts_with(glue("params.{instance_class}"))), 
      list( ~ str_replace(., glue("params.{instance_class}"), "feature"))
    ) %>% 
    rename(
      energy_gap = glue("metrics.energy_gap_{instance_class}")
    ) %>% 
    mutate_if(is.logical, as.numeric) %>% 
    select_if(~n_distinct(.) > 1) %>% 
    drop_na() %>% 
    mutate(Source = instance_class) %>% 
    select(Instances, Source, energy_gap, starts_with("feature"))
}) %>% 
  drop_na()

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
  rename(algo_energy_gap = energy_gap) %>% 
  select(Instances, Source, starts_with("feature"), starts_with("algo")) %>% 
  write_csv("data/d_matilda.csv")

d_matilda %>% 
  select(Instances, Source, contains("feature_tsp"), contains("algo")) %>% 
  count(Source)


qplot(d_matilda$energy_gap)
