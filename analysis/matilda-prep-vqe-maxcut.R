###############################################################################
# Prep stuff for matilda
#
# Author: Vivek Katial
# Created 2021-04-13 17:16:59
###############################################################################

library(tidyverse)
library(glue)

Sys.time()

RUN_DATETIME <- as.POSIXct("2022-11-21 17:45:00 AEDT", tz = "UTC")

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
    select(-contains("feature_acyclic")) %>% 
    select(-contains("feature_number_of_vertices")) %>%
    drop_na() %>% 
    mutate(Source = instance_class) %>% 
    select(Instances, Source, energy_gap, starts_with("feature"))
}) %>% 
  filter(feature_laplacian_second_largest_eigenvalue > 0) %>% 
  select(-c("feature_eulerian","feature_bipartite","feature_regular")) %>% 
  drop_na()

d_matilda %>% 
  count(Source)

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


d_matilda %>% 
  filter(Source == "geometric") %>% 
  ggplot(aes(x = feature_density)) +
  geom_histogram()

d_matilda %>% 
  filter(str_detect(Instances, "3b75")) %>% 
  filter(Source == "nearly_complete_bi_partite") %>% 
  gather(var, val) %>% 
  View()





d_dist <- read_csv("~/Documents/HAQC/data/geometric-graphs.csv")
d_dist %>% 
  group_by(radius) %>% 
  count(connected) %>% 
  ungroup() %>% 
  group_by(radius) %>% 
  mutate(perc = n/sum(n)) %>% 
  filter(connected) %>% 
  ggplot(aes(x = radius, y = perc)) + 
  geom_line() + 
  theme_minimal()


d_dist %>% 
  group_by(radius) %>% 
  count(connected) %>% 
  ungroup() %>% 
  group_by(radius) %>% 
  mutate(perc = n/sum(n)) %>% 
  filter(connected) %>% 
  View()
