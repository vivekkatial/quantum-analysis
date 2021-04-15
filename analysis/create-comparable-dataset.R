###############################################################################
# Analysis of Combined Data
#
# Author: Vivek Katial
# Created 2020-12-10 14:46:37
###############################################################################

library(tidyverse)

d_comb <- read_rds("data/d_combined.rds") %>%
  filter(!is.na(classical_optimiser),
         tags_mlflow_user.qaoa != "ubuntu")

fair_instances <- d_comb %>%
  # Count how many runs on each instance
  count(params_instance, classical_optimiser) %>%
  spread(classical_optimiser, n) %>%
  janitor::clean_names() %>%
  # Only look at instances that have same number of classical optimisers
  filter(cma_es == nelder_mead) %>%
  pull(params_instance)

d_comparable <- d_comb %>% 
  filter(params_instance %in% fair_instances)

d_comparable %>% 
  write_rds("data/d_comparable.rds")
