###############################################################################
# Script to generate instance set for analysis
#
# Author: Vivek Katial
# Created 2020-06-02 00:38:55
###############################################################################

# Source in scripts
library(here)
library(tidyverse)
library(furrr)
future::plan(multiprocess, workers = parallel::detectCores())

# Source in relevant scripts
source(here("src/instance-generation/generate-relaxed-usa-instances.R"))
source(here("src/instance-generation/generate-usa-instances.R"))

t0 = proc.time()
d_instance_set <- list(
  n_qubits = 5:15,
  n_sat = 3,
  instance_index = 1:100
) %>% 
  cross_df() %>% 
  mutate(
    params = future_map2(n_qubits, n_sat, function(x, y){list(n_qubits = x, n_sat = y)}, .progress = T),
    d_clauses_usa = future_map(params, generate_clauses, .progress = T),
    d_clauses_usa_relaxed = future_map(params, generate_relaxed_usa_clauses, .progress = T)
  )
t1 = proc.time() - t0
print(t1[[3]])
d_instance_set %>% 
  write_rds("data/d_instance_set.rds")
