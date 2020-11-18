###############################################################################
# Script to identify r coeffes for each run and produce a QAOA circuit input
# instance.
#
# Author: Vivek Katial
# Created 2020-09-07 21:52:42
###############################################################################

library(tidyverse)
library(furrr)
library(logging)
source("analysis/hamiltonian-coeffs.R")
source("src/build-hamiltonians/build-hamiltonians.R")
source("src/instance-generation/generate-usa-instances.R")
source("utils/define-pauli-matrices.R")
source("utils/exp-utils.R")
source("src/make-templates.R")

future::plan(multiprocess, workers = parallel::detectCores())

#' This function writes parameter files
#' We need to make sure a directory to hold  them is  there
#' @param file_name Name of the file
#' @param instance Instance
.write_qaoa_instance = function(file_name, instance){
  
  # Check if all necessary folders are present!
  if (!dir.exists("data/qaoa/qaoa_instances")) {
    cat("WARNING: Directory 'data/qaoa_instances' not  found.")
    dir.create("data/qaoa")
    dir.create("data/qaoa/qaoa_instances")
  }
  
  # Construct filename
  file_name <- paste0("qaoa_instance_", file_name , ".json")
  file_name <- file.path("data", "qaoa/qaoa_instances",  file_name)
  
  # Write file out to path
  jsonlite::write_json(instance[1], file_name, simplifyVector=T)
}

d_runs <- read_csv("data/d_runs.csv")

d_complete_runs <- list.files("data/clause-instances/") %>% 
  as_tibble() %>% 
  rename(clause_file = value) %>% 
  mutate(run_id = str_remove_all(clause_file, "d_clauses_|.rds")) %>% 
  left_join(d_runs, by = "run_id") %>% 
  filter(!is.na(params.n_qubits)) %>% 
  mutate(clause_file = file.path("data", "clause-instances", clause_file)) %>% 
  filter(params.n_qubits < 9) %>% 
  sample_n(1000) %>% 
  mutate(
    d_clauses = future_map(clause_file, read_rds),
    d_coeffs = future_map2(params.n_qubits, d_clauses, compute_pauli_coefficients),
    sat_assgn = future_map2_chr(d_clauses, params.n_qubits, solve_three_sat),
    d_clauses_str = map_chr(d_clauses, stringi::stri_flatten)
    ) %>% 
  distinct(params.n_qubits, d_clauses_str, .keep_all = T) %>% 
  mutate(
    qaoa_json = future_pmap_chr(., function(...){
      params = list(...)
      generate_instance_json(params$params.n_qubits, params$d_coeffs, params$run_id, params$sat_assgn)
    }))


# Write the files out -----------------------------------------------------

mapply(
  .write_qaoa_instance, 
  d_complete_runs$run_id,
  d_complete_runs$qaoa_json
  )
