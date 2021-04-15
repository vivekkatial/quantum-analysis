###############################################################################
# Analysis Init
#
# Author: Vivek Katial
# Created 2020-11-19 12:56:50
###############################################################################

library(tidyverse)

# Global Vars
QAOA_DATA_PATH = "data/d_three-sat-usa-qaoa.csv"
AQC_DATA_PATH = "data/d_three-sat-usa-inc-gs_enriched.rds"

d_qaoa_runs <- read_csv(QAOA_DATA_PATH) %>%
  filter(!is.na(params.instance)) %>%
  mutate(aqc_run_id = str_remove(params.instance, "qaoa_instance_")) %>%
  janitor::clean_names()


d_aqc_runs <- read_rds(AQC_DATA_PATH)


d_overall <- d_qaoa_runs %>%
  inner_join(
    d_aqc_runs,
    by = c("aqc_run_id" = "run_id"),
    suffix = c(".qaoa", ".aqc")
  ) %>%
  mutate(
    n_rounds = map_int(params_alpha_init, nchar) / 3,
    classical_optimiser = str_extract(params_params_file, "cma-es|nelder-mead")
  ) %>%
  distinct()

# Write Combined data to data folder
d_overall %>%
  write_rds("data/d_combined.rds")
