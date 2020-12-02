###############################################################################
# Analysis Init
#
# Author: Vivek Katial
# Created 2020-11-19 12:56:50
###############################################################################

library(tidyverse)

# Global Vars
QAOA_DATA_PATH = "data/d_runs_qaoa.csv"
AQC_DATA_PATH = "data/d_runs_aqc.csv"

d_qaoa_runs <- read_csv(QAOA_DATA_PATH) %>% 
  filter(!is.na(params.instance)) %>% 
  mutate(aqc_run_id = str_remove(params.instance, "qaoa_instance_"))

d_runs_aqc <- read_csv(AQC_DATA_PATH)


d_overall <- d_qaoa_runs %>% 
  inner_join(d_runs_aqc, by = c("aqc_run_id" = "run_id"), suffix = c(".qaoa", ".aqc")) %>% 
  mutate(n_rounds = map_int(params.alpha_init, nchar)/3) %>% 
  select(
    params.budget, 
    params.instance, 
    metrics.energy, 
    metrics.p_success.qaoa, 
    metrics.min_energy_gap,
    metrics.clause_var_ratio,
    n_qubits = params.n_qubits.aqc,
    n_rounds
    )


# P Success ---------------------------------------------------------------

p_1 <- d_overall %>% 
  gather(type, prob, metrics.p_success.qaoa) %>% 
  mutate(
    n_rounds = paste0("n_rounds=", n_rounds),
    n_qubits = paste0("n_qubits=", n_qubits)
    ) %>% 
  ggplot(aes(x = params.budget, y = prob, col = as.character(n_rounds))) + 
  geom_point() + 
  geom_hline(yintercept = 1/64) + 
  theme_classic() +
  labs(x = "Budget", y = "Probability") + 
  facet_wrap(~n_rounds, nrow = 1)

p_1

p_1 + 
  facet_wrap(~n_qubits+n_rounds)


p_2 <- d_overall %>% 
  distinct(params.instance, .keep_all = T) %>% 
  gather(type, prob, metrics.p_success.qaoa) %>% 
  ggplot(aes(x = metrics.clause_var_ratio, y = prob)) + 
  geom_point() + 
  theme_minimal() +
  labs(x = "metrics.clause_var_ratio", y = "Probability")

p_2

d_overall %>% 
  group_by(params.instance) %>% 
  summarise_if(is.numeric, mean, na.rm=T)

