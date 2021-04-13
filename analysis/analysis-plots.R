###############################################################################
# Read and create some plots
#
# Author: Vivek Katial
# Created 2020-12-21 14:23:02
###############################################################################


# Import libraries and read in stuff --------------------------------------

LAST_RUN_TIME <- as.Date("2020-12-10")

library(tidyverse)

d_comparable <- read_rds("data/d_comparable.rds") %>%
  filter(start_time.qaoa > LAST_RUN_TIME)

# P Success ---------------------------------------------------------------

d_comparable %>%
  gather(type, prob, metrics_p_success.qaoa) %>%
  filter(type == "metrics_p_success.qaoa") %>%
  mutate(
    n_qubits = paste0("n_qubits=", params_n_qubits.qaoa),
    n_rounds = as_factor(n_rounds)
  ) %>%
  filter(params_budget == max(params_budget)) %>%
  ggplot(aes(x = n_rounds, y = prob, fill = classical_optimiser)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "n rounds",
       y = "p success",
       title = "Distribution of results when budget=20,000 iterations")

d_comparable %>%
  gather(type, prob, metrics_p_success.qaoa) %>%
  filter(type == "metrics_p_success.qaoa") %>%
  mutate(
    n_qubits = paste0("n_qubits=", params_n_qubits.qaoa),
    n_rounds = as_factor(n_rounds)
  ) %>%
  filter(params_budget == max(params_budget)) %>%
  ggplot(aes(x = n_rounds, y = prob, fill = classical_optimiser)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "n rounds",
       y = "p success",
       title = "Distribution of results when budget=20,000 iterations by Instance type") +
  facet_wrap(~ params_instance_type)


d_comparable %>%
  filter(classical_optimiser == "cma-es") %>%
  mutate(
    n_qubits = paste0("n_qubits=", params_n_qubits.qaoa),
    n_rounds = as_factor(n_rounds)
  ) %>%
  count(classical_optimiser, n_rounds, metrics_classical_iter) %>%
  ggplot(aes(x = n_rounds, y = metrics_classical_iter)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "n rounds",
       y = "number of function evaluation (CMA-ES)")


d_diffs <- d_comparable %>%
  filter(params_budget == max(params_budget)) %>%
  select(
    classical_optimiser,
    metrics_p_success.qaoa,
    params_instance,
    params_params_file,
    n_rounds,
    metrics_energy
  ) %>%
  pivot_wider(
    id_cols = c(params_instance, n_rounds),
    names_from = classical_optimiser,
    values_from = c(metrics_p_success.qaoa, metrics_energy)
  ) %>%
  janitor::clean_names() %>%
  mutate(
    diff_p_success = metrics_p_success_qaoa_cma_es - metrics_p_success_qaoa_nelder_mead,
    diff_energy = metrics_energy_cma_es - metrics_energy_nelder_mead
  )

d_diffs %>% 
  ggplot(aes(x = diff_p_success)) + 
  geom_histogram(bins = 30, fill = "pink", col = "black", alpha=0.5) + 
  theme_minimal() +
  labs(
    x = "P(CMA-ES) - P(Nelder Mead)"
  )


d_diffs %>% 
  ggplot(aes(x = diff_energy)) + 
  geom_histogram(bins = 30, fill = "pink", col = "black", alpha=0.5) + 
  theme_minimal() + 
  geom_vline(xintercept = 0, linetype="dashed") +
  labs(
    x = "Energy(CMA-ES) - Energy(Nelder Mead)"
  )

d_comparable %>% 
  mutate(
    p_random_guess = 1/2^params_n_qubits.qaoa,
    rate = metrics_p_success.qaoa/p_random_guess,
    n_rounds = as_factor(n_rounds)
    ) %>% 
  ggplot(aes(x = rate)) + 
  geom_histogram(bins = 100, fill = "pink", col = "black", alpha=0.5) +
  theme_minimal()

d_comparable %>% 
  mutate(
    p_random_guess = 1/2^params_n_qubits.qaoa,
    rate = metrics_p_success.qaoa/p_random_guess,
    n_rounds = as_factor(n_rounds)
  ) %>% 
  filter(rate < 1) %>% 
  ggplot(aes(x = rate)) + 
  geom_histogram(bins = 50, fill = "pink", col = "black", alpha=0.5)+
  theme_minimal()

d_comparable %>% 
  ggplot(aes(x = metrics_energy, y = metrics_p_success.qaoa)) + 
  geom_point(alpha = 0.2) + 
  theme_minimal() + 
  labs(
    x = "Energy",
    y = "P(Success)"
  )
  

d_comparable %>% 
  ggplot(aes(x = metrics_energy, y = metrics_p_success.qaoa)) + 
  geom_point(alpha = 0.2) + 
  theme_minimal() + 
  labs(
    x = "Energy",
    y = "P(Success)"
  ) + 
  facet_wrap(~classical_optimiser)


d_comparable %>% 
  ggplot(aes(x = metrics_energy, y = metrics_p_success.qaoa)) + 
  geom_point(alpha = 0.2) + 
  theme_minimal() + 
  labs(
    x = "Energy",
    y = "P(Success)"
  ) + 
  facet_wrap(~classical_optimiser+params_instance_type)


## How do we normalize
#' 1. Initial energy
#' 2. Final energy -- how much have we improved?
#' 3. Energy/global possible minimum 
#' 4.