###############################################################################
# Matilda Prep script
#
# Author: Vivek Katial
# Created 2020-07-16 11:30:57
###############################################################################

library(tidyverse)

d_data <- read_rds("data/d_enriched.rds")
d_matilda <- d_data %>% 
  # Construct instance
  mutate(
    Instances = paste("Instance", params_instance_index, params_instance_type, params_n_sat, params_n_qubits, sep = "_")
  ) %>% 
  # Select only feature columns and performance
  select(
    Instances,
    starts_with("f_"), 
    -f_vcg_graph, 
    -f_vg_graph,
    alg_param_time_t = params_time_t,
    alg_param_tstep = params_t_step,
    Source = params_instance_type,
    metrics_p_success
    ) %>% 
  mutate(
    Source = str_replace_all(Source, pattern = "/", "_"),
    alg_params = sprintf("algo_time_T_%s_tstep_%s", alg_param_time_t, alg_param_tstep),
    alg_params = str_replace(alg_params, "0\\.", "")
    ) %>% 
  pivot_wider(
    id_cols = c(starts_with("f_"), "Source", "Instances"), 
    names_from = alg_params, 
    values_from = metrics_p_success,
    values_fn = list(metrics_p_success = mean)
    ) %>% 
  rename_at(vars(starts_with("f_")), list(~str_replace(., "f_", "feature_"))) %>% 
  # Remove all missing values
  filter(
      !is.na(algo_time_T_100_tstep_1),
      !is.na(algo_time_T_400_tstep_1)
  ) %>% 
  select(-starts_with("algo"), algo_time_T_200_tstep_1, algo_time_T_100_tstep_1) %>% 
  select(Instances, Source, everything())

# Test no missing values
testthat::expect_equal(
  d_matilda %>% 
    summarise_all(list(~sum(is.na(.)))) %>% 
    gather(var, n) %>% 
    pull(n) %>% 
    sum(),
  0
)

# Tests unique names 
testthat::expect_equal(
  d_matilda %>% 
    count(Instances) %>% 
    pull(n) %>% 
    sum(),
  d_matilda %>% 
    nrow(),
  info = "Checking Unique Instances"
)

# Test that atleast 2 sources
testthat::expect_gt(
  d_matilda %>% 
    count(Source) %>% 
    nrow(),
  1
)

d_matilda %>% 
  select(algo_time_T_200_tstep_1, algo_time_T_100_tstep_1) %>% 
  gather(alg, prob) %>% 
  ggplot(aes(x = prob)) + 
  geom_histogram(color = "white") + 
  facet_wrap(~alg) + 
  theme_minimal()

d_res<- d_matilda %>% 
  filter(
    algo_time_T_200_tstep_1 < 0.5, 
    algo_time_T_100_tstep_1 < 0.5
    ) %>% 
  select(
    feature_p_size_n_clauses, 
    feature_p_size_lin_ratio, 
    feature_vg_median,
    algo_time_T_100_tstep_1,
    algo_time_T_200_tstep_1
    ) %>% 
  gather(algo, prob, -starts_with("feature_"))

d_res %>% 
  ggplot(aes(x = feature_p_size_n_clauses, y = prob, col = algo)) + 
  geom_point() + 
  facet_wrap(~algo)


d_matilda %>% 
  summary()

# Define Columns which need to be scaled 
scale_cols <- c("feature_p_size_n_variables", "feature_vcg_mean_cls", "feature_vcg_min_cls",
                "feature_vcg_max_cls")

# d_matilda %>% 
#   filter(feature_p_size_n_variables == 8) %>% 
#   filter(algo_time_T_100_tstep_1 > 0.15, algo_time_T_500_tstep_1 > 0.15) %>% 
#   count()

# Write to `d_matilda.csv`
d_matilda %>% 
  mutate_if(is.numeric, list(~ ./feature_p_size_n_variables)) %>% 
  filter(algo_time_T_100_tstep_1 > 0.15, algo_time_T_200_tstep_1 > 0.15) %>% 
  select(-scale_cols) %>% 
  write_csv("data/d_matilda.csv")
