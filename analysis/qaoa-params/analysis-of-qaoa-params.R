###############################################################################
# Analysis for QAOA params
#
# Author: Vivek Katial
# Created 2023-02-14 15:45:16
###############################################################################

library(tidyverse)
library(here)
library(glue)

Sys.time()

RUN_DATETIME <- as.POSIXct("2023-02-14 14:55:00 AEDT", tz = "UTC")

d_runs_fw <- read_csv("data/d_QAOA-Parameter.csv") %>% 
  filter(start_time >= RUN_DATETIME)

metric_labels_new_run <- d_runs_fw %>% 
  names() %>% 
  as_tibble() %>% 
  rename(col_names = value) %>% 
  filter(
    str_detect(col_names, "beta|gamma"),
    str_detect(col_names, "qubit"),
    str_detect(col_names, "metrics")
  ) %>%
  pull(col_names)

d_results_clean_fixed_graph <- d_runs_fw %>% 
  select(
    metric_labels_new_run,
    run_id
  ) %>% 
  gather(
    col, val, -run_id
  ) %>% 
  # Extract Qubits
  mutate(
    qubits = str_extract(col, "\\d+") %>% as.numeric(),
    source = str_extract(col, "(?<=instanceType_)(.*)(?=_gamma|_beta)"),
    param_type = str_extract(col, "gamma|beta"),
    param_layer = str_extract(col, "\\d+(?![\\s\\S]*\\d+)") %>% as.factor()
  ) %>% 
  select(run_id, qubits, source, param_type, param_layer, val)



# Adding stuff for QAOA (older runs)
d_runs <- read_csv("data/d_QAOA-Parameter-instances.csv") %>% 
  filter(start_time >= RUN_DATETIME)


metric_labels <- d_runs %>% 
  names() %>% 
  as_tibble() %>% 
  rename(col_names = value) %>% 
  filter(
    str_detect(col_names, "beta|gamma"),
    str_detect(col_names, "qubit"),
    str_detect(col_names, "metrics")
    ) %>%
  pull(col_names)

d_clean_results <- d_runs %>% 
  select(
    metric_labels,
    run_id
  ) %>% 
  gather(
    col, val, -run_id
  ) %>% 
  # Extract Qubits
  mutate(
    qubits = str_extract(col, "\\d+") %>% as.numeric(),
    source = str_extract(col, "(?<=instanceType_)(.*)(?=_gamma|_beta)"),
    param_type = str_extract(col, "gamma|beta"),
    param_layer = str_extract(col, "\\d+(?![\\s\\S]*\\d+)") %>% as.factor()
  ) %>% 
  select(run_id, qubits, source, param_type, param_layer, val) %>% 
  bind_rows(d_results_clean_fixed_graph)


d_clean_results %>% 
  filter(
#    source == "four_regular_graph",
    param_type == "gamma",
    qubits == 10
    ) %>% 
  ggplot(aes(x = val))+ 
  # geom_histogram(data = . %>% filter(param_layer==0),alpha = 0.4, fill="green", bins=40) +
  # geom_histogram(data = . %>% filter(param_layer==1),alpha = 0.4, fill="red", bins=40) +
  # geom_histogram(data = . %>% filter(param_layer==2),alpha = 0.4, fill="blue", bins=40) +
  geom_density(aes(fill=param_layer, color=param_layer), alpha=0.4) + 
  facet_wrap(~source, ncol = 1) #+ 
  # xlim(c(-1, 1))


d_clean_results %>% 
  filter(
    source == "four_regular_graph_fixed_weights",
    param_type == "gamma",
    !is.na(val)
  ) %>% 
  count(qubits, param_type) %>% 
  mutate(n = n/3)

d_clean_results %>% 
  filter(
    source == "four_regular_graph_fixed_weights",
    param_type == "gamma",
    !is.na(val)
  ) %>% 
  ggplot(aes(x = val)) +
  # geom_histogram(data = . %>% filter(param_layer==0),alpha = 0.4, fill="green", bins=40) +
  # geom_histogram(data = . %>% filter(param_layer==1),alpha = 0.4, fill="red", bins=40) +
  # geom_histogram(data = . %>% filter(param_layer==2),alpha = 0.4, fill="blue", bins=40) +
  geom_density(aes(fill=param_layer, color=param_layer), alpha=0.4) + 
  facet_wrap(~qubits, ncol = 1) #+ 
#  xlim(c(-2*pi, 2*pi))


# Fixed Weight Graphs -----------------------------------------------------


